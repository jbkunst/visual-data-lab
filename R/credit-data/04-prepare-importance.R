required_packages <- c(
  "celavi", "cli", "dplyr", "purrr", "randomForest",
  "rpart", "tibble", "xgboost"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install missing packages: ",
    paste(missing_packages, collapse = ", "),
    if ("celavi" %in% missing_packages) {
      "\nInstall celavi with pak::pkg_install(\"jbkunst/celavi\")."
    } else {
      ""
    }
  )
}

source("R/credit-data/00-helpers.R", local = TRUE)

importance_seed <- 2026L
permutation_iterations <- 100L
sage_iterations <- 50L

# 1. Preparación ----------------------------------------------------------
cli::cli_h1("Preparación")

credit_models <- readRDS("R/credit-data/credit-models.rds")

models <- credit_models$models
train <- credit_models$train
test <- credit_models$test
predictors <- credit_models$predictors
test_predictors <- test |>
  dplyr::select(dplyr::all_of(predictors))

# 2. Permutation importance ----------------------------------------------
cli::cli_h1("Permutation importance")

# Ambas métricas se expresan como pérdidas, por lo que una importancia positiva
# siempre significa que permutar la variable empeoró el modelo.
permutation_losses <- list(
  log_loss = log_loss,
  `1_minus_auc_roc` = one_minus_auc,
  `1_minus_ks` = one_minus_ks
)

permutation_importance <- purrr::imap_dfr(
  permutation_losses,
  function(loss_function, metric_name) {
    cli::cli_h2(metric_name)

    purrr::imap_dfr(models, function(model_object, model_name) {
      # Reiniciar la semilla permite comparar las métricas con las mismas
      # permutaciones para un modelo determinado.
      set.seed(importance_seed)

      if (identical(model_object$type, "xgboost") && is.raw(model_object$fit)) model_object$fit <- xgboost::xgb.load.raw(model_object$fit)

      raw_importance <- celavi::variable_importance(
        object = model_object,
        data = test |>
          dplyr::select(status_bad, dplyr::all_of(predictors)),
        variables = predictors,
        response = "status_bad",
        loss_function = loss_function,
        iterations = permutation_iterations,
        predict_function = function(object, newdata) {
          predict_model(
            object,
            newdata[, predictors, drop = FALSE]
          )
        },
        verbose = TRUE
      )

      full_loss <- raw_importance |>
        dplyr::filter(variable == "_full_model_") |>
        dplyr::select(iteration, loss_before = value)

      raw_importance |>
        dplyr::filter(variable %in% predictors) |>
        dplyr::left_join(full_loss, by = "iteration") |>
        dplyr::transmute(
          model = model_name,
          sample = "test",
          metric = metric_name,
          method = "permutation",
          variable,
          iteration,
          importance = value - loss_before,
          loss_before,
          loss_after = value
        )
    })
  }
)

# 3. SAGE ----------------------------------------------------------------
cli::cli_h1("SAGE")

sage_losses <- list(
  log_loss = log_loss,
  `1_minus_auc_roc` = one_minus_auc,
  `1_minus_ks` = one_minus_ks
)

sage_importance <- purrr::imap_dfr(
  models,
  function(model_object, model_name) {
    cli::cli_inform("SAGE: {.val {model_object$label}}")

    if (identical(model_object$type, "xgboost") && is.raw(model_object$fit)) model_object$fit <- xgboost::xgb.load.raw(model_object$fit)

    purrr::map_dfr(seq_len(sage_iterations), function(iteration) {
      set.seed(importance_seed + iteration)

      variable_order <- sample(predictors)
      reference_rows <- sample.int(nrow(test_predictors))
      current_data <- test_predictors[reference_rows, , drop = FALSE]
      score_before <- predict_model(model_object, current_data)
      losses_before <- purrr::map_dbl(
        sage_losses,
        ~ .x(test$status_bad, score_before)
      )
      iteration_values <- vector("list", length(variable_order))

      for (position in seq_along(variable_order)) {
        variable <- variable_order[[position]]
        current_data[[variable]] <- test_predictors[[variable]]
        score_after <- predict_model(model_object, current_data)
        losses_after <- purrr::map_dbl(
          sage_losses,
          ~ .x(test$status_bad, score_after)
        )

        iteration_values[[position]] <- tibble::tibble(
          model = model_name,
          sample = "test",
          metric = names(sage_losses),
          method = "sage",
          variable = variable,
          iteration = iteration,
          position = position,
          importance = losses_before - losses_after,
          loss_before = losses_before,
          loss_after = losses_after
        )

        losses_before <- losses_after
      }

      result <- dplyr::bind_rows(iteration_values)
      result
    })
  }
)

# 4. Curvas de calidad ----------------------------------------------------
cli::cli_h1("Curvas de calidad")

diagnostic_predictions <- purrr::imap_dfr(models, function(model_object, model_name) {
  purrr::map_dfr(c("train", "test"), function(sample_name) {
    sample_data <- if (sample_name == "train") train else test
    scores <- predict_model(
      model_object,
      sample_data[, predictors, drop = FALSE]
    )

    tibble::tibble(
      model = model_name, sample = sample_name,
      status_bad = sample_data$status_bad, score = scores,
      individual_log_loss = individual_log_loss(sample_data$status_bad, scores)
    )
  })
})

threshold_curves <- diagnostic_predictions |>
  dplyr::group_by(model, sample) |>
  dplyr::group_modify(function(data, key) {
    n_positive <- sum(data$status_bad == 1L)
    n_negative <- sum(data$status_bad == 0L)

    curve <- data |>
      dplyr::summarise(
        positives = sum(status_bad == 1L),
        negatives = sum(status_bad == 0L),
        .by = score
      ) |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::mutate(
        true_positive_rate = cumsum(positives) / n_positive,
        false_positive_rate = cumsum(negatives) / n_negative,
        ks_gap = true_positive_rate - false_positive_rate
      ) |>
      dplyr::select(threshold = score, true_positive_rate, false_positive_rate, ks_gap)

    dplyr::bind_rows(
      tibble::tibble(
        threshold = Inf, true_positive_rate = 0,
        false_positive_rate = 0, ks_gap = 0
      ),
      curve
    )
  }) |>
  dplyr::ungroup()

gains_curves <- diagnostic_predictions |>
  dplyr::group_by(model, sample) |>
  dplyr::group_modify(function(data, key) {
    curve <- data |>
      dplyr::summarise(
        observations = dplyr::n(), positives = sum(status_bad),
        .by = score
      ) |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::mutate(
        population_fraction = cumsum(observations) / sum(observations),
        positive_fraction = cumsum(positives) / sum(positives)
      ) |>
      dplyr::select(score, population_fraction, positive_fraction)

    dplyr::bind_rows(
      tibble::tibble(
        score = Inf, population_fraction = 0, positive_fraction = 0
      ),
      curve
    )
  }) |>
  dplyr::ungroup()

quality_summary <- diagnostic_predictions |>
  dplyr::group_by(model, sample) |>
  dplyr::summarise(
    log_loss = log_loss(status_bad, score),
    auc = auc_roc(status_bad, score),
    ks = ks_statistic(status_bad, score),
    default_rate = mean(status_bad),
    .groups = "drop"
  )

importance_values <- dplyr::bind_rows(
  permutation_importance,
  sage_importance
)

# 5. Artefacto ------------------------------------------------------------
cli::cli_h1("Artefacto")

importance_artifact <- list(
  predictors = predictors,
  importance_values = importance_values,
  diagnostics = list(
    threshold_curves = threshold_curves,
    gains_curves = gains_curves,
    quality_summary = quality_summary,
    log_loss_values = diagnostic_predictions |>
      dplyr::select(model, sample, individual_log_loss)
  ),
  metadata = c(
    credit_models$metadata,
    list(
      importance = list(
        sample = "test",
        permutation_package = "celavi",
        permutation_metrics = names(permutation_losses),
        permutation_iterations = permutation_iterations,
        sage_implementation = "marginal Monte Carlo path approximation",
        sage_metrics = names(sage_losses),
        sage_iterations = sage_iterations,
        seed = importance_seed
      )
    )
  )
)

save_credit_artifact(
  importance_artifact,
  "global-feature-importance/credit-importance.rds"
)
