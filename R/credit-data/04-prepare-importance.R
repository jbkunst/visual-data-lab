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
permutation_iterations <- 30L
sage_iterations <- 50L

credit_models <- readRDS("R/credit-data/credit-models.rds")
shap_artifact <- readRDS("shap-explorer/shap-credit.rds")

models <- credit_models$models
train <- credit_models$train
test <- credit_models$test
predictors <- credit_models$predictors
test_predictors <- test |>
  dplyr::select(dplyr::all_of(predictors))

# 1. Permutation importance ----------------------------------------------
permutation_importance <- purrr::imap_dfr(
  models,
  function(model, model_name) {
    set.seed(importance_seed)

    raw_importance <- celavi::variable_importance(
      object = model,
      data = test |>
        dplyr::select(status_bad, dplyr::all_of(predictors)),
      variables = predictors,
      response = "status_bad",
      loss_function = log_loss,
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
        metric = "log_loss",
        method = "permutation",
        variable,
        iteration,
        importance = value - loss_before,
        loss_before,
        loss_after = value
      )
  }
)

# 2. Drop-column importance ----------------------------------------------
full_losses <- purrr::imap_dfr(models, function(model, model_name) {
  tibble::tibble(
    model = model_name,
    loss_before = log_loss(
      test$status_bad,
      predict_model(model, test_predictors)
    )
  )
})

drop_progress <- cli::cli_progress_bar(
  name = "Drop-column",
  total = length(models) * length(predictors)
)

drop_column_importance <- purrr::imap_dfr(
  models,
  function(model, model_name) {
    purrr::map_dfr(predictors, function(variable) {
      cli::cli_progress_update(id = drop_progress)

      reduced_predictors <- setdiff(predictors, variable)
      reduced_model <- fit_credit_model(
        model_name,
        train = train,
        predictors = reduced_predictors,
        seed = importance_seed
      )
      loss_before <- full_losses$loss_before[full_losses$model == model_name]
      loss_after <- log_loss(
        test$status_bad,
        predict_model(
          reduced_model,
          test[, reduced_predictors, drop = FALSE]
        )
      )

      tibble::tibble(
        model = model_name,
        sample = "test",
        metric = "log_loss",
        method = "drop_column",
        variable = variable,
        iteration = NA_integer_,
        importance = loss_after - loss_before,
        loss_before,
        loss_after
      )
    })
  }
)

cli::cli_progress_done(id = drop_progress)

# 3. SAGE ----------------------------------------------------------------
sage_progress <- cli::cli_progress_bar(
  name = "SAGE",
  total = length(models) * sage_iterations
)

sage_importance <- purrr::imap_dfr(
  models,
  function(model, model_name) {
    purrr::map_dfr(seq_len(sage_iterations), function(iteration) {
      cli::cli_progress_update(id = sage_progress)
      set.seed(importance_seed + iteration)

      variable_order <- sample(predictors)
      reference_rows <- sample.int(nrow(test_predictors))
      current_data <- test_predictors[reference_rows, , drop = FALSE]
      loss_before <- log_loss(
        test$status_bad,
        predict_model(model, current_data)
      )
      iteration_values <- vector("list", length(variable_order))

      for (position in seq_along(variable_order)) {
        variable <- variable_order[[position]]
        current_data[[variable]] <- test_predictors[[variable]]
        loss_after <- log_loss(
          test$status_bad,
          predict_model(model, current_data)
        )

        iteration_values[[position]] <- tibble::tibble(
          model = model_name,
          sample = "test",
          metric = "log_loss",
          method = "sage",
          variable = variable,
          iteration = iteration,
          importance = loss_before - loss_after,
          loss_before,
          loss_after
        )

        loss_before <- loss_after
      }

      dplyr::bind_rows(iteration_values)
    })
  }
)

cli::cli_progress_done(id = sage_progress)

# 4. Global SHAP ----------------------------------------------------------
shap_global_importance <- shap_artifact$shap_values |>
  dplyr::summarise(
    importance = mean(abs(shap)),
    .by = c(model, variable)
  ) |>
  dplyr::mutate(
    sample = "test",
    metric = "prediction_change",
    method = "shap_global",
    iteration = NA_integer_,
    loss_before = NA_real_,
    loss_after = NA_real_
  ) |>
  dplyr::select(
    model, sample, metric, method, variable, iteration,
    importance, loss_before, loss_after
  )

importance_values <- dplyr::bind_rows(
  permutation_importance,
  drop_column_importance,
  sage_importance,
  shap_global_importance
)

importance_artifact <- list(
  predictors = predictors,
  importance_values = importance_values,
  metadata = c(
    credit_models$metadata,
    list(
      importance = list(
        sample = "test",
        permutation_package = "celavi",
        permutation_iterations = permutation_iterations,
        sage_implementation = "marginal Monte Carlo path approximation",
        sage_iterations = sage_iterations,
        seed = importance_seed,
        shap_global = "mean(abs(shap))"
      )
    )
  )
)

save_credit_artifact(
  importance_artifact,
  "global-feature-importance/credit-importance.rds"
)
