required_packages <- c("cli", "dplyr", "purrr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) stop("Install missing packages: ", paste(missing_packages, collapse = ", "))

source("R/credit-data/00-helpers.R", local = TRUE)

# 1. Preparación ----------------------------------------------------------
cli::cli_h1("Preparación")

credit_models <- readRDS("R/credit-data/credit-models.rds")
predictions <- credit_models$predictions

# 2. ROC y KS -------------------------------------------------------------
cli::cli_h1("ROC y KS")

# La misma tabla de umbrales alimenta las curvas ROC y la distancia KS.
threshold_curve <- predictions |>
  dplyr::group_by(model) |>
  dplyr::group_modify(function(data, key) {
    n_positive <- sum(data$status_bad == 1L)
    n_negative <- sum(data$status_bad == 0L)

    curve <- data |>
      dplyr::group_by(score) |>
      dplyr::summarise(
        positives = sum(status_bad == 1L),
        negatives = sum(status_bad == 0L),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::mutate(
        true_positive_rate = cumsum(positives) / n_positive,
        false_positive_rate = cumsum(negatives) / n_negative,
        ks_gap = true_positive_rate - false_positive_rate,
        threshold = score
      ) |>
      dplyr::select(
        threshold,
        true_positive_rate,
        false_positive_rate,
        ks_gap
      )

    dplyr::bind_rows(
      tibble::tibble(
        threshold = Inf,
        true_positive_rate = 0,
        false_positive_rate = 0,
        ks_gap = 0
      ),
      curve
    )
  }) |>
  dplyr::ungroup()

# 3. Ganancia y lift ------------------------------------------------------
cli::cli_h1("Ganancia y lift")

# La misma tabla ordenada alimenta ganancia acumulada, vistas tipo Lorenz y lift.
gains_curve <- predictions |>
  dplyr::group_by(model) |>
  dplyr::group_modify(function(data, key) {
    ordered <- data |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::mutate(
        population_fraction = dplyr::row_number() / dplyr::n(),
        positive_fraction = cumsum(status_bad) / sum(status_bad),
        lift = positive_fraction / population_fraction
      ) |>
      dplyr::select(
        row_id,
        score,
        population_fraction,
        positive_fraction,
        lift
      )

    dplyr::bind_rows(
      tibble::tibble(
        row_id = NA_integer_,
        score = Inf,
        population_fraction = 0,
        positive_fraction = 0,
        lift = NA_real_
      ),
      ordered
    )
  }) |>
  dplyr::ungroup()

# 4. Resumen --------------------------------------------------------------
cli::cli_h1("Resumen")

evaluation_summary <- purrr::map_dfr(
  unique(predictions$model),
  function(model_name) {
    model_predictions <- predictions |>
      dplyr::filter(model == model_name)
    model_thresholds <- threshold_curve |>
      dplyr::filter(model == model_name)
    ks_row <- model_thresholds |>
      dplyr::slice_max(ks_gap, n = 1, with_ties = FALSE)

    actual <- model_predictions$status_bad
    predicted <- model_predictions$score
    auc <- auc_roc(actual, predicted)

    tibble::tibble(
      model = model_name,
      sample = "test",
      log_loss = log_loss(actual, predicted),
      auc = auc,
      gini = 2 * auc - 1,
      ks = ks_row$ks_gap,
      ks_threshold = ks_row$threshold
    )
  }
)

# 5. Artefacto ------------------------------------------------------------
cli::cli_h1("Artefacto")

evaluation_artifact <- list(
  predictions = predictions,
  threshold_curve = threshold_curve,
  gains_curve = gains_curve,
  evaluation_summary = evaluation_summary,
  metadata = c(
    credit_models$metadata,
    list(
      evaluation = list(
        sample = "test",
        positive_class = 1L,
        score_direction = "higher score means higher probability of bad"
      )
    )
  )
)

save_credit_artifact(
  evaluation_artifact,
  "model-evaluation/credit-evaluation.rds"
)
