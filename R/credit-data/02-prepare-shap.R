required_packages <- c("cli", "dplyr", "purrr", "tibble", "xgboost")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "))
}

source("R/credit-data/00-helpers.R", local = TRUE)

shap_seed <- 2026L
credit_models <- readRDS("R/credit-data/credit-models.rds")

test_predictors <- credit_models$test |>
  dplyr::select(dplyr::all_of(credit_models$predictors))

control_meta <- credit_models$predictors |>
  stats::setNames(credit_models$predictors) |>
  purrr::map(function(variable) {
    values <- credit_models$train[[variable]]

    list(
      min = min(values),
      max = max(values),
      value = unname(stats::median(values))
    )
  })

# Test is both the explained population and the SHAP background.
shap_values <- purrr::imap_dfr(
  credit_models$models,
  function(model, model_name) {
    progress_id <- cli::cli_progress_bar(
      name = paste("SHAP:", model$label),
      total = nrow(test_predictors),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_percent}",
        "| {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
      )
    )

    values <- purrr::map_dfr(seq_len(nrow(test_predictors)), function(row_id) {
      cli::cli_progress_update(id = progress_id)

      shap <- local_shap_trace_optimized(
        model,
        x = test_predictors[row_id, , drop = FALSE],
        background = test_predictors,
        seed = shap_seed
      ) |>
        dplyr::summarise(
          shap = mean(diff),
          .by = variable
        )

      tibble::tibble(
        model = model_name,
        row_id = row_id,
        variable = as.character(shap$variable),
        shap = shap$shap
      )
    })

    cli::cli_progress_done(id = progress_id)
    values
  }
)

expected_rows <- length(credit_models$models) *
  nrow(credit_models$test) *
  length(credit_models$predictors)

if (nrow(shap_values) != expected_rows || anyNA(shap_values$shap)) {
  stop("The SHAP table is incomplete.")
}

shap_artifact <- list(
  test = credit_models$test,
  predictors = credit_models$predictors,
  models = credit_models$models,
  predictions = credit_models$predictions,
  control_meta = control_meta,
  baseline = credit_models$baseline,
  shap_values = shap_values,
  metadata = c(
    credit_models$metadata,
    list(
      shap = list(
        background = "complete test set",
        explained_sample = "complete test set",
        seed = shap_seed
      )
    )
  )
)

save_credit_artifact(
  shap_artifact,
  "shap-explorer/shap-credit.rds"
)
