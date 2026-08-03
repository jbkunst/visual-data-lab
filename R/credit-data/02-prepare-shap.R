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

# Test is both the explained population and the SHAP background.
shap_values <- calculate_shap_values(
  models = credit_models$models,
  explanation_data = test_predictors,
  background = test_predictors,
  seed = shap_seed
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
  control_meta = credit_models$control_meta,
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
