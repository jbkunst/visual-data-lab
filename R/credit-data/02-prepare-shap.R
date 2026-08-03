required_packages <- c("cli", "dplyr", "purrr", "tibble", "xgboost")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "))
}

source("R/credit-data/00-helpers.R", local = TRUE)

analysis_path <- "R/credit-data/credit-analysis.rds"
shap_seed <- 2026L
credit_analysis <- readRDS(analysis_path)

test_predictors <- credit_analysis$test |>
  dplyr::select(dplyr::all_of(credit_analysis$predictors))

# Test is both the explained population and the SHAP background.
shap_values <- calculate_shap_values(
  models = credit_analysis$models,
  explanation_data = test_predictors,
  background = test_predictors,
  seed = shap_seed
)

expected_rows <- length(credit_analysis$models) *
  nrow(credit_analysis$test) *
  length(credit_analysis$predictors)

if (nrow(shap_values) != expected_rows || anyNA(shap_values$shap)) {
  stop("The SHAP table is incomplete.")
}

credit_analysis$shap_values <- shap_values
credit_analysis$metadata$shap <- list(
  background = "complete test set",
  explained_sample = "complete test set",
  seed = shap_seed
)

saveRDS(credit_analysis, analysis_path, compress = "gzip")
cli::cli_success("Added SHAP results to {.path {analysis_path}}.")
