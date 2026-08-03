required_packages <- c("cli", "dplyr", "purrr", "tibble", "xgboost")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "))
}

source("R/credit-data/00-helpers.R", local = TRUE)

credit_models <- readRDS("R/credit-data/credit-models.rds")
grid_size <- 25L
ale_bins <- 10L

ice_values <- calculate_ice_values(
  models = credit_models$models,
  test = credit_models$test,
  predictors = credit_models$predictors,
  grid_size = grid_size
)

ale_values <- calculate_ale_values(
  models = credit_models$models,
  test = credit_models$test,
  predictors = credit_models$predictors,
  bins = ale_bins
)

# PDP is not duplicated: the app obtains it by averaging ICE over row_id.
# The observed distribution is read directly from test.
effects_artifact <- list(
  test = credit_models$test,
  predictors = credit_models$predictors,
  ice_values = ice_values,
  ale_values = ale_values,
  metadata = c(
    credit_models$metadata,
    list(
      effects = list(
        sample = "test",
        grid_size = grid_size,
        ale_bins = ale_bins,
        pdp_source = "mean ICE by model, variable and x",
        distribution_source = "test"
      )
    )
  )
)

save_credit_artifact(
  effects_artifact,
  "variable-effects/credit-effects.rds"
)
