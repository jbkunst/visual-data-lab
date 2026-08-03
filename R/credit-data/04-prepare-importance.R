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

permutation_importance <- calculate_permutation_importance(
  models = credit_models$models,
  test = credit_models$test,
  predictors = credit_models$predictors,
  iterations = permutation_iterations,
  seed = importance_seed
)

drop_column_importance <- calculate_drop_column_importance(
  models = credit_models$models,
  train = credit_models$train,
  test = credit_models$test,
  predictors = credit_models$predictors,
  seed = importance_seed
)

sage_importance <- calculate_sage_importance(
  models = credit_models$models,
  test = credit_models$test,
  predictors = credit_models$predictors,
  iterations = sage_iterations,
  seed = importance_seed
)

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
  predictors = credit_models$predictors,
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
