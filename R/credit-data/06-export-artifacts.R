required_packages <- c("cli", "dplyr", "purrr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "))
}

source("R/credit-data/00-helpers.R", local = TRUE)

analysis_path <- "R/credit-data/credit-analysis.rds"
credit_analysis <- readRDS(analysis_path)

required_results <- c(
  "shap_values", "ice_values", "ale_values", "importance_values",
  "threshold_curve", "gains_curve", "evaluation_summary"
)
missing_results <- required_results[
  !vapply(required_results, function(name) !is.null(credit_analysis[[name]]), logical(1))
]

if (length(missing_results)) {
  stop(
    "Run the missing preparation stages before exporting: ",
    paste(missing_results, collapse = ", ")
  )
}

control_meta <- credit_analysis$predictors |>
  stats::setNames(credit_analysis$predictors) |>
  purrr::map(function(variable) {
    values <- credit_analysis$train[[variable]]

    list(
      min = min(values),
      max = max(values),
      value = unname(stats::median(values))
    )
  })

baseline <- credit_analysis$predictions |>
  dplyr::summarise(value = mean(score), .by = model) |>
  tibble::deframe()

# Each app gets only what it needs. The complete reusable source remains
# R/credit-data/credit-analysis.rds.
shap_artifact <- list(
  test = credit_analysis$test,
  predictors = credit_analysis$predictors,
  models = credit_analysis$models,
  predictions = credit_analysis$predictions,
  control_meta = control_meta,
  baseline = baseline,
  shap_values = credit_analysis$shap_values,
  metadata = credit_analysis$metadata
)

effects_artifact <- list(
  test = credit_analysis$test,
  predictors = credit_analysis$predictors,
  ice_values = credit_analysis$ice_values,
  ale_values = credit_analysis$ale_values,
  metadata = credit_analysis$metadata
)

importance_artifact <- list(
  predictors = credit_analysis$predictors,
  importance_values = credit_analysis$importance_values,
  metadata = credit_analysis$metadata
)

evaluation_artifact <- list(
  predictions = credit_analysis$predictions,
  threshold_curve = credit_analysis$threshold_curve,
  gains_curve = credit_analysis$gains_curve,
  evaluation_summary = credit_analysis$evaluation_summary,
  metadata = credit_analysis$metadata
)

artifact_paths <- c(
  "shap-explorer/shap-credit.rds",
  "variable-effects/credit-effects.rds",
  "global-feature-importance/credit-importance.rds",
  "model-evaluation/credit-evaluation.rds"
)

purrr::walk2(
  list(
    shap_artifact,
    effects_artifact,
    importance_artifact,
    evaluation_artifact
  ),
  artifact_paths,
  save_credit_artifact
)

artifact_sizes <- tibble::tibble(
  file = artifact_paths,
  size_mb = file.info(artifact_paths)$size / 1024^2
)

print(artifact_sizes)
cli::cli_success("Application artifacts are ready.")
