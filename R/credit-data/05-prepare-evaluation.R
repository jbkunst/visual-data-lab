required_packages <- c("cli", "dplyr", "purrr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "))
}

source("R/credit-data/00-helpers.R", local = TRUE)

credit_models <- readRDS("R/credit-data/credit-models.rds")
evaluation_results <- calculate_evaluation_results(
  credit_models$predictions
)

evaluation_artifact <- list(
  predictions = credit_models$predictions,
  threshold_curve = evaluation_results$threshold_curve,
  gains_curve = evaluation_results$gains_curve,
  evaluation_summary = evaluation_results$evaluation_summary,
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
