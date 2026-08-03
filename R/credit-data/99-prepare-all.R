preparation_scripts <- c(
  "R/credit-data/01-prepare-data.R",
  "R/credit-data/02-prepare-shap.R",
  "R/credit-data/03-prepare-effects.R",
  "R/credit-data/04-prepare-importance.R",
  "R/credit-data/05-prepare-evaluation.R"
)

for (script in preparation_scripts) {
  message("\n--- Running ", script, " ---")
  source(script, local = new.env(parent = globalenv()))
}

artifact_paths <- c(
  "R/credit-data/credit-models.rds",
  "shap-explorer/shap-credit.rds",
  "variable-effects/credit-effects.rds",
  "global-feature-importance/credit-importance.rds",
  "model-evaluation/credit-evaluation.rds"
)

artifact_sizes <- tibble::tibble(
  file = artifact_paths,
  size_mb = file.info(artifact_paths)$size / 1024^2
)

print(artifact_sizes)
