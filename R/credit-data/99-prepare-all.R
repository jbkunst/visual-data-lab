preparation_scripts <- c(
  "R/credit-data/01-prepare-data.R",
  "R/credit-data/02-prepare-shap.R",
  "R/credit-data/03-prepare-effects.R",
  "R/credit-data/04-prepare-importance.R",
  "R/credit-data/05-prepare-evaluation.R",
  "R/credit-data/06-export-artifacts.R"
)

for (script in preparation_scripts) {
  message("\n--- Running ", script, " ---")
  source(script, local = new.env(parent = globalenv()))
}
