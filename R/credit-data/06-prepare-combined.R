required_packages <- c("cli", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) stop("Install missing packages: ", paste(missing_packages, collapse = ", "))

source("R/credit-data/00-helpers.R", local = TRUE)

# 1. Ejecutar preparaciones ------------------------------------------------
cli::cli_h1("Preparaciones")

# Cada etapa se ejecuta en su propio entorno para que sus objetos temporales no
# se mezclen. Los archivos RDS son la interfaz explícita entre etapas.
preparation_scripts <- sprintf(
  "R/credit-data/%02d-prepare-%s.R",
  1:5,
  c("data", "shap", "effects", "importance", "evaluation")
)

for (script in preparation_scripts) {
  cli::cli_alert_info("Running {.path {script}}.")
  source(script, local = new.env(parent = globalenv()))
}

# 2. Leer y validar --------------------------------------------------------
cli::cli_h1("Lectura y validación")

artifact_paths <- c(
  models = "R/credit-data/credit-models.rds",
  shap = "shap-explorer/shap-credit.rds",
  effects = "variable-effects/credit-effects.rds",
  importance = "global-feature-importance/credit-importance.rds",
  evaluation = "model-evaluation/credit-evaluation.rds"
)

missing_artifacts <- artifact_paths[!file.exists(artifact_paths)]
if (length(missing_artifacts)) stop("Missing artifacts: ", paste(missing_artifacts, collapse = ", "))

artifacts <- lapply(artifact_paths, readRDS)
credit_models <- artifacts$models
shap_artifact <- artifacts$shap
effects_artifact <- artifacts$effects
importance_artifact <- artifacts$importance
evaluation_artifact <- artifacts$evaluation

# Estas igualdades permiten eliminar copias repetidas con seguridad. Si una
# etapa utilizó otro split o modelos distintos, el combinado no debe guardarse.
if (!identical(credit_models$test, shap_artifact$test)) stop("SHAP uses a different test sample.")
if (!identical(credit_models$test, effects_artifact$test)) stop("Effects use a different test sample.")
if (!identical(credit_models$predictors, shap_artifact$predictors)) stop("SHAP uses different predictors.")
if (!identical(credit_models$predictors, effects_artifact$predictors)) stop("Effects use different predictors.")
if (!identical(credit_models$predictors, importance_artifact$predictors)) stop("Importance uses different predictors.")
if (!identical(credit_models$models, shap_artifact$models)) stop("SHAP contains different models.")
if (!identical(credit_models$predictions, shap_artifact$predictions)) stop("SHAP contains different predictions.")
if (!identical(credit_models$predictions, evaluation_artifact$predictions)) stop("Evaluation contains different predictions.")

# 3. Consolidar ------------------------------------------------------------
cli::cli_h1("Consolidación")

# status_bad se conserva en test y no se repite cuatro veces en predictions.
# Las grillas ICE y los cortes ALE se recuperan desde x, lower y upper.
combined_artifact <- list(
  train = credit_models$train,
  test = credit_models$test,
  predictors = credit_models$predictors,
  models = credit_models$models,
  predictions = credit_models$predictions |>
    dplyr::select(-status_bad),
  baseline = credit_models$baseline,
  explanations = list(
    shap_values = shap_artifact$shap_values,
    ice_values = effects_artifact$ice_values,
    ale_values = effects_artifact$ale_values
  ),
  importance_values = importance_artifact$importance_values,
  evaluation = list(
    threshold_curve = evaluation_artifact$threshold_curve,
    gains_curve = evaluation_artifact$gains_curve |>
      dplyr::select(-score),
    summary = evaluation_artifact$evaluation_summary
  ),
  metadata = c(
    credit_models$metadata,
    list(
      shap = shap_artifact$metadata$shap,
      effects = effects_artifact$metadata$effects,
      importance = importance_artifact$metadata$importance,
      evaluation = evaluation_artifact$metadata$evaluation,
      combined = list(
        version = 1L,
        prepared_at = Sys.time(),
        sources = as.list(artifact_paths),
        predictions_target = "status_bad in test; join by row_id",
        gains_score = "join predictions by model and row_id"
      )
    )
  )
)

# 4. Artefacto ------------------------------------------------------------
cli::cli_h1("Artefacto combinado")

combined_path <- "R/credit-data/credit-analysis.rds"
save_credit_artifact(combined_artifact, combined_path)

cli::cli_inform(c(
  "Combined artifact: {.path {combined_path}}",
  "i" = "Size: {round(file.info(combined_path)$size / 1024^2, 2)} MB."
))

all_artifact_paths <- c(artifact_paths, combined = combined_path)
artifact_sizes <- data.frame(
  file = unname(all_artifact_paths),
  size_mb = file.info(all_artifact_paths)$size / 1024^2,
  row.names = NULL
)

print(artifact_sizes)
