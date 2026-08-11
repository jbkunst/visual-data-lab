# rpart y randomForest son necesarios aunque aquí no se entrenen modelos: al
# leerlos desde RDS, cargar sus namespaces registra los métodos S3 de predict().
required_packages <- c(
  "cli", "dplyr", "purrr", "randomForest", "rpart", "tibble", "xgboost"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) stop("Install missing packages: ", paste(missing_packages, collapse = ", "))

source("R/credit-data/00-helpers.R", local = TRUE)
source("shap-explorer/local_shap.R", local = TRUE)

shap_seed <- 2026L

# 1. Preparación ----------------------------------------------------------
cli::cli_h1("Preparación")

# El paso 01 fija la muestra, los modelos y sus convenciones. SHAP debe explicar
# exactamente esos objetos para ser comparable con los demás artefactos.
credit_models <- readRDS("R/credit-data/credit-models.rds")

# Se excluyen row_id y status_bad porque la explicación solo recorre las entradas
# que cada modelo utilizó para producir su probabilidad.
test_predictors <- credit_models$test |>
  dplyr::select(dplyr::all_of(credit_models$predictors))

# 2. SHAP -----------------------------------------------------------------
cli::cli_h1("SHAP")

# Cada fila de test se explica usando la muestra completa de test como fondo.
# Esta es una aproximación SHAP marginal Monte Carlo: para cada fila de fondo se
# recorre una permutación. Las mismas permutaciones se reutilizan en todos los
# perfiles para que sus diferencias no dependan de distinto ruido aleatorio.
shap_values <- purrr::imap_dfr(
  credit_models$models,
  function(model_object, model_name) {
    cli::cli_inform("SHAP: {.val {model_object$label}}")

    # El booster se reconstruye una vez por modelo; hacerlo dentro de cada fila
    # repetiría innecesariamente una operación de deserialización costosa.
    if (identical(model_object$type, "xgboost") && is.raw(model_object$fit)) model_object$fit <- xgboost::xgb.load.raw(model_object$fit)

    purrr::map_dfr(seq_len(nrow(test_predictors)), function(row_id) {
      shap <- local_shap_trace_optimized(
        model_object,
        x = test_predictors[row_id, , drop = FALSE],
        background = test_predictors,
        seed = shap_seed
      ) |>
        dplyr::summarise(
          shap = mean(diff),
          .by = variable
        )

      result <- tibble::tibble(
        model = model_name,
        row_id = row_id,
        variable = as.character(shap$variable),
        shap = shap$shap
      )

      result
    })
  }
)

# 3. Validación -----------------------------------------------------------
cli::cli_h1("Validación")

# Debe existir exactamente una contribución por modelo, observación y predictor.
expected_rows <- length(credit_models$models) *
  nrow(credit_models$test) *
  length(credit_models$predictors)

if (nrow(shap_values) != expected_rows || anyNA(shap_values$shap)) stop("The SHAP table is incomplete.")

# La suma de SHAP más el baseline debe reconstruir la probabilidad original.
# Esta identidad detecta errores de agregación aunque la tabla esté completa.
shap_check <- shap_values |>
  dplyr::summarise(shap_sum = sum(shap), .by = c(model, row_id)) |>
  dplyr::mutate(
    reconstructed_score = credit_models$baseline[model] + shap_sum
  ) |>
  dplyr::left_join(
    credit_models$predictions |>
      dplyr::select(model, row_id, score),
    by = c("model", "row_id")
  )

max_reconstruction_error <- max(abs(
  shap_check$reconstructed_score - shap_check$score
))

if (max_reconstruction_error > 1e-10) stop("SHAP values do not reconstruct the original predictions.")

# 4. Artefacto ------------------------------------------------------------
cli::cli_h1("Artefacto")

# Se guardan los modelos y predicciones para que la app pueda mostrar el perfil,
# la probabilidad y sus contribuciones sin recalcular explicaciones al vuelo.
shap_artifact <- list(
  test = credit_models$test,
  predictors = credit_models$predictors,
  models = credit_models$models,
  predictions = credit_models$predictions,
  baseline = credit_models$baseline,
  shap_values = shap_values,
  metadata = c(
    credit_models$metadata,
    list(
      shap = list(
        estimator = "marginal Monte Carlo SHAP",
        background = "complete test set",
        explained_sample = "complete test set",
        paths_per_explanation = nrow(test_predictors),
        common_random_permutations = TRUE,
        seed = shap_seed
      )
    )
  )
)

save_credit_artifact(
  shap_artifact,
  "shap-explorer/shap-credit.rds"
)
