required_packages <- c(
  "butcher", "cli", "dplyr", "modeldata", "purrr", "randomForest",
  "rpart", "rsample", "tibble", "tidyr", "xgboost"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install the missing packages before preparing the artifacts: ",
    paste(missing_packages, collapse = ", ")
  )
}

source("R/credit-data/00-helpers.R", local = TRUE)

# Las semillas separadas documentan qué parte controla la división de datos y
# qué parte controla los algoritmos estocásticos de entrenamiento.
split_seed <- 2026L
model_seed <- 2026L

# 1. Preparación ----------------------------------------------------------
cli::cli_h1("Preparación")

# Se conserva un conjunto pequeño de predictores numéricos compartido por todos
# los modelos. Así se comparan algoritmos sin cambiar la información de entrada.
predictors <- c(
  "seniority", "time", "age", "expenses", "income",
  "assets", "debt", "amount", "price"
)

# Se usan casos completos para que ningún modelo aplique una imputación distinta.
# status_bad convierte la clase de interés en 1 y split_stratum existe solamente
# para mantener su proporción al dividir train y test.
credit_data <- modeldata::credit_data |>
  tibble::as_tibble() |>
  dplyr::rename_with(tolower) |>
  tidyr::drop_na(status, dplyr::all_of(predictors)) |>
  dplyr::mutate(
    status_bad = as.integer(status == "bad"),
    split_stratum = factor(status_bad, levels = c(0, 1))
  ) |>
  dplyr::select(status_bad, split_stratum, dplyr::all_of(predictors))

set.seed(split_seed)

# La estratificación evita que una diferencia accidental en la tasa de malos
# pagadores distorsione la comparación posterior de los modelos.
credit_split <- rsample::initial_split(
  credit_data,
  prop = 0.75,
  strata = split_stratum
)

train <- rsample::training(credit_split) |>
  dplyr::select(-split_stratum)

test <- rsample::testing(credit_split) |>
  dplyr::select(-split_stratum) |>
  dplyr::mutate(row_id = dplyr::row_number(), .before = 1)

# El objeto enviado a predict() contiene solo variables explicativas;
# status_bad y row_id no fueron usados para entrenar.
test_predictors <- test |>
  dplyr::select(dplyr::all_of(predictors))

cli::cli_inform(c(
  "Prepared {nrow(credit_data)} complete observations.",
  "i" = "Train: {nrow(train)} observations.",
  "i" = "Test: {nrow(test)} observations."
))

# 2. Entrenamiento --------------------------------------------------------
cli::cli_h1("Entrenamiento")

# Todos los algoritmos reciben el mismo train, predictores y semilla.
# fit_credit_model() mantiene sus especificaciones en el archivo de helpers.
model_names <- c("logistic", "tree", "random_forest", "xgboost")

models <- model_names |>
  stats::setNames(model_names) |>
  purrr::map(
    fit_credit_model,
    train = train,
    predictors = predictors,
    seed = model_seed
  )

# 3. Predicciones ---------------------------------------------------------
cli::cli_h1("Predicciones")

# Estas probabilidades alimentan las apps y también sirven como referencia para
# comprobar que la reducción posterior no altera el comportamiento del modelo.
test_predictions <- purrr::map(
  models,
  predict_model,
  newdata = test_predictors
)

# Cada modelo debe producir una probabilidad numérica, completa y dentro de
# [0, 1] por observación. Fallar aquí evita guardar resultados parciales.
purrr::iwalk(test_predictions, function(prediction, model_name) {
  if (!is.numeric(prediction)) stop("Predictions are not numeric for model: ", model_name)

  if (length(prediction) != nrow(test)) stop("Unexpected prediction length for model: ", model_name)

  if (anyNA(prediction)) stop("Missing predictions for model: ", model_name)

  if (any(prediction < 0 | prediction > 1)) stop("Predictions outside [0, 1] for model: ", model_name)
})

# El formato largo simplifica filtros, agrupaciones y comparaciones en las apps.
predictions <- purrr::imap_dfr(
  test_predictions,
  function(score, model_name) {
    tibble::tibble(
      row_id = test$row_id,
      model = model_name,
      status_bad = test$status_bad,
      score = score
    )
  }
)

# 4. Reducción de modelos ------------------------------------------------
cli::cli_h1("Reducción de modelos")

# butcher elimina componentes de entrenamiento innecesarios para predecir. La
# versión reducida solo se acepta si ocupa menos espacio y reproduce exactamente
# las probabilidades de referencia dentro de una tolerancia estricta.
models <- purrr::imap(models, function(model, model_name) {
  prediction_before <- predict_model(model, test_predictors)
  original_size <- as.numeric(utils::object.size(model$fit))

  reduced_fit <- tryCatch(
    butcher::butcher(model$fit),
    error = function(error) {
      cli::cli_inform(
        "Keeping {.val {model_name}} unchanged: {conditionMessage(error)}"
      )
      NULL
    }
  )

  if (is.null(reduced_fit)) return(model)

  reduced_size <- as.numeric(utils::object.size(reduced_fit))

  if (reduced_size >= original_size) {
    cli::cli_inform(
      "Keeping {.val {model_name}} unchanged: butcher did not reduce its size."
    )
    return(model)
  }

  reduced_model <- model
  reduced_model$fit <- reduced_fit
  prediction_after <- predict_model(reduced_model, test_predictors)

  if (!isTRUE(all.equal(prediction_before, prediction_after, tolerance = 1e-12))) stop("Butchering changed predictions for model: ", model_name)

  cli::cli_inform(
    "Reduced {.val {model_name}} from {format(original_size, big.mark = ',')} to {format(reduced_size, big.mark = ',')} bytes."
  )

  reduced_model
})

# El booster de XGBoost tiene una serialización binaria propia, más estable y
# compacta que guardar directamente el objeto externo dentro del RDS.
models <- purrr::map(models, function(model) {
  if (identical(model$type, "xgboost") && !is.raw(model$fit)) model$fit <- xgboost::xgb.save.raw(model$fit)

  model
})

# La probabilidad media es la referencia desde la cual se interpretan después
# las contribuciones SHAP locales de cada modelo.
baseline <- predictions |>
  dplyr::summarise(value = mean(score), .by = model) |>
  tibble::deframe()

# 5. Artefacto ------------------------------------------------------------
cli::cli_h1("Artefacto")

# Esta es la fuente común para SHAP, efectos, importancia y evaluación. Guardar
# semillas y convenciones permite reproducir y auditar todos los resultados.
credit_models <- list(
  train = train,
  test = test,
  predictors = predictors,
  models = models,
  predictions = predictions,
  baseline = baseline,
  metadata = list(
    version = 1L,
    data = "modeldata::credit_data",
    target = "status_bad",
    positive_class = 1L,
    sample = "test",
    split = "stratified 75/25 train/test",
    split_seed = split_seed,
    model_seed = model_seed,
    model_labels = vapply(models, `[[`, character(1), "label"),
    prepared_at = Sys.time()
  )
)

# train se conserva solo en este artefacto intermedio porque la importancia
# Drop-column debe reentrenar cada modelo sin una variable. No llega a las apps.
saveRDS(
  credit_models,
  "R/credit-data/credit-models.rds",
  compress = "xz"
)

cli::cli_alert_success("Saved {.path R/credit-data/credit-models.rds}.")
