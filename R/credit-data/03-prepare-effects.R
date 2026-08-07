# Paquetes usados directamente por este script o necesarios para predecir con
# los modelos guardados. rpart y randomForest deben cargar sus métodos predict()
# porque los modelos se leen desde un RDS creado en una sesión anterior.
required_packages <- c(
  "cli", "dplyr", "purrr", "randomForest", "rpart", "tibble", "xgboost"
)

# requireNamespace() comprueba y carga cada namespace sin adjuntarlo al buscador.
# vapply() devuelve TRUE/FALSE por paquete; con ! conservamos solo los ausentes.
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

# Es mejor detenerse al inicio con un mensaje claro que fallar a mitad de un
# cálculo costoso porque falta un paquete.
if (length(missing_packages)) stop("Install missing packages: ", paste(missing_packages, collapse = ", "))

# Carga predict_model() y save_credit_artifact(). local = TRUE evita dejar los
# objetos del archivo auxiliar en el entorno global cuando se usa source().
source("R/credit-data/00-helpers.R", local = TRUE)

# 1. Preparación ----------------------------------------------------------
cli::cli_h1("Preparación")

# Recupera el mismo split, predictores y modelos preparados en el paso 01. Así
# todos los análisis explican exactamente los mismos modelos y observaciones.
credit_models <- readRDS("R/credit-data/credit-models.rds")

# ICE evaluará hasta 25 valores por variable. ALE dividirá cada variable en 10
# intervalos. La L indica que son números enteros en R.
grid_size <- 25L
ale_bins <- 10L

# Separamos los objetos que se usarán varias veces para simplificar el código.
test <- credit_models$test
predictors <- credit_models$predictors

# El modelo solo debe recibir las columnas con las que fue entrenado. row_id y
# status_bad sirven para identificar/evaluar casos, pero no son predictores.
test_predictors <- test |>
  dplyr::select(dplyr::all_of(predictors))

# 2. Individual Conditional Expectation (ICE) -----------------------------
cli::cli_h1("ICE")

# Para cada predictor construimos una grilla de valores donde calcularemos ICE.
# Se usan cuantiles en vez de puntos equidistantes para concentrar la grilla en
# las zonas donde realmente hay datos y evitar demasiados puntos casi vacíos.
variable_grids <- predictors |>
  # Convierte el vector en una lista nombrada: seniority, time, age, etc.
  stats::setNames(predictors) |>
  purrr::map(function(variable) {
    # seq(0, 1, ...) pide cuantiles desde el mínimo hasta el máximo.
    # unique() elimina cuantiles repetidos, frecuentes en variables discretas.
    unique(as.numeric(stats::quantile(
      test[[variable]],
      probs = seq(0, 1, length.out = grid_size),
      names = FALSE
    )))
  })

# El total usa las grillas reales después de unique(), no grid_size * variables;
# por eso los cuantiles repetidos no dejan la barra incompleta.
ice_steps <- sum(lengths(variable_grids))

# ICE pregunta: "para este mismo cliente, ¿cómo cambiaría la predicción si solo
# cambiáramos una variable?" Se conserva una fila por cliente, modelo, variable
# y punto de la grilla. imap_dfr() entrega también el nombre de cada modelo y
# une todos los resultados por filas.
#
# Valores de ejemplo para seguir una iteración del bloque interior:
# model_name <- "logistic"
# model <- credit_models$models[[model_name]]
# variable <- "price"
# grid_id <- 10L
ice_progress <- cli::cli_progress_bar(
  name = "ICE",
  total = ice_steps * length(credit_models$models),
  current = 0,
  auto_terminate = TRUE,
  clear = FALSE,
  format = paste(
    "{cli::pb_name} {cli::pb_bar} {cli::pb_percent}",
    "| {cli::pb_status} | {cli::pb_current}/{cli::pb_total}",
    "| ETA: {cli::pb_eta}"
  )
)

ice_values <- purrr::imap_dfr(
  credit_models$models,
  function(model_object, model_name) {
    # XGBoost se guarda como bytes para reducir el RDS. Lo reconstruimos una vez
    # por modelo, en vez de hacerlo nuevamente en cada punto de la grilla.
    if (identical(model_object$type, "xgboost") && is.raw(model_object$fit)) model_object$fit <- xgboost::xgb.load.raw(model_object$fit)

    # Repite el cálculo para cada predictor.
    purrr::map_dfr(predictors, function(variable) {
      grid <- variable_grids[[variable]]

      # Repite el cálculo para cada valor representativo del predictor.
      purrr::map_dfr(seq_along(grid), function(grid_id) {
        progress_status <- paste0(
          model_object$label, " | ", variable,
          " | grid ", grid_id, "/", length(grid)
        )

        # Copiamos los datos para no modificar el test original. Se reemplaza
        # la variable analizada por el mismo valor en todos los clientes; las
        # demás variables mantienen los valores reales de cada cliente.
        modified_data <- test_predictors
        modified_data[[variable]] <- grid[[grid_id]]

        # Se obtiene el escalar antes de tibble(). Dentro de tibble(), una columna
        # llamada grid_id ocultaría al argumento grid_id de esta función.
        grid_value <- grid[[grid_id]]

        # predict_model() devuelve una estimación por cliente. Guardar row_id
        # permite dibujar cada curva ICE individual. Promediar estimate entre
        # los clientes para un mismo x produce posteriormente la curva PDP.
        result <- tibble::tibble(
          model = model_name,
          variable = variable,
          row_id = test$row_id,
          grid_id = grid_id,
          x = grid_value,
          estimate = predict_model(model_object, modified_data)
        )

        cli::cli_progress_update(
          id = ice_progress,
          status = progress_status
        )
        result
      })
    })
  }
)

# 3. Accumulated Local Effects (ALE) --------------------------------------
cli::cli_h1("ALE")

# ALE usa otra grilla: sus valores son límites de intervalos, no puntos donde
# se evalúa directamente una curva. Se calcula una sola vez porque todos los
# modelos explican la misma muestra de test.
ale_breaks <- predictors |>
  stats::setNames(predictors) |>
  purrr::map(function(variable) {
    unique(as.numeric(stats::quantile(
      test[[variable]],
      probs = seq(0, 1, length.out = ale_bins + 1L),
      names = FALSE
    )))
  })

# Cada vector de n límites genera n - 1 intervalos efectivos.
ale_steps <- sum(pmax(lengths(ale_breaks) - 1L, 0L))

# ALE pregunta: "entre dos valores cercanos observados, ¿cuánto cambia en
# promedio la predicción?" A diferencia de ICE/PDP, solo usa clientes que están
# realmente dentro de cada intervalo. Esto reduce extrapolaciones poco realistas
# cuando los predictores están correlacionados.
ale_progress <- cli::cli_progress_bar(
  name = "ALE",
  total = ale_steps * length(credit_models$models),
  current = 0,
  auto_terminate = TRUE,
  clear = FALSE,
  format = paste(
    "{cli::pb_name} {cli::pb_bar} {cli::pb_percent}",
    "| {cli::pb_status} | {cli::pb_current}/{cli::pb_total}",
    "| ETA: {cli::pb_eta}"
  )
)

# Valores de ejemplo para seguir una iteración del bloque interior:
# model_name <- "logistic"
# model_object <- credit_models$models[[model_name]]
# variable <- "price"
# bin_index <- 5L
ale_values <- purrr::imap_dfr(
  credit_models$models,
  function(model_object, model_name) {
    if (identical(model_object$type, "xgboost") && is.raw(model_object$fit)) model_object$fit <- xgboost::xgb.load.raw(model_object$fit)

    purrr::map_dfr(predictors, function(variable) {
      # Los límites por cuantiles buscan que cada intervalo contenga una cantidad
      # parecida de observaciones. unique() trata variables con pocos valores.
      breaks <- ale_breaks[[variable]]

      # Sin al menos dos límites no existe un intervalo ni se puede medir efecto.
      if (length(breaks) < 2L) return(tibble::tibble())

      # cut() asigna cada observación al intervalo de su valor real. Incluir el
      # límite inferior asegura que la observación mínima no quede fuera.
      interval <- cut(
        test[[variable]],
        breaks = breaks,
        include.lowest = TRUE,
        labels = FALSE
      )

      # Calculamos el efecto local de cada intervalo y juntamos los resultados.
      local_effects <- purrr::map_dfr(
        seq_len(length(breaks) - 1L),
        function(bin_index) {
          progress_status <- sprintf(
            "%s | %s | bin %d/%d",
            model_object$label, variable, bin_index, length(breaks) - 1L
          )

          # Solo se usan las observaciones cuyo valor real cae en este intervalo.
          rows <- which(interval == bin_index)

          # Un intervalo vacío no aporta información.
          if (!length(rows)) {
            cli::cli_progress_update(id = ale_progress, status = progress_status)
            return(tibble::tibble())
          }

          lower <- breaks[[bin_index]]
          upper <- breaks[[bin_index + 1L]]

          # Creamos dos versiones de los mismos clientes: una fija la variable
          # en el límite inferior y otra en el superior. Todo lo demás se conserva.
          lower_data <- test_predictors[rows, , drop = FALSE]
          upper_data <- test_predictors[rows, , drop = FALSE]
          lower_data[[variable]] <- lower
          upper_data[[variable]] <- upper

          # La diferencia de predicciones mide el efecto de atravesar el intervalo.
          # Se promedia entre sus clientes para obtener un único efecto local.
          result <- tibble::tibble(
            bin_id = bin_index,
            lower = lower,
            upper = upper,
            n = length(rows),
            local_effect = mean(
              predict_model(model_object, upper_data) -
                predict_model(model_object, lower_data)
            )
          )

          cli::cli_progress_update(id = ale_progress, status = progress_status)
          result
        }
      ) |>
        # ALE acumula los cambios locales para reconstruir el efecto a lo largo
        # de la variable. Es análogo a sumar pequeños incrementos sucesivos.
        dplyr::mutate(accumulated = cumsum(local_effect))

      # El nivel vertical de una curva de efectos es arbitrario. Calculamos su
      # promedio ponderado por la cantidad de casos para centrar la curva en cero.
      center <- stats::weighted.mean(
        local_effects$accumulated,
        local_effects$n
      )

      # Dejamos una tabla lista para graficar. x es el punto medio del intervalo
      # y estimate es el efecto ALE centrado: positivo aumenta el riesgo respecto
      # del promedio; negativo lo disminuye.
      local_effects |>
        dplyr::transmute(
          model = model_name,
          variable = variable,
          bin_id,
          lower,
          upper,
          x = (lower + upper) / 2,
          n,
          local_effect,
          estimate = accumulated - center
        )
    })
  }
)

# 4. Artefacto ------------------------------------------------------------
cli::cli_h1("Artefacto")

# No guardamos PDP porque duplicaría información: la app lo obtiene promediando
# ICE por model, variable y x. La distribución observada también está en test.
# El artefacto reúne resultados y parámetros para que la app solo filtre y dibuje,
# sin repetir estas predicciones costosas durante su ejecución.
effects_artifact <- list(
  test = test,
  predictors = predictors,
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

# Guarda el artefacto comprimido y crea el directorio de destino si no existe.
save_credit_artifact(
  effects_artifact,
  "variable-effects/credit-effects.rds"
)
