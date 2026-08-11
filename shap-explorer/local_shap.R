# Versión educativa y lenta. Construye paso a paso un camino aleatorio desde
# cada cliente del background hasta el perfil x. Se conserva como referencia
# porque refleja directamente la definición del cálculo.
local_shap_trace <- function(model, x, background, seed = 1L) {
  set.seed(seed)

  variables <- names(x)
  n_background <- nrow(background)

  # Construye un camino completo desde cada cliente de referencia hasta x.
  trace <- purrr::map_dfr(seq_len(n_background), function(background_id) {
    
    permutation <- sample(variables)

    initial_state <- background |>
      dplyr::slice(background_id) |>
      dplyr::select(dplyr::all_of(permutation))

    profile_state <- x |>
      dplyr::select(dplyr::all_of(permutation))

    # Parte desde z y reemplaza una variable a la vez hasta llegar a x.
    current <- initial_state
    states <- current

    for (position in seq_along(permutation)) {
      # Toma desde x las variables ya introducidas y desde z las restantes.
      current <- dplyr::bind_cols(
        profile_state |>
          dplyr::select(1:position),
        initial_state |>
          dplyr::select(-(1:position))
      )

      states <- dplyr::bind_rows(states, current)
    }

    dplyr::bind_cols(
      tibble::tibble(
        background_id = background_id,
        step = seq.int(0L, length(permutation)),
        variable = factor(c(NA_character_, permutation), levels = variables)
      ),
      states |>
        dplyr::select(dplyr::all_of(variables))
    )
  })

  # Predice cada estado y compara probabilidades consecutivas del camino.
  probabilities <- predict_model(
    model,
    trace |>
      dplyr::select(dplyr::all_of(variables))
  )

  trace <- trace |>
    dplyr::mutate(probability = probabilities) |>
    dplyr::group_by(background_id) |>
    dplyr::arrange(step, .by_group = TRUE) |>
    dplyr::mutate(diff = probability - dplyr::lag(probability)) |>
    dplyr::ungroup() |>
    dplyr::filter(step > 0L) |>
    dplyr::select(
      background_id = background_id,
      step,
      variable,
      probability,
      diff
    )

  trace
}

# Versión optimizada. Construye los mismos caminos en matrices y predice todos
# los estados juntos. La usan tanto la app como el pipeline offline.
local_shap_trace_optimized <- function(model, x, background, seed = 1L) {
  set.seed(seed)

  variables <- names(x)
  n_variables <- length(variables)
  n_background <- nrow(background)
  permutations <- replicate(
    n_background,
    sample(variables),
    simplify = FALSE
  )

  background_matrix <- as.matrix(background[, variables, drop = FALSE])
  profile <- as.numeric(unlist(x[1, variables, drop = FALSE], use.names = FALSE))
  states <- matrix(
    NA_real_,
    nrow = n_background * (n_variables + 1L),
    ncol = n_variables,
    dimnames = list(NULL, variables)
  )

  for (background_id in seq_len(n_background)) {
    rows <- (background_id - 1L) * (n_variables + 1L) +
      seq_len(n_variables + 1L)
    path <- background_matrix[
      rep(background_id, n_variables + 1L),
      ,
      drop = FALSE
    ]
    positions <- match(permutations[[background_id]], variables)

    for (step in seq_len(n_variables)) {
      path[(step + 1L):(n_variables + 1L), positions[[step]]] <-
        profile[[positions[[step]]]]
    }

    states[rows, ] <- path
  }

  background_id <- rep(seq_len(n_background), each = n_variables + 1L)
  step <- rep(0:n_variables, times = n_background)
  variable <- factor(
    unlist(lapply(permutations, function(x) c(NA_character_, x))),
    levels = variables
  )
  probability <- predict_model(model, as.data.frame(states))
  previous_probability <- numeric(length(probability))
  first_step <- step == 0L
  previous_probability[!first_step] <- probability[which(!first_step) - 1L]
  diff <- probability - previous_probability

  data.frame(
    background_id = background_id[!first_step],
    step = step[!first_step],
    variable = variable[!first_step],
    probability = probability[!first_step],
    diff = diff[!first_step]
  )
}

# Promedia las contribuciones de los caminos: un valor SHAP por variable.
summarize_shap <- function(trace) {
  trace |>
    dplyr::group_by(variable) |>
    dplyr::summarise(shap = mean(diff), .groups = "drop") |>
    tibble::deframe()
}
