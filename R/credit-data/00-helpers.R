predict_model <- function(model, newdata) {
  fit <- model$fit

  if (identical(model$type, "xgboost") && is.raw(fit)) {
    fit <- xgboost::xgb.load.raw(fit)
  }

  prediction <- switch(
    model$type,
    logistic = stats::predict(fit, newdata = newdata, type = "response"),
    tree = stats::predict(fit, newdata = newdata),
    random_forest = stats::predict(fit, newdata = newdata, type = "prob")[, "1"],
    xgboost = stats::predict(
      fit,
      xgboost::xgb.DMatrix(as.matrix(newdata), nthread = 1)
    ),
    stop("Unsupported model type: ", model$type)
  )

  pmin(pmax(as.numeric(prediction), 0), 1)
}

reduce_models <- function(models, validation_data) {
  purrr::imap(models, function(model, model_name) {
    prediction_before <- predict_model(model, validation_data)
    original_size <- as.numeric(utils::object.size(model$fit))
    reduced_fit <- butcher::butcher(model$fit)
    reduced_size <- as.numeric(utils::object.size(reduced_fit))

    if (reduced_size >= original_size) {
      cli::cli_inform("Keeping {.val {model_name}} unchanged: butcher did not reduce its size.")
      return(model)
    }

    reduced_model <- model
    reduced_model$fit <- reduced_fit
    prediction_after <- predict_model(reduced_model, validation_data)

    if (!isTRUE(all.equal(prediction_before, prediction_after, tolerance = 1e-12))) {
      stop("Butchering changed predictions for model: ", model_name)
    }

    cli::cli_inform(c(
      "Reduced {.val {model_name}} from {format(original_size, big.mark = ',')} to {format(reduced_size, big.mark = ',')} bytes."
    ))

    reduced_model
  })
}

serialize_models <- function(models) {
  purrr::map(models, function(model) {
    if (identical(model$type, "xgboost") && !is.raw(model$fit)) {
      model$fit <- xgboost::xgb.save.raw(model$fit)
    }

    model
  })
}

validate_predictions <- function(predictions, n_expected) {
  purrr::iwalk(predictions, function(prediction, model_name) {
    if (!is.numeric(prediction)) {
      stop("Predictions are not numeric for model: ", model_name)
    }

    if (length(prediction) != n_expected) {
      stop("Unexpected prediction length for model: ", model_name)
    }

    if (anyNA(prediction)) {
      stop("Missing predictions for model: ", model_name)
    }

    if (any(prediction < 0 | prediction > 1)) {
      stop("Predictions outside [0, 1] for model: ", model_name)
    }
  })

  invisible(predictions)
}

save_credit_artifact <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, path, compress = "xz")
  cli::cli_inform("Saved {.path {path}}")
  invisible(path)
}

# Walk one random feature permutation from every background client to x.
local_shap_trace <- function(model, x, background, seed = 1L) {
  set.seed(seed)

  variables <- names(x)
  n_background <- nrow(background)

  trace <- purrr::map_dfr(seq_len(n_background), function(background_id) {
    permutation <- sample(variables)

    initial_state <- background |>
      dplyr::slice(background_id) |>
      dplyr::select(dplyr::all_of(permutation))

    profile_state <- x |>
      dplyr::select(dplyr::all_of(permutation))

    current <- initial_state
    states <- current

    for (position in seq_along(permutation)) {
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

  probabilities <- predict_model(
    model,
    trace |>
      dplyr::select(dplyr::all_of(variables))
  )

  trace |>
    dplyr::mutate(probability = probabilities) |>
    dplyr::group_by(background_id) |>
    dplyr::arrange(step, .by_group = TRUE) |>
    dplyr::mutate(diff = probability - dplyr::lag(probability)) |>
    dplyr::ungroup() |>
    dplyr::filter(step > 0L) |>
    dplyr::select(background_id, step, variable, probability, diff)
}

# Vectorized implementation used for precomputation.
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
    unlist(lapply(permutations, function(permutation) c(NA_character_, permutation))),
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

summarize_shap <- function(trace) {
  trace |>
    dplyr::group_by(variable) |>
    dplyr::summarise(shap = mean(diff), .groups = "drop") |>
    tibble::deframe()
}

calculate_shap_values <- function(models, explanation_data, background, seed = 2026L) {
  purrr::map_dfr(names(models), function(model_name) {
    model <- models[[model_name]]
    progress_id <- cli::cli_progress_bar(
      name = paste("SHAP:", model$label),
      total = nrow(explanation_data),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_percent}",
        "| {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
      )
    )

    values <- purrr::map_dfr(seq_len(nrow(explanation_data)), function(row_id) {
      cli::cli_progress_update(id = progress_id)

      shap <- local_shap_trace_optimized(
        model,
        x = explanation_data[row_id, , drop = FALSE],
        background = background,
        seed = seed
      ) |>
        summarize_shap()

      tibble::tibble(
        model = model_name,
        row_id = row_id,
        variable = names(shap),
        shap = unname(shap)
      )
    })

    cli::cli_progress_done(id = progress_id)
    values
  })
}
