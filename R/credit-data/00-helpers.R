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

fit_credit_model <- function(model_name, train, predictors, seed = 2026L) {
  formula <- stats::reformulate(predictors, response = "status_bad")
  model_data <- train |>
    dplyr::select(status_bad, dplyr::all_of(predictors))

  fit <- switch(
    model_name,
    logistic = stats::glm(
      formula,
      data = model_data,
      family = stats::binomial()
    ),
    tree = rpart::rpart(
      formula,
      data = model_data,
      method = "anova",
      control = rpart::rpart.control(
        cp = 0.004,
        minsplit = 60,
        maxdepth = 5,
        xval = 0
      )
    ),
    random_forest = {
      set.seed(seed)
      randomForest::randomForest(
        formula,
        data = model_data |>
          dplyr::mutate(status_bad = factor(status_bad, levels = c(0, 1))),
        ntree = 500,
        mtry = ceiling(sqrt(length(predictors)))
      )
    },
    xgboost = {
      set.seed(seed)
      xgboost::xgb.train(
        data = xgboost::xgb.DMatrix(
          as.matrix(model_data[, predictors, drop = FALSE]),
          label = model_data$status_bad,
          nthread = 1
        ),
        nrounds = 150,
        params = list(
          objective = "binary:logistic",
          eval_metric = "logloss",
          max_depth = 4,
          eta = 0.05,
          subsample = 0.8,
          colsample_bytree = 0.8,
          nthread = 1
        ),
        verbose = 0
      )
    },
    stop("Unsupported model name: ", model_name)
  )

  labels <- c(
    logistic = "Logistic regression",
    tree = "Decision tree",
    random_forest = "Random Forest",
    xgboost = "XGBoost"
  )

  list(type = model_name, label = labels[[model_name]], fit = fit)
}

reduce_models <- function(models, validation_data) {
  purrr::imap(models, function(model, model_name) {
    prediction_before <- predict_model(model, validation_data)
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

    if (is.null(reduced_fit)) {
      return(model)
    }

    reduced_size <- as.numeric(utils::object.size(reduced_fit))

    if (reduced_size >= original_size) {
      cli::cli_inform(
        "Keeping {.val {model_name}} unchanged: butcher did not reduce its size."
      )
      return(model)
    }

    reduced_model <- model
    reduced_model$fit <- reduced_fit
    prediction_after <- predict_model(reduced_model, validation_data)

    if (!isTRUE(all.equal(prediction_before, prediction_after, tolerance = 1e-12))) {
      stop("Butchering changed predictions for model: ", model_name)
    }

    cli::cli_inform(
      "Reduced {.val {model_name}} from {format(original_size, big.mark = ',')} to {format(reduced_size, big.mark = ',')} bytes."
    )

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

clip_probability <- function(predicted, epsilon = 1e-15) {
  pmin(pmax(as.numeric(predicted), epsilon), 1 - epsilon)
}

log_loss <- function(actual, predicted) {
  predicted <- clip_probability(predicted)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

auc_score <- function(actual, predicted) {
  n_positive <- sum(actual == 1L)
  n_negative <- sum(actual == 0L)
  ranks <- rank(predicted, ties.method = "average")

  (
    sum(ranks[actual == 1L]) - n_positive * (n_positive + 1) / 2
  ) / (n_positive * n_negative)
}

variable_grid <- function(values, size = 25L) {
  unique(as.numeric(stats::quantile(
    values,
    probs = seq(0, 1, length.out = size),
    names = FALSE
  )))
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

calculate_ice_values <- function(models, test, predictors, grid_size = 25L) {
  test_predictors <- test |>
    dplyr::select(dplyr::all_of(predictors))

  total_steps <- sum(vapply(
    predictors,
    function(variable) length(variable_grid(test[[variable]], grid_size)),
    integer(1)
  )) * length(models)

  progress_id <- cli::cli_progress_bar(name = "ICE", total = total_steps)

  values <- purrr::imap_dfr(models, function(model, model_name) {
    purrr::map_dfr(predictors, function(variable) {
      grid <- variable_grid(test[[variable]], grid_size)

      purrr::map_dfr(seq_along(grid), function(grid_id) {
        cli::cli_progress_update(id = progress_id)

        modified_data <- test_predictors
        modified_data[[variable]] <- grid[[grid_id]]

        tibble::tibble(
          model = model_name,
          variable = variable,
          row_id = test$row_id,
          grid_id = grid_id,
          x = grid[[grid_id]],
          estimate = predict_model(model, modified_data)
        )
      })
    })
  })

  cli::cli_progress_done(id = progress_id)
  values
}

calculate_ale_values <- function(models, test, predictors, bins = 10L) {
  test_predictors <- test |>
    dplyr::select(dplyr::all_of(predictors))

  purrr::imap_dfr(models, function(model, model_name) {
    purrr::map_dfr(predictors, function(variable) {
      breaks <- unique(as.numeric(stats::quantile(
        test[[variable]],
        probs = seq(0, 1, length.out = bins + 1L),
        names = FALSE
      )))

      if (length(breaks) < 2L) {
        return(tibble::tibble())
      }

      interval <- cut(
        test[[variable]],
        breaks = breaks,
        include.lowest = TRUE,
        labels = FALSE
      )

      local_effects <- purrr::map_dfr(
        seq_len(length(breaks) - 1L),
        function(bin_id) {
          rows <- which(interval == bin_id)

          if (!length(rows)) {
            return(tibble::tibble())
          }

          lower_data <- test_predictors[rows, , drop = FALSE]
          upper_data <- test_predictors[rows, , drop = FALSE]
          lower_data[[variable]] <- breaks[[bin_id]]
          upper_data[[variable]] <- breaks[[bin_id + 1L]]

          tibble::tibble(
            bin_id = bin_id,
            lower = breaks[[bin_id]],
            upper = breaks[[bin_id + 1L]],
            n = length(rows),
            local_effect = mean(
              predict_model(model, upper_data) -
                predict_model(model, lower_data)
            )
          )
        }
      ) |>
        dplyr::mutate(accumulated = cumsum(local_effect))

      center <- stats::weighted.mean(
        local_effects$accumulated,
        local_effects$n
      )

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
  })
}

calculate_permutation_importance <- function(
  models,
  test,
  predictors,
  iterations = 30L,
  seed = 2026L
) {
  purrr::imap_dfr(models, function(model, model_name) {
    set.seed(seed)

    raw_importance <- celavi::variable_importance(
      object = model,
      data = test |>
        dplyr::select(status_bad, dplyr::all_of(predictors)),
      variables = predictors,
      response = "status_bad",
      loss_function = log_loss,
      iterations = iterations,
      predict_function = function(object, newdata) {
        predict_model(
          object,
          newdata[, predictors, drop = FALSE]
        )
      },
      verbose = TRUE
    )

    full_loss <- raw_importance |>
      dplyr::filter(variable == "_full_model_") |>
      dplyr::select(iteration, loss_before = value)

    raw_importance |>
      dplyr::filter(variable %in% predictors) |>
      dplyr::left_join(full_loss, by = "iteration") |>
      dplyr::transmute(
        model = model_name,
        sample = "test",
        metric = "log_loss",
        method = "permutation",
        variable,
        iteration,
        importance = value - loss_before,
        loss_before,
        loss_after = value
      )
  })
}

calculate_drop_column_importance <- function(
  models,
  train,
  test,
  predictors,
  seed = 2026L
) {
  test_predictors <- test |>
    dplyr::select(dplyr::all_of(predictors))

  full_losses <- purrr::imap_dfr(models, function(model, model_name) {
    tibble::tibble(
      model = model_name,
      loss_before = log_loss(
        test$status_bad,
        predict_model(model, test_predictors)
      )
    )
  })

  progress_id <- cli::cli_progress_bar(
    name = "Drop-column",
    total = length(models) * length(predictors)
  )

  values <- purrr::imap_dfr(models, function(model, model_name) {
    purrr::map_dfr(predictors, function(variable) {
      cli::cli_progress_update(id = progress_id)

      reduced_predictors <- setdiff(predictors, variable)
      reduced_model <- fit_credit_model(
        model_name,
        train = train,
        predictors = reduced_predictors,
        seed = seed
      )
      loss_before <- full_losses$loss_before[full_losses$model == model_name]
      loss_after <- log_loss(
        test$status_bad,
        predict_model(
          reduced_model,
          test[, reduced_predictors, drop = FALSE]
        )
      )

      tibble::tibble(
        model = model_name,
        sample = "test",
        metric = "log_loss",
        method = "drop_column",
        variable = variable,
        iteration = NA_integer_,
        importance = loss_after - loss_before,
        loss_before,
        loss_after
      )
    })
  })

  cli::cli_progress_done(id = progress_id)
  values
}

calculate_sage_importance <- function(
  models,
  test,
  predictors,
  iterations = 50L,
  seed = 2026L
) {
  test_predictors <- test |>
    dplyr::select(dplyr::all_of(predictors))

  progress_id <- cli::cli_progress_bar(
    name = "SAGE",
    total = length(models) * iterations
  )

  values <- purrr::imap_dfr(models, function(model, model_name) {
    purrr::map_dfr(seq_len(iterations), function(iteration) {
      cli::cli_progress_update(id = progress_id)
      set.seed(seed + iteration)

      variable_order <- sample(predictors)
      reference_rows <- sample.int(nrow(test_predictors))
      current_data <- test_predictors[reference_rows, , drop = FALSE]
      loss_before <- log_loss(
        test$status_bad,
        predict_model(model, current_data)
      )
      iteration_values <- vector("list", length(variable_order))

      for (position in seq_along(variable_order)) {
        variable <- variable_order[[position]]
        current_data[[variable]] <- test_predictors[[variable]]
        loss_after <- log_loss(
          test$status_bad,
          predict_model(model, current_data)
        )

        iteration_values[[position]] <- tibble::tibble(
          model = model_name,
          sample = "test",
          metric = "log_loss",
          method = "sage",
          variable = variable,
          iteration = iteration,
          importance = loss_before - loss_after,
          loss_before,
          loss_after
        )

        loss_before <- loss_after
      }

      dplyr::bind_rows(iteration_values)
    })
  })

  cli::cli_progress_done(id = progress_id)
  values
}

calculate_evaluation_results <- function(predictions) {
  threshold_curve <- predictions |>
    dplyr::group_by(model) |>
    dplyr::group_modify(function(data, key) {
      n_positive <- sum(data$status_bad == 1L)
      n_negative <- sum(data$status_bad == 0L)

      curve <- data |>
        dplyr::group_by(score) |>
        dplyr::summarise(
          positives = sum(status_bad == 1L),
          negatives = sum(status_bad == 0L),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(score)) |>
        dplyr::mutate(
          true_positive_rate = cumsum(positives) / n_positive,
          false_positive_rate = cumsum(negatives) / n_negative,
          ks_gap = true_positive_rate - false_positive_rate,
          threshold = score
        ) |>
        dplyr::select(
          threshold,
          true_positive_rate,
          false_positive_rate,
          ks_gap
        )

      dplyr::bind_rows(
        tibble::tibble(
          threshold = Inf,
          true_positive_rate = 0,
          false_positive_rate = 0,
          ks_gap = 0
        ),
        curve
      )
    }) |>
    dplyr::ungroup()

  gains_curve <- predictions |>
    dplyr::group_by(model) |>
    dplyr::group_modify(function(data, key) {
      ordered <- data |>
        dplyr::arrange(dplyr::desc(score)) |>
        dplyr::mutate(
          population_fraction = dplyr::row_number() / dplyr::n(),
          positive_fraction = cumsum(status_bad) / sum(status_bad),
          lift = positive_fraction / population_fraction
        ) |>
        dplyr::select(
          row_id,
          score,
          population_fraction,
          positive_fraction,
          lift
        )

      dplyr::bind_rows(
        tibble::tibble(
          row_id = NA_integer_,
          score = Inf,
          population_fraction = 0,
          positive_fraction = 0,
          lift = NA_real_
        ),
        ordered
      )
    }) |>
    dplyr::ungroup()

  evaluation_summary <- purrr::map_dfr(
    unique(predictions$model),
    function(model_name) {
      model_predictions <- predictions |>
        dplyr::filter(model == model_name)
      model_thresholds <- threshold_curve |>
        dplyr::filter(model == model_name)
      ks_row <- model_thresholds |>
        dplyr::slice_max(ks_gap, n = 1, with_ties = FALSE)
      auc <- auc_score(
        model_predictions$status_bad,
        model_predictions$score
      )

      tibble::tibble(
        model = model_name,
        sample = "test",
        log_loss = log_loss(
          model_predictions$status_bad,
          model_predictions$score
        ),
        auc = auc,
        gini = 2 * auc - 1,
        ks = ks_row$ks_gap,
        ks_threshold = ks_row$threshold
      )
    }
  )

  list(
    threshold_curve = threshold_curve,
    gains_curve = gains_curve,
    evaluation_summary = evaluation_summary
  )
}
