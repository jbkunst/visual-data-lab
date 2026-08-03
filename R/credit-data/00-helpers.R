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

save_credit_artifact <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, path, compress = "xz")
  cli::cli_inform("Saved {.path {path}}")
  invisible(path)
}

log_loss <- function(actual, predicted, epsilon = 1e-15) {
  predicted <- pmin(pmax(as.numeric(predicted), epsilon), 1 - epsilon)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

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
