predict_model <- function(model, newdata) {
  fit <- model$fit

  if (identical(model$type, "xgboost") && is.raw(fit)) fit <- xgboost::xgb.load.raw(fit)

  prediction <- switch(
    model$type,
    logistic = stats::predict(fit, newdata = newdata, type = "response"),
    tree = stats::predict(fit, newdata = newdata, type = "prob")[, "1"],
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
      data = model_data |>
        dplyr::mutate(status_bad = factor(status_bad, levels = c(0, 1))),
      method = "class",
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

individual_log_loss <- function(actual, predicted, epsilon = 1e-15) {
  predicted <- pmin(pmax(as.numeric(predicted), epsilon), 1 - epsilon)
  -(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

log_loss <- function(actual, predicted, epsilon = 1e-15) {
  mean(individual_log_loss(actual, predicted, epsilon))
}

auc_roc <- function(actual, predicted) {
  actual <- as.integer(actual)
  n_positive <- sum(actual == 1L)
  n_negative <- sum(actual == 0L)

  if (!n_positive || !n_negative) stop("AUC requires both outcome classes.")

  ranks <- rank(predicted, ties.method = "average")
  (
    sum(ranks[actual == 1L]) - n_positive * (n_positive + 1) / 2
  ) / (n_positive * n_negative)
}

# Permutation importance expects a loss: larger values must be worse.
one_minus_auc <- function(actual, predicted) {
  1 - auc_roc(actual, predicted)
}

ks_statistic <- function(actual, predicted) {
  actual <- as.integer(actual)
  n_positive <- sum(actual == 1L)
  n_negative <- sum(actual == 0L)

  if (!n_positive || !n_negative) stop("KS requires both outcome classes.")

  counts <- stats::aggregate(
    cbind(positives = actual == 1L, negatives = actual == 0L),
    by = list(score = predicted), sum
  )
  counts <- counts[order(counts$score, decreasing = TRUE), ]

  max(c(
    0,
    cumsum(counts$positives) / n_positive -
      cumsum(counts$negatives) / n_negative
  ))
}

# Permutation and SAGE expect a loss: larger values must be worse.
one_minus_ks <- function(actual, predicted) {
  1 - ks_statistic(actual, predicted)
}
