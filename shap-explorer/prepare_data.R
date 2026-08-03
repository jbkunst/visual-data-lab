# packages ---------------------------------------------------------------
library(tidyverse)
source("shap-explorer/local_shap.R", local = TRUE)
# 1. Prepare data ---------------------------------------------------------
# Select the modeling sample and define the response and predictors.
# Keep numeric predictors for consistent sliders and model inputs, not because they are more important.
predictors <- c(
  "seniority", "time", "age", "expenses", "income",
  "assets", "debt", "amount", "price"
)

train <- modeldata::credit_data |>
  as_tibble() |>
  rename_with(tolower) |>
  drop_na(status, all_of(predictors)) |>
  mutate(status_bad = as.integer(status == "bad")) |>
  select(status_bad, all_of(predictors))

predictor_data <- train |>
  select(all_of(predictors))

# 2. Define prediction helper --------------------------------------------
# Use one prediction interface for all supported models.
predict_model <- function(model, newdata) {
  prediction <- switch(
    model$type,
    logistic = stats::predict(model$fit, newdata = newdata, type = "response"),
    tree = stats::predict(model$fit, newdata = newdata),
    random_forest = stats::predict(model$fit, newdata = newdata, type = "prob")[, "1"],
    xgboost = stats::predict(model$fit, xgboost::xgb.DMatrix(as.matrix(newdata), nthread = 1))
  )

  # pmin(pmax(as.numeric(prediction), 0), 1)
  as.numeric(prediction)
}

# 3. Train models ---------------------------------------------------------
# Fit the four models compared in the app.
logistic <- stats::glm(
  status_bad ~ .,
  data = train,
  family = stats::binomial()
)

tree <- rpart::rpart(
  status_bad ~ .,
  data = train,
  method = "anova",
  control = rpart::rpart.control(
    cp = 0.004,
    minsplit = 60,
    maxdepth = 5,
    xval = 0
  )
)

train_class <- train |>
  mutate(status_bad = factor(status_bad, levels = c(0, 1)))

set.seed(2026)
random_forest <- randomForest::randomForest(
  status_bad ~ .,
  data = train_class,
  ntree = 500,
  mtry = ceiling(sqrt(length(predictors)))
)

set.seed(2026)
xgboost <- xgboost::xgb.train(
  data = xgboost::xgb.DMatrix(
    as.matrix(predictor_data),
    label = train$status_bad,
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

models <- list(
  logistic = list(type = "logistic", label = "Logistic regression", fit = logistic),
  tree = list(type = "tree", label = "Decision tree", fit = tree),
  random_forest = list(type = "random_forest", label = "Random Forest", fit = random_forest),
  xgboost = list(type = "xgboost", label = "XGBoost", fit = xgboost)
)

# 4. Precompute portfolio predictions ------------------------------------
# Score the full portfolio for the distribution chart.
portfolio_pd <- map(models, predict_model, newdata = predictor_data)

# 5. Define SHAP background ----------------------------------------------
# Sample the reference clients used by the SHAP approximation.
set.seed(2026)
background <- predictor_data |>
  slice_sample(n = min(500L, nrow(predictor_data)))

# Use the same reference population for the displayed baseline and SHAP values.
baseline <- map_dbl(models, ~ mean(predict_model(.x, background)))

# 6. Precompute SHAP dependence data -------------------------------------
# Calculate the variable values and contributions shown in dependence plots.
set.seed(2026)
shap_rows <- sample(
  seq_len(nrow(predictor_data)),
  min(500L, nrow(predictor_data))
)

dependence_data <- map_dfr(names(models), function(model_name) {
  model <- models[[model_name]]

  progress_id <- cli::cli_progress_bar(
    name = paste("SHAP dependence:", model$label),
    total = length(shap_rows),
    format = paste(
      "{cli::pb_name} {cli::pb_bar} {cli::pb_percent}",
      "| {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  )

  model_dependence <- map_dfr(shap_rows, function(row_id) {
    cli::cli_progress_update(id = progress_id)
    x <- predictor_data |>
      slice(row_id)
    values <- local_shap_trace_optimized(
      model,
      x = x,
      background = background,
      seed = 2026L
    ) |>
      summarize_shap()

    tibble(
      model = model_name,
      row_id = row_id,
      variable = predictors,
      value = as.numeric(unlist(x[1, predictors], use.names = FALSE)),
      shap = unname(values)
    )
  })

  cli::cli_progress_done(id = progress_id)
  model_dependence
})

# 7. Create slider metadata ----------------------------------------------
# Derive the slider ranges and defaults from the training sample.
control_meta <- predictors |>
  set_names() |>
  map(function(variable) {
    x <- train[[variable]]

    list(
      min = min(x),
      max = max(x),
      value = unname(stats::median(x))
    )
  })

# 8. Save artifact --------------------------------------------------------
# Serialize the models and precomputed data consumed by the app.
dir.create("shap-explorer/data", recursive = TRUE, showWarnings = FALSE)
models$xgboost$fit <- xgboost::xgb.save.raw(models$xgboost$fit)

saveRDS(
  list(
    predictor_data = predictor_data,
    predictors = predictors,
    control_meta = control_meta,
    models = models,
    portfolio_pd = portfolio_pd,
    baseline = baseline,
    background = background,
    dependence_data = dependence_data
  ),
  "shap-explorer/data/shap-credit.rds",
  compress = "xz"
)

message("Saved shap-explorer/data/shap-credit.rds")
