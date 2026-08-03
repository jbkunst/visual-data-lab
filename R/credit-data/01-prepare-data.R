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

split_seed <- 2026L
model_seed <- 2026L
shap_seed <- 2026L

# 1. Prepare the modeling sample -----------------------------------------
predictors <- c(
  "seniority", "time", "age", "expenses", "income",
  "assets", "debt", "amount", "price"
)

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
credit_split <- rsample::initial_split(
  credit_data,
  prop = 0.75,
  strata = split_stratum
)

train <- rsample::training(credit_split) |>
  dplyr::select(-split_stratum)

test <- rsample::testing(credit_split) |>
  dplyr::select(-split_stratum)

train_predictors <- train |>
  dplyr::select(dplyr::all_of(predictors))

test_predictors <- test |>
  dplyr::select(dplyr::all_of(predictors))

cli::cli_inform(c(
  "Prepared {nrow(credit_data)} complete observations.",
  "i" = "Train: {nrow(train)} observations.",
  "i" = "Test: {nrow(test)} observations."
))

# 2. Train the four shared models ----------------------------------------
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
  dplyr::mutate(status_bad = factor(status_bad, levels = c(0, 1)))

set.seed(model_seed)
random_forest <- randomForest::randomForest(
  status_bad ~ .,
  data = train_class,
  ntree = 500,
  mtry = ceiling(sqrt(length(predictors)))
)

set.seed(model_seed)
xgboost <- xgboost::xgb.train(
  data = xgboost::xgb.DMatrix(
    as.matrix(train_predictors),
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

# 3. Reduce model size and validate predictions --------------------------
models <- reduce_models(models, validation_data = test_predictors)

test_predictions <- purrr::map(
  models,
  predict_model,
  newdata = test_predictors
)

validate_predictions(test_predictions, nrow(test))

# 4. Precompute SHAP values ----------------------------------------------
# Test is used directly as both the explained population and SHAP background.
# No separate background object or row identifiers are stored in the artifacts.
shap_values <- calculate_shap_values(
  models = models,
  explanation_data = test_predictors,
  background = test_predictors,
  seed = shap_seed
)

expected_shap_rows <- length(models) * nrow(test) * length(predictors)

if (nrow(shap_values) != expected_shap_rows || anyNA(shap_values$shap)) {
  stop("The precomputed SHAP table is incomplete.")
}

# 5. Build small app-specific artifacts ----------------------------------
control_meta <- predictors |>
  stats::setNames(predictors) |>
  purrr::map(function(variable) {
    values <- train[[variable]]

    list(
      min = min(values),
      max = max(values),
      value = unname(stats::median(values))
    )
  })

serialized_models <- serialize_models(models)

shap_artifact <- list(
  test = test_predictors,
  predictors = predictors,
  models = serialized_models,
  control_meta = control_meta,
  shap_values = shap_values
)

effects_artifact <- list(
  test = test_predictors,
  predictors = predictors,
  models = serialized_models
)

importance_artifact <- list(
  train = train,
  test = test,
  predictors = predictors,
  models = serialized_models,
  shap_values = shap_values
)

evaluation_artifact <- tibble::tibble(
  row_id = seq_len(nrow(test)),
  status_bad = test$status_bad
) |>
  dplyr::bind_cols(tibble::as_tibble(test_predictions))

# 6. Save one artifact beside each app -----------------------------------
save_credit_artifact(
  shap_artifact,
  "shap-explorer/shap-credit.rds"
)

save_credit_artifact(
  effects_artifact,
  "variable-effects/credit-effects.rds"
)

save_credit_artifact(
  importance_artifact,
  "global-feature-importance/credit-importance.rds"
)

save_credit_artifact(
  evaluation_artifact,
  "model-evaluation/credit-evaluation.rds"
)

cli::cli_success("Credit artifacts are ready.")
