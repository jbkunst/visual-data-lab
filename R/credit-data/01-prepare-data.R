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

analysis_path <- "R/credit-data/credit-analysis.rds"
split_seed <- 2026L
model_seed <- 2026L

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
  dplyr::select(-split_stratum) |>
  dplyr::mutate(row_id = dplyr::row_number(), .before = 1)

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

# 3. Reduce models and preserve common predictions -----------------------
models <- reduce_models(models, validation_data = test_predictors)

test_predictions <- purrr::map(
  models,
  predict_model,
  newdata = test_predictors
)

validate_predictions(test_predictions, nrow(test))

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

models <- serialize_models(models)

# 4. Start one reusable analysis artifact --------------------------------
credit_analysis <- list(
  train = train,
  test = test,
  predictors = predictors,
  models = models,
  predictions = predictions,
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

saveRDS(credit_analysis, analysis_path, compress = "gzip")
cli::cli_success("Saved the base analysis to {.path {analysis_path}}.")
