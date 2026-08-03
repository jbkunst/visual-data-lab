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

# 1. Prepare data ---------------------------------------------------------
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

# 2. Train shared models --------------------------------------------------
model_names <- c("logistic", "tree", "random_forest", "xgboost")

models <- model_names |>
  stats::setNames(model_names) |>
  purrr::map(
    fit_credit_model,
    train = train,
    predictors = predictors,
    seed = model_seed
  )

# 3. Predict the complete test sample ------------------------------------
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

# 4. Reduce models after predictions are validated -----------------------
models <- reduce_models(
  models,
  validation_data = test_predictors
) |>
  serialize_models()

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

baseline <- predictions |>
  dplyr::summarise(value = mean(score), .by = model) |>
  tibble::deframe()

credit_models <- list(
  train = train,
  test = test,
  predictors = predictors,
  models = models,
  predictions = predictions,
  control_meta = control_meta,
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

# This is an intermediate preparation artifact. Train is kept here only
# because Drop-column importance must retrain models without each variable.
saveRDS(
  credit_models,
  "R/credit-data/credit-models.rds",
  compress = "xz"
)

cli::cli_success("Saved {.path R/credit-data/credit-models.rds}.")
