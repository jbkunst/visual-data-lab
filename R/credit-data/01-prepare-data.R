required_packages <- c(
  "butcher", "celavi", "cli", "dplyr", "modeldata", "purrr",
  "randomForest", "rpart", "rsample", "tibble", "tidyr", "xgboost"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install the missing packages before preparing the artifacts: ",
    paste(missing_packages, collapse = ", "),
    if ("celavi" %in% missing_packages) {
      "\nInstall celavi with pak::pkg_install(\"jbkunst/celavi\")."
    } else {
      ""
    }
  )
}

source("R/credit-data/00-helpers.R", local = TRUE)

split_seed <- 2026L
model_seed <- 2026L
shap_seed <- 2026L
importance_seed <- 2026L

grid_size <- 25L
ale_bins <- 10L
permutation_iterations <- 30L
sage_iterations <- 50L

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

# 4. Calculate SHAP -------------------------------------------------------
# Test is both the explained population and the SHAP background.
shap_values <- calculate_shap_values(
  models = models,
  explanation_data = test_predictors,
  background = test_predictors,
  seed = shap_seed
)

expected_shap_rows <- length(models) * nrow(test) * length(predictors)

if (nrow(shap_values) != expected_shap_rows || anyNA(shap_values$shap)) {
  stop("The SHAP table is incomplete.")
}

# 5. Calculate variable effects ------------------------------------------
# ICE is stored once. PDP is obtained by averaging ICE over row_id.
# The variable distribution is read directly from test.
ice_values <- calculate_ice_values(
  models = models,
  test = test,
  predictors = predictors,
  grid_size = grid_size
)

ale_values <- calculate_ale_values(
  models = models,
  test = test,
  predictors = predictors,
  bins = ale_bins
)

# 6. Calculate global importance -----------------------------------------
permutation_importance <- calculate_permutation_importance(
  models = models,
  test = test,
  predictors = predictors,
  iterations = permutation_iterations,
  seed = importance_seed
)

drop_column_importance <- calculate_drop_column_importance(
  models = models,
  train = train,
  test = test,
  predictors = predictors,
  seed = importance_seed
)

sage_importance <- calculate_sage_importance(
  models = models,
  test = test,
  predictors = predictors,
  iterations = sage_iterations,
  seed = importance_seed
)

shap_global_importance <- shap_values |>
  dplyr::summarise(
    importance = mean(abs(shap)),
    .by = c(model, variable)
  ) |>
  dplyr::mutate(
    sample = "test",
    metric = "prediction_change",
    method = "shap_global",
    iteration = NA_integer_,
    loss_before = NA_real_,
    loss_after = NA_real_
  ) |>
  dplyr::select(
    model, sample, metric, method, variable, iteration,
    importance, loss_before, loss_after
  )

importance_values <- dplyr::bind_rows(
  permutation_importance,
  drop_column_importance,
  sage_importance,
  shap_global_importance
)

# 7. Calculate evaluation results ----------------------------------------
evaluation_results <- calculate_evaluation_results(predictions)

threshold_curve <- evaluation_results$threshold_curve
gains_curve <- evaluation_results$gains_curve
evaluation_summary <- evaluation_results$evaluation_summary

# 8. Reduce model size only after all calculations -----------------------
models <- reduce_models(
  models,
  validation_data = test_predictors
) |>
  serialize_models()

# 9. Build reusable results -----------------------------------------------
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

metadata <- list(
  version = 1L,
  data = "modeldata::credit_data",
  target = "status_bad",
  positive_class = 1L,
  sample = "test",
  split = "stratified 75/25 train/test",
  split_seed = split_seed,
  model_seed = model_seed,
  model_labels = vapply(models, `[[`, character(1), "label"),
  shap = list(
    background = "complete test set",
    explained_sample = "complete test set",
    seed = shap_seed
  ),
  effects = list(
    grid_size = grid_size,
    ale_bins = ale_bins,
    pdp_source = "mean ICE by model, variable and x",
    distribution_source = "test"
  ),
  importance = list(
    permutation_package = "celavi",
    permutation_iterations = permutation_iterations,
    sage_implementation = "marginal Monte Carlo path approximation",
    sage_iterations = sage_iterations,
    seed = importance_seed,
    shap_global = "mean(abs(shap))"
  ),
  evaluation = list(
    score_direction = "higher score means higher probability of bad"
  ),
  prepared_at = Sys.time()
)

# This object contains everything a future combined app would need.
# Train is deliberately excluded: it is only needed during preparation.
credit_analysis <- list(
  test = test,
  predictors = predictors,
  models = models,
  predictions = predictions,
  control_meta = control_meta,
  baseline = baseline,
  shap_values = shap_values,
  ice_values = ice_values,
  ale_values = ale_values,
  importance_values = importance_values,
  threshold_curve = threshold_curve,
  gains_curve = gains_curve,
  evaluation_summary = evaluation_summary,
  metadata = metadata
)

# 10. Export minimal app-specific artifacts ------------------------------
shap_artifact <- credit_analysis[c(
  "test", "predictors", "models", "predictions", "control_meta",
  "baseline", "shap_values", "metadata"
)]

effects_artifact <- credit_analysis[c(
  "test", "predictors", "ice_values", "ale_values", "metadata"
)]

importance_artifact <- credit_analysis[c(
  "predictors", "importance_values", "metadata"
)]

evaluation_artifact <- credit_analysis[c(
  "predictions", "threshold_curve", "gains_curve",
  "evaluation_summary", "metadata"
)]

artifact_paths <- c(
  "shap-explorer/shap-credit.rds",
  "variable-effects/credit-effects.rds",
  "global-feature-importance/credit-importance.rds",
  "model-evaluation/credit-evaluation.rds"
)

purrr::walk2(
  list(
    shap_artifact,
    effects_artifact,
    importance_artifact,
    evaluation_artifact
  ),
  artifact_paths,
  save_credit_artifact
)

artifact_sizes <- tibble::tibble(
  file = artifact_paths,
  size_mb = file.info(artifact_paths)$size / 1024^2
)

print(artifact_sizes)
cli::cli_success("Credit analysis artifacts are ready.")
