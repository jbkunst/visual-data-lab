required_packages <- c(
  "celavi", "cli", "dplyr", "purrr", "randomForest",
  "rpart", "tibble", "xgboost"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install missing packages: ",
    paste(missing_packages, collapse = ", "),
    if ("celavi" %in% missing_packages) {
      "\nInstall celavi with pak::pkg_install(\"jbkunst/celavi\")."
    } else {
      ""
    }
  )
}

source("R/credit-data/00-helpers.R", local = TRUE)

analysis_path <- "R/credit-data/credit-analysis.rds"
permutation_iterations <- 30L
sage_iterations <- 50L
importance_seed <- 2026L
credit_analysis <- readRDS(analysis_path)

if (is.null(credit_analysis$shap_values)) {
  stop("Run R/credit-data/02-prepare-shap.R before this script.")
}

predictors <- credit_analysis$predictors
train <- credit_analysis$train
test <- credit_analysis$test
test_predictors <- test |>
  dplyr::select(dplyr::all_of(predictors))

clip_probability <- function(predicted, epsilon = 1e-15) {
  pmin(pmax(as.numeric(predicted), epsilon), 1 - epsilon)
}

log_loss <- function(actual, predicted) {
  predicted <- clip_probability(predicted)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

fit_reduced_model <- function(model_name, variable) {
  reduced_predictors <- setdiff(predictors, variable)
  formula <- stats::reformulate(reduced_predictors, response = "status_bad")
  reduced_train <- train |>
    dplyr::select(status_bad, dplyr::all_of(reduced_predictors))

  fit <- switch(
    model_name,
    logistic = stats::glm(
      formula,
      data = reduced_train,
      family = stats::binomial()
    ),
    tree = rpart::rpart(
      formula,
      data = reduced_train,
      method = "anova",
      control = rpart::rpart.control(
        cp = 0.004,
        minsplit = 60,
        maxdepth = 5,
        xval = 0
      )
    ),
    random_forest = {
      set.seed(importance_seed)
      randomForest::randomForest(
        formula,
        data = reduced_train |>
          dplyr::mutate(status_bad = factor(status_bad, levels = c(0, 1))),
        ntree = 500,
        mtry = ceiling(sqrt(length(reduced_predictors)))
      )
    },
    xgboost = {
      set.seed(importance_seed)
      xgboost::xgb.train(
        data = xgboost::xgb.DMatrix(
          as.matrix(reduced_train[, reduced_predictors, drop = FALSE]),
          label = reduced_train$status_bad,
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
    }
  )

  list(type = model_name, fit = fit)
}

predict_reduced_model <- function(model, newdata, reduced_predictors) {
  prediction <- switch(
    model$type,
    logistic = stats::predict(model$fit, newdata = newdata, type = "response"),
    tree = stats::predict(model$fit, newdata = newdata),
    random_forest = stats::predict(
      model$fit,
      newdata = newdata,
      type = "prob"
    )[, "1"],
    xgboost = stats::predict(
      model$fit,
      xgboost::xgb.DMatrix(
        as.matrix(newdata[, reduced_predictors, drop = FALSE]),
        nthread = 1
      )
    )
  )

  pmin(pmax(as.numeric(prediction), 0), 1)
}

# 1. Permutation importance with celavi ----------------------------------
permutation_importance <- purrr::imap_dfr(
  credit_analysis$models,
  function(model, model_name) {
    set.seed(importance_seed)

    raw_importance <- celavi::variable_importance(
      object = model,
      data = test |>
        dplyr::select(status_bad, dplyr::all_of(predictors)),
      variables = predictors,
      response = "status_bad",
      loss_function = log_loss,
      iterations = permutation_iterations,
      predict_function = function(object, newdata) {
        predict_model(object, newdata)
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
  }
)

# 2. Drop-column importance ---------------------------------------------
full_losses <- purrr::imap_dfr(
  credit_analysis$models,
  function(model, model_name) {
    tibble::tibble(
      model = model_name,
      loss_before = log_loss(
        test$status_bad,
        predict_model(model, test_predictors)
      )
    )
  }
)

drop_progress <- cli::cli_progress_bar(
  name = "Drop-column",
  total = length(credit_analysis$models) * length(predictors)
)

drop_column_importance <- purrr::imap_dfr(
  credit_analysis$models,
  function(model, model_name) {
    purrr::map_dfr(predictors, function(variable) {
      cli::cli_progress_update(id = drop_progress)

      reduced_predictors <- setdiff(predictors, variable)
      reduced_model <- fit_reduced_model(model_name, variable)
      loss_before <- full_losses$loss_before[full_losses$model == model_name]
      loss_after <- log_loss(
        test$status_bad,
        predict_reduced_model(
          reduced_model,
          test,
          reduced_predictors
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
  }
)

cli::cli_progress_done(id = drop_progress)

# 3. Marginal Monte Carlo SAGE approximation ----------------------------
sage_progress <- cli::cli_progress_bar(
  name = "SAGE",
  total = length(credit_analysis$models) * sage_iterations
)

sage_importance <- purrr::imap_dfr(
  credit_analysis$models,
  function(model, model_name) {
    purrr::map_dfr(seq_len(sage_iterations), function(iteration) {
      cli::cli_progress_update(id = sage_progress)
      set.seed(importance_seed + iteration)

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
  }
)

cli::cli_progress_done(id = sage_progress)

# 4. Global SHAP from the same local SHAP table --------------------------
shap_global_importance <- credit_analysis$shap_values |>
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

credit_analysis$importance_values <- importance_values
credit_analysis$metadata$importance <- list(
  sample = "test",
  permutation_package = "celavi",
  permutation_iterations = permutation_iterations,
  sage_implementation = "marginal Monte Carlo path approximation",
  sage_iterations = sage_iterations,
  seed = importance_seed,
  shap_global = "mean(abs(shap))"
)

saveRDS(credit_analysis, analysis_path, compress = "gzip")
cli::cli_success("Added global-importance results to {.path {analysis_path}}.")
