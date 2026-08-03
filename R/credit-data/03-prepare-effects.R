required_packages <- c("cli", "dplyr", "purrr", "tibble", "xgboost")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "))
}

source("R/credit-data/00-helpers.R", local = TRUE)

credit_models <- readRDS("R/credit-data/credit-models.rds")
grid_size <- 25L
ale_bins <- 10L

test <- credit_models$test
predictors <- credit_models$predictors
test_predictors <- test |>
  dplyr::select(dplyr::all_of(predictors))

variable_grids <- predictors |>
  stats::setNames(predictors) |>
  purrr::map(function(variable) {
    unique(as.numeric(stats::quantile(
      test[[variable]],
      probs = seq(0, 1, length.out = grid_size),
      names = FALSE
    )))
  })

ice_progress <- cli::cli_progress_bar(
  name = "ICE",
  total = sum(lengths(variable_grids)) * length(credit_models$models)
)

ice_values <- purrr::imap_dfr(
  credit_models$models,
  function(model, model_name) {
    purrr::map_dfr(predictors, function(variable) {
      grid <- variable_grids[[variable]]

      purrr::map_dfr(seq_along(grid), function(grid_id) {
        cli::cli_progress_update(id = ice_progress)

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
  }
)

cli::cli_progress_done(id = ice_progress)

ale_values <- purrr::imap_dfr(
  credit_models$models,
  function(model, model_name) {
    purrr::map_dfr(predictors, function(variable) {
      breaks <- unique(as.numeric(stats::quantile(
        test[[variable]],
        probs = seq(0, 1, length.out = ale_bins + 1L),
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
  }
)

# PDP is not duplicated: the app obtains it by averaging ICE over row_id.
# The observed distribution is read directly from test.
effects_artifact <- list(
  test = test,
  predictors = predictors,
  ice_values = ice_values,
  ale_values = ale_values,
  metadata = c(
    credit_models$metadata,
    list(
      effects = list(
        sample = "test",
        grid_size = grid_size,
        ale_bins = ale_bins,
        pdp_source = "mean ICE by model, variable and x",
        distribution_source = "test"
      )
    )
  )
)

save_credit_artifact(
  effects_artifact,
  "variable-effects/credit-effects.rds"
)
