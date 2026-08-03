required_packages <- c("cli", "dplyr", "purrr", "tibble", "xgboost")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "))
}

source("R/credit-data/00-helpers.R", local = TRUE)

analysis_path <- "R/credit-data/credit-analysis.rds"
grid_size <- 25L
ale_bins <- 10L
credit_analysis <- readRDS(analysis_path)

predictors <- credit_analysis$predictors
test <- credit_analysis$test
test_predictors <- test |>
  dplyr::select(dplyr::all_of(predictors))

variable_grid <- function(values, size = 25L) {
  unique(as.numeric(stats::quantile(
    values,
    probs = seq(0, 1, length.out = size),
    names = FALSE
  )))
}

# ICE is stored once. PDP is the mean ICE curve by model, variable and x.
total_ice_steps <- sum(vapply(
  predictors,
  function(variable) length(variable_grid(test[[variable]], grid_size)),
  integer(1)
)) * length(credit_analysis$models)

ice_progress <- cli::cli_progress_bar(
  name = "ICE",
  total = total_ice_steps
)

ice_values <- purrr::imap_dfr(
  credit_analysis$models,
  function(model, model_name) {
    purrr::map_dfr(predictors, function(variable) {
      grid <- variable_grid(test[[variable]], grid_size)

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
  credit_analysis$models,
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

credit_analysis$ice_values <- ice_values
credit_analysis$ale_values <- ale_values
credit_analysis$metadata$effects <- list(
  sample = "test",
  grid_size = grid_size,
  ale_bins = ale_bins,
  pdp_source = "mean ICE by model, variable and x",
  distribution_source = "test"
)

saveRDS(credit_analysis, analysis_path, compress = "gzip")
cli::cli_success("Added variable-effect results to {.path {analysis_path}}.")
