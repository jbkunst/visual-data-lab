required_packages <- c("cli", "dplyr", "purrr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "))
}

analysis_path <- "R/credit-data/credit-analysis.rds"
credit_analysis <- readRDS(analysis_path)
predictions <- credit_analysis$predictions

clip_probability <- function(predicted, epsilon = 1e-15) {
  pmin(pmax(as.numeric(predicted), epsilon), 1 - epsilon)
}

log_loss <- function(actual, predicted) {
  predicted <- clip_probability(predicted)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

auc_score <- function(actual, predicted) {
  n_positive <- sum(actual == 1L)
  n_negative <- sum(actual == 0L)
  ranks <- rank(predicted, ties.method = "average")

  (
    sum(ranks[actual == 1L]) - n_positive * (n_positive + 1) / 2
  ) / (n_positive * n_negative)
}

# The same threshold table feeds ROC and KS.
threshold_curve <- predictions |>
  dplyr::group_by(model) |>
  dplyr::group_modify(function(data, key) {
    n_positive <- sum(data$status_bad == 1L)
    n_negative <- sum(data$status_bad == 0L)

    curve <- data |>
      dplyr::group_by(score) |>
      dplyr::summarise(
        positives = sum(status_bad == 1L),
        negatives = sum(status_bad == 0L),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::mutate(
        true_positive_rate = cumsum(positives) / n_positive,
        false_positive_rate = cumsum(negatives) / n_negative,
        ks_gap = true_positive_rate - false_positive_rate,
        threshold = score
      ) |>
      dplyr::select(
        threshold, true_positive_rate, false_positive_rate, ks_gap
      )

    dplyr::bind_rows(
      tibble::tibble(
        threshold = Inf,
        true_positive_rate = 0,
        false_positive_rate = 0,
        ks_gap = 0
      ),
      curve
    )
  }) |>
  dplyr::ungroup()

# The same ordered table feeds cumulative gains, Lorenz-style views and lift.
gains_curve <- predictions |>
  dplyr::group_by(model) |>
  dplyr::group_modify(function(data, key) {
    ordered <- data |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::mutate(
        population_fraction = dplyr::row_number() / dplyr::n(),
        positive_fraction = cumsum(status_bad) / sum(status_bad),
        lift = positive_fraction / population_fraction
      ) |>
      dplyr::select(
        row_id, score, population_fraction, positive_fraction, lift
      )

    dplyr::bind_rows(
      tibble::tibble(
        row_id = NA_integer_,
        score = Inf,
        population_fraction = 0,
        positive_fraction = 0,
        lift = NA_real_
      ),
      ordered
    )
  }) |>
  dplyr::ungroup()

evaluation_summary <- purrr::map_dfr(
  unique(predictions$model),
  function(model_name) {
    model_predictions <- predictions |>
      dplyr::filter(model == model_name)
    model_thresholds <- threshold_curve |>
      dplyr::filter(model == model_name)
    ks_row <- model_thresholds |>
      dplyr::slice_max(ks_gap, n = 1, with_ties = FALSE)
    auc <- auc_score(
      model_predictions$status_bad,
      model_predictions$score
    )

    tibble::tibble(
      model = model_name,
      sample = "test",
      log_loss = log_loss(
        model_predictions$status_bad,
        model_predictions$score
      ),
      auc = auc,
      gini = 2 * auc - 1,
      ks = ks_row$ks_gap,
      ks_threshold = ks_row$threshold
    )
  }
)

credit_analysis$threshold_curve <- threshold_curve
credit_analysis$gains_curve <- gains_curve
credit_analysis$evaluation_summary <- evaluation_summary
credit_analysis$metadata$evaluation <- list(
  sample = "test",
  positive_class = 1L,
  score_direction = "higher score means higher probability of bad"
)

saveRDS(credit_analysis, analysis_path, compress = "gzip")
cli::cli_success("Added evaluation results to {.path {analysis_path}}.")
