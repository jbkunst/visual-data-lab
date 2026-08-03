# Run after defining model, x, background, and predict_model.
source("shap-explorer/local_shap.R")

comparison_background <- background |>
  dplyr::slice_head(n = 500)

seed <- 2026L

educational_time <- system.time({
  educational_trace <- local_shap_trace(
    model = model,
    x = x,
    background = comparison_background,
    seed = seed
  )
})

optimized_time <- system.time({
  optimized_trace <- local_shap_trace_optimized(
    model = model,
    x = x,
    background = comparison_background,
    seed = seed
  )
})

educational_shap <- summarize_shap(educational_trace)
optimized_shap <- summarize_shap(optimized_trace)

shap_comparison <- tibble::tibble(
  variable = names(educational_shap),
  educational = unname(educational_shap),
  optimized = unname(optimized_shap),
  difference = educational - optimized
)

same_permutations <- identical(
  as.character(educational_trace$variable),
  as.character(optimized_trace$variable)
)

maximum_difference <- c(
  probability = max(abs(
    educational_trace$probability - optimized_trace$probability
  )),
  diff = max(abs(
    educational_trace$diff - optimized_trace$diff
  )),
  shap = max(abs(
    educational_shap - optimized_shap
  ))
)

baseline <- mean(predict_model(model, comparison_background))
prediction <- predict_model(model, x)[[1]]
additivity_error <- c(
  educational = prediction - baseline - sum(educational_shap),
  optimized = prediction - baseline - sum(optimized_shap)
)

timings <- rbind(
  educational = educational_time,
  optimized = optimized_time
)

optimized_repetitions <- system.time({
  for (i in seq_len(100)) {
    local_shap_trace_optimized(
      model,
      x,
      comparison_background,
      seed = seed
    )
  }
})

shap_comparison
same_permutations
maximum_difference
additivity_error
timings
optimized_repetitions
