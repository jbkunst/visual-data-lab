# Run from the repository root: Rscript gini-inequality/tests.R
source("gini-inequality/calculations.R")
near <- function(x, y, tolerance = 1e-10) stopifnot(isTRUE(all.equal(x, y, tolerance = tolerance)))

# Population definition and independent pair-sum / Lorenz-area checks.
near(gini(rep(10, 10)), 0)
near(gini(c(rep(0, 9), 100)), 0.9)
stopifnot(is.na(gini(rep(0, 10))), nrow(lorenz(rep(0, 10))) == 0)
stopifnot(inherits(try(gini(c(-1, 2)), silent = TRUE), "try-error"))
x <- c(0, 2, 5, 10, 18, 30, 70)
near(gini(x), mean(abs(outer(x, x, "-"))) / (2 * mean(x)))
near(gini(x), gini(x * 1e300))
near(gini(x), gini(x * 1e-300))
l <- lorenz(x)
near(gini(x), 1 - sum(diff(l$p) * (head(l$L, -1) + tail(l$L, -1))))
near(sum(vapply(quantile_pairs(x), `[[`, numeric(1), "contribution")), gini(x))

for (family in c("lognormal", "gamma", "exponential", "beta")) {
  par <- switch(family, lognormal = list(mu = 0, sigma = 1),
    gamma = list(alpha = 2, beta = 2), exponential = list(lam = 1), beta = list(alpha = 2, beta = 2))
  d <- income_distribution(family, par)
  near(d$curve$L[c(1, nrow(d$curve))], c(0, 1))
  stopifnot(all(diff(d$curve$L) >= 0), all(d$curve$L <= d$curve$p + 1e-12))
  # Dense quantile quadrature independently verifies the analytic formulas.
  x <- d$quantile((seq_len(100000) - 0.5) / 100000)
  near(gini(x), d$gini, tolerance = 0.001)
  near(mean(x), d$mean, tolerance = 0.001)
}
near(income_distribution("gamma", list(alpha = 1, beta = 3))$gini, 0.5)
near(income_distribution("beta", list(alpha = 1, beta = 1))$gini, 1 / 3)
uniform <- income_distribution("beta", list(alpha = 1, beta = 1))
near(uniform$curve$L, uniform$curve$p^2)
concentrated <- income_distribution("beta", list(alpha = 100, beta = 100))
stopifnot(concentrated$gini < 0.05)
for (par in list(list(alpha = 0.5, beta = 0.5), list(alpha = 2, beta = 5), list(alpha = 5, beta = 2))) {
  d <- income_distribution("beta", par)
  near(gini(d$quantile((seq_len(100000) - 0.5) / 100000)), d$gini, tolerance = 0.001)
}

setwd("gini-inequality")
app <- source("app.R")$value
shiny::testServer(server, {
  session$setInputs(mode = "Theoretical", family = "lognormal",
    dist_lognormal = list(family = "lognormal", mu = 0, sigma = 1), resample = 0)
  near(current_gini(), 2 * pnorm(1 / sqrt(2)) - 1)
  stopifnot(length(pairs()) == choose(50, 2), nrow(curve()) == 301)
  stopifnot(pairs()[[1]]$group1 == "0-2%", pairs()[[1]]$group2 == "2-4%")
  stopifnot(all(vapply(pairs(), function(p) p$x == as.integer(p$x) && p$y == as.integer(p$y), logical(1))))
  stopifnot(nzchar(output$distribution_input$html), nzchar(output$density_chart),
    nzchar(output$lorenz_chart), nzchar(output$pairs_chart), nzchar(output$quantiles_chart))
  initial_density_widget <- output$density_chart
  density_options <- jsonlite::fromJSON(initial_density_widget, simplifyVector = FALSE)$x$hc_opts
  stopifnot(density_options$series[[1]]$type == "area", density_options$series[[1]]$fillOpacity == 0.15)
  theoretical_curve <- curve()
  theoretical_pairs <- pairs()
  # Check actual proxy payloads, not only the server's reactive calculations.
  messages <- list()
  session$sendCustomMessage <- function(type, message) {
    messages[[length(messages) + 1L]] <<- list(type = type, message = message)
  }
  check_chart_updates <- function() {
    updates <- Filter(function(x) x$type == "updateSeries", messages)
    for (id in c("density_chart", "lorenz_chart", "pairs_chart", "quantiles_chart")) {
      stopifnot(any(vapply(updates, function(x) x$message$id == id, logical(1))))
    }
    density_updates <- Filter(function(x) x$message$id == "density_chart", updates)
    stopifnot(length(density_updates) == 1)
    density_update <- density_updates[[1]]$message$options
    stopifnot(density_update$type == if (input$mode == "Theoretical") "area" else "column",
      density_update$name == input$mode, identical(output$density_chart, initial_density_widget))
    near(density_update$data, density_data()$data)
    lorenz_update <- tail(Filter(function(x) x$message$id == "lorenz_chart", updates), 1)[[1]]$message$options
    pairs_update <- tail(Filter(function(x) x$message$id == "pairs_chart", updates), 1)[[1]]$message$options
    near(lorenz_update$data, xy_data(curve()$p, curve()$L))
    near(pairs_update$data, pairs())
    quantile_update <- tail(Filter(function(x) x$message$id == "quantiles_chart" && x$message$idSeries == "quantiles", updates), 1)[[1]]$message$options
    near(quantile_update$data, quantile_data()$observed)
    stopifnot(lorenz_update$name == curve_label())
    messages <<- list()
  }
  session$setInputs(mode = "Simulated")
  check_chart_updates()
  stopifnot(nrow(curve()) != nrow(theoretical_curve), !identical(pairs(), theoretical_pairs))
  chart <- jsonlite::fromJSON(output$lorenz_chart, simplifyVector = FALSE)$x$hc_opts
  stopifnot(chart$series[[1]]$id == "lorenz", chart$series[[2]]$name == "Equality")
  stopifnot(is.null(chart$title$text), is.null(chart$subtitle$text))
  first <- sample_income()
  stopifnot(length(first) == 1000)
  near(vapply(quantile_data()$observed, `[[`, numeric(1), 2), sort(first))
  near(current_gini(), gini(first))
  near(current_gini(), mean(abs(outer(first, first, "-"))) / (2 * mean(first)))
  session$setInputs(dist_lognormal = list(mu = 1, sigma = 1))
  check_chart_updates()
  near(sample_income(), first * exp(1))
  near(gini(first), current_gini())
  old_curve <- curve()
  old_pairs <- pairs()
  session$setInputs(resample = 1)
  check_chart_updates()
  stopifnot(!identical(old_curve, curve()), !identical(old_pairs, pairs()))
  session$setInputs(dist_lognormal = list(mu = 0, sigma = 1))
  stopifnot(!identical(first, sample_income()))
  session$setInputs(resample = 0)
  near(first, sample_income())
  session$setInputs(family = "gamma", dist_gamma = list(alpha = 2, beta = 2))
  gamma_sample <- sample_income()
  session$setInputs(dist_gamma = list(alpha = 2, beta = 4))
  near(sample_income(), gamma_sample / 2)
  near(gini(gamma_sample), current_gini())
  session$setInputs(dist_gamma = list(alpha = 5, beta = 4))
  stopifnot(distribution()$gini < income_distribution("gamma", list(alpha = 2, beta = 4))$gini)
  session$setInputs(family = "exponential", dist_exponential = list(lam = 1))
  near(distribution()$gini, 0.5)
  exp_sample <- sample_income()
  session$setInputs(dist_exponential = list(lam = 3))
  near(sample_income(), exp_sample / 3)
  near(gini(exp_sample), current_gini())
  stopifnot(nrow(curve()) == 1001, length(pairs()) == choose(50, 2),
    nzchar(output$density_chart), nzchar(output$lorenz_chart), nzchar(output$pairs_chart))
  messages <- list()
  session$setInputs(mode = "Theoretical")
  check_chart_updates()
  near(current_gini(), 0.5)
  near(quantile_data()$observed, quantile_data()$theoretical)
  stopifnot(nrow(curve()) == 301)
  session$setInputs(family = "beta", dist_beta = list(alpha = 2, beta = 5))
  near(distribution()$mean, 2 / 7)
  stopifnot(nzchar(output$distribution_input$html), nzchar(output$density_chart))
  session$setInputs(mode = "Simulated")
  stopifnot(all(sample_income() >= 0), all(sample_income() <= 1))
  near(current_gini(), gini(sample_income()))
  messages <- list()
  session$setInputs(dist_beta = list(alpha = 5, beta = 2))
  check_chart_updates()
  stopifnot(nzchar(output$lorenz_chart), nzchar(output$pairs_chart), nzchar(output$quantiles_chart))
})
cat("All distribution, Gini, quantile-grid and Shiny server checks passed.\n")
