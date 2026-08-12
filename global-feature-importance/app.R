# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(markdown)
library(highcharter)
library(vdltheme)

# data --------------------------------------------------------------------
app_dir <- if (file.exists("credit-importance.rds")) "." else "global-feature-importance"
importance_data <- readRDS(file.path(app_dir, "credit-importance.rds"))
model_labels <- importance_data$metadata$model_labels

variable_labels <- c(
  seniority = "Seniority", amount = "Loan amount", income = "Income",
  assets = "Assets", price = "Price", expenses = "Expenses",
  time = "Loan term", age = "Age", debt = "Debt"
)

metric_labels <- c(
  auc = "AUC ROC", gini = "Gini", log_loss = "Log-loss", ks = "KS"
)

# chart helpers -----------------------------------------------------------
xy_points <- function(x, y) list_parse2(data.frame(x, y))

metric_spec <- function(metric) {
  switch(
    metric,
    auc = list(loss = "1_minus_auc_roc", transform = function(x) 1 - x,
      contribution = function(x) x, higher = TRUE),
    gini = list(loss = "1_minus_auc_roc", transform = function(x) 2 * (1 - x) - 1,
      contribution = function(x) 2 * x, higher = TRUE),
    log_loss = list(loss = "log_loss", transform = identity,
      contribution = function(x) -x, higher = FALSE),
    ks = list(loss = "1_minus_ks", transform = function(x) 1 - x,
      contribution = function(x) x, higher = TRUE)
  )
}

permutation_data <- function(model, metric) {
  spec <- metric_spec(metric)
  values <- importance_data$importance_values
  values <- values[
    values$model == model & values$method == "permutation" &
      values$metric == spec$loss,
  ]
  summary <- stats::aggregate(
    cbind(loss_before, loss_after, importance) ~ variable,
    values, mean
  )
  summary$model_quality <- spec$transform(summary$loss_before)
  summary$permuted_quality <- spec$transform(summary$loss_after)
  summary$deterioration <- if (spec$higher) {
    summary$model_quality - summary$permuted_quality
  } else {
    summary$permuted_quality - summary$model_quality
  }
  summary <- summary[order(summary$deterioration, decreasing = TRUE), ]
  quality_range <- range(c(summary$model_quality, summary$permuted_quality))
  padding <- max(diff(quality_range) * 0.3, 0.03)

  list(
    categories = unname(variable_labels[summary$variable]),
    points = lapply(seq_len(nrow(summary)), function(i) {
      list(
        y = summary$permuted_quality[[i]],
        modelQuality = summary$model_quality[[i]],
        deterioration = summary$deterioration[[i]],
        color = primary_color
      )
    }),
    model_quality = summary$model_quality[[1]],
    axis_min = quality_range[[1]] - padding,
    axis_max = quality_range[[2]] + padding,
    higher = spec$higher
  )
}

permutation_chart <- function(data, metric) {
  highchart() |>
    hc_chart(type = "bar") |>
    hc_xAxis(categories = data$categories, reversed = TRUE) |>
    hc_yAxis(
      title = list(text = unname(metric_labels[[metric]])),
      min = data$axis_min, max = data$axis_max,
      startOnTick = FALSE, endOnTick = FALSE,
      plotLines = list(list(
        value = data$model_quality, color = "#34495e", width = 2, zIndex = 10
      ))
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(pointFormat = paste(
      "Full model: {point.modelQuality:.3f}<br/>",
      "After permutation: {point.y:.3f}<br/>",
      "Deterioration: {point.deterioration:.3f}"
    )) |>
    hc_plotOptions(series = list(
      animation = list(duration = 300), borderWidth = 0,
      threshold = data$model_quality
    )) |>
    hc_add_series(id = "permutation", name = "Permutation", data = data$points)
}

sage_data <- function(model, metric) {
  spec <- metric_spec(metric)
  values <- importance_data$importance_values
  values <- values[
    values$model == model & values$method == "sage" & values$metric == spec$loss,
  ]
  summary <- stats::aggregate(importance ~ variable, values, mean)
  summary <- summary[order(summary$importance, decreasing = TRUE), ]
  start <- spec$transform(mean(values$loss_before[values$position == 1L]))

  list(
    categories = c(
      "No variables", unname(variable_labels[summary$variable]), "Full model"
    ),
    points = unname(c(
      list(list(name = "No variables", y = start, color = "#e0e0e0")),
      Map(function(name, value) {
        effect <- spec$contribution(value)
        list(
          name = name, y = unname(effect),
          color = if (if (spec$higher) effect >= 0 else effect <= 0) primary_color else danger_color
        )
      }, unname(variable_labels[summary$variable]), summary$importance),
      list(list(name = "Full model", isSum = TRUE, color = "#34495e"))
    ))
  )
}

sage_chart <- function(data, metric) {
  axis <- list(title = list(text = unname(metric_labels[[metric]])))

  highchart() |>
    hc_add_dependency("modules/waterfall.js") |>
    hc_chart(type = "waterfall", spacingRight = 35) |>
    hc_xAxis(type = "category", categories = data$categories) |>
    hc_yAxis_multiples(
      axis,
      list(linkedTo = 0, opposite = TRUE, title = list(text = NULL))
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(pointFormat = "{point.y:.3f}") |>
    hc_plotOptions(series = list(
      animation = list(duration = 300), borderWidth = 0,
      dataLabels = list(
        enabled = TRUE, inside = FALSE,
        style = list(color = "#495057", fontWeight = "normal", textOutline = "none"),
        formatter = JS(paste(
          "function () {",
          "  if (this.point.isSum || this.point.index === 0)",
          "    return Highcharts.numberFormat(this.y, 3);",
          "  return null;",
          "}"
        ))
      )
    )) |>
    hc_add_series(id = "sage", name = "Loss", data = data$points)
}

log_loss_chart <- function(model) {
  values <- importance_data$diagnostics$log_loss_values
  validate(need(
    !is.null(values) && all(is.finite(values$individual_log_loss)),
    "Regenerate credit-importance.rds to view individual log-loss."
  ))
  values <- values[values$model == model, ]
  upper <- max(2, ceiling(stats::quantile(values$individual_log_loss, 0.99) * 2) / 2)
  breaks <- seq(0, upper, length.out = 21L)
  bin_width <- diff(breaks)[[1]]
  colors <- c(train = "#adb5bd", test = primary_color)
  means <- stats::aggregate(individual_log_loss ~ sample, values, mean)

  chart <- highchart() |>
    hc_chart(type = "column") |>
    hc_subtitle(
      text = sprintf("Final bin includes individual losses ≥ %.2f", tail(breaks, 2)[[1]]),
      align = "right"
    ) |>
    hc_xAxis(
      title = list(text = "Individual log-loss"),
      plotLines = lapply(seq_len(nrow(means)), function(i) {
        list(
          value = means$individual_log_loss[[i]],
          color = colors[[means$sample[[i]]]], width = 2,
          dashStyle = if (means$sample[[i]] == "train") "ShortDash" else "Solid",
          zIndex = 4
        )
      })
    ) |>
    hc_yAxis(title = list(text = "Share of observations"), min = 0) |>
    hc_tooltip(
      headerFormat = "<b>{series.name}</b><br/>",
      pointFormat = "Loss: {point.x:.2f}<br/>Share: {point.y:.1%}"
    ) |>
    hc_plotOptions(column = list(
      grouping = FALSE, borderWidth = 0, pointRange = bin_width,
      opacity = 0.55, animation = list(duration = 300)
    ))

  for (sample_name in names(colors)) {
    losses <- values$individual_log_loss[values$sample == sample_name]
    losses <- pmin(losses, upper - .Machine$double.eps^0.5)
    distribution <- hist(losses, breaks = breaks, plot = FALSE, include.lowest = TRUE)
    mean_loss <- means$individual_log_loss[means$sample == sample_name]
    chart <- chart |> hc_add_series(
      name = sprintf("%s (mean %.3f)", tools::toTitleCase(sample_name), mean_loss),
      data = xy_points(distribution$mids, distribution$counts / length(losses)),
      color = colors[[sample_name]], zIndex = if (sample_name == "test") 2 else 1
    )
  }

  chart
}

metric_chart <- function(model, metric) {
  diagnostics <- importance_data$diagnostics
  summary <- diagnostics$quality_summary
  selected_summary <- summary[summary$model == model, ]

  if (metric == "log_loss") {
    return(log_loss_chart(model))
  } else if (metric %in% c("auc", "ks")) {
    curves <- diagnostics$threshold_curves
    curves <- curves[curves$model == model, ]
    if (metric == "auc") {
      chart <- highchart() |>
        hc_chart(type = "line") |>
        hc_xAxis(title = list(text = "False-positive rate"), min = 0, max = 1) |>
        hc_yAxis(title = list(text = "True-positive rate"), min = 0, max = 1) |>
        hc_add_series(
          name = "Random", data = xy_points(c(0, 1), c(0, 1)),
          color = "#ced4da", dashStyle = "ShortDash", enableMouseTracking = FALSE
        )
      x_name <- "false_positive_rate"
      y_name <- "true_positive_rate"
    } else {
      curves <- curves[is.finite(curves$threshold), ]
      test_curve <- curves[curves$sample == "test", ]
      ks_row <- test_curve[which.max(test_curve$ks_gap), ]
      chart <- highchart() |>
        hc_chart(type = "line") |>
        hc_xAxis(
          title = list(text = "PD threshold"),
          plotLines = list(list(
            value = ks_row$threshold, color = "#adb5bd", width = 1,
            dashStyle = "ShortDash", zIndex = 3,
            label = list(
              text = sprintf("Test KS · %.3f", ks_row$ks_gap),
              rotation = 0, style = list(fontWeight = "normal")
            )
          ))
        ) |>
        hc_yAxis(title = list(text = "Cumulative rate"), min = 0, max = 1)
      x_name <- "threshold"
      y_name <- NULL
    }
  } else {
    curves <- diagnostics$gains_curves
    curves <- curves[curves$model == model, ]
    test_rate <- selected_summary$default_rate[selected_summary$sample == "test"]
    chart <- highchart() |>
      hc_chart(type = "line") |>
      hc_xAxis(title = list(text = "Population targeted"), min = 0, max = 1) |>
      hc_yAxis(title = list(text = "Defaults captured"), min = 0, max = 1) |>
      hc_add_series(
        name = "Random", data = xy_points(c(0, 1), c(0, 1)),
        color = "#ced4da", dashStyle = "ShortDash", enableMouseTracking = FALSE
      ) |>
      hc_add_series(
        name = "Perfect (test)",
        data = xy_points(c(0, test_rate, 1), c(0, 1, 1)),
        color = "#dee2e6", dashStyle = "ShortDot", enableMouseTracking = FALSE
      )
    x_name <- "population_fraction"
    y_name <- "positive_fraction"
  }

  samples <- c(train = "#adb5bd", test = primary_color)
  for (sample_name in names(samples)) {
    sample_data <- curves[curves$sample == sample_name, ]
    suffix <- if (sample_name == "train") "Train" else "Test"
    dash <- if (sample_name == "train") "ShortDash" else "Solid"

    if (metric == "ks") {
      chart <- chart |>
        hc_add_series(
          name = paste("Bad", suffix),
          data = xy_points(sample_data$threshold, 1 - sample_data$true_positive_rate),
          color = samples[[sample_name]], dashStyle = dash
        ) |>
        hc_add_series(
          name = paste("Good", suffix),
          data = xy_points(sample_data$threshold, 1 - sample_data$false_positive_rate),
          color = samples[[sample_name]], dashStyle = "ShortDot"
        )
    } else {
      metric_column <- if (metric == "log_loss") "log_loss" else "auc"
      metric_value <- selected_summary[[metric_column]][
        selected_summary$sample == sample_name
      ]
      if (metric == "gini") metric_value <- 2 * metric_value - 1
      chart <- chart |> hc_add_series(
        name = sprintf("%s (%.3f)", suffix, metric_value),
        data = xy_points(sample_data[[x_name]], sample_data[[y_name]]),
        color = samples[[sample_name]], dashStyle = dash
      )
    }
  }

  chart |>
    hc_tooltip(shared = TRUE, valueDecimals = 3) |>
    hc_plotOptions(series = list(
      animation = list(duration = 300), lineWidth = 2,
      marker = list(enabled = metric == "log_loss", symbol = "circle")
    ))
}

gains_chart <- function(selected_model) {
  curves <- importance_data$diagnostics$gains_curves
  curves <- curves[curves$sample == "test", ]
  chart <- highchart() |>
    hc_chart(type = "line") |>
    hc_xAxis(title = list(text = "Population targeted"), min = 0, max = 1) |>
    hc_yAxis(title = list(text = "Defaults captured"), min = 0, max = 1) |>
    hc_add_series(
      name = "Random", data = xy_points(c(0, 1), c(0, 1)),
      color = "#dee2e6", dashStyle = "ShortDash", enableMouseTracking = FALSE
    )

  for (model_name in names(model_labels)) {
    data <- curves[curves$model == model_name, ]
    selected <- identical(model_name, selected_model)
    chart <- chart |> hc_add_series(
      id = paste0("gains_", model_name), name = unname(model_labels[[model_name]]),
      data = xy_points(data$population_fraction, data$positive_fraction),
      color = if (selected) primary_color else "#ced4da",
      lineWidth = if (selected) 3 else 1,
      zIndex = if (selected) 3 else 1
    )
  }

  chart |>
    hc_tooltip(shared = TRUE, valueDecimals = 3) |>
    hc_plotOptions(series = list(
      animation = list(duration = 300), marker = list(enabled = FALSE)
    ))
}

# ui ---------------------------------------------------------------------
apptheme <- theme_vdl()
primary_color <- unname(bs_get_variables(apptheme, "primary"))
danger_color <- unname(bs_get_variables(apptheme, "danger"))
sidebar <- purrr::partial(sidebar, width = 300)
card <- purrr::partial(card, full_screen = TRUE,
  wrapper = purrr::partial(card_body, padding = 0))

options(highcharter.theme = highcharter_theme_vdl())

ui <- page_fillable(
  theme = apptheme, padding = 0,
  layout_sidebar(
    fillable = TRUE, padding = "0.75rem",
    sidebar = sidebar(
      title = "Model Quality Decomposition",
      radioButtons(
        "model", tags$small("Model"),
        choices = stats::setNames(names(model_labels), model_labels),
        selected = "logistic"
      ),
      selectInput(
        "quality_metric",
        input_label_vdl(
          "Model quality metric",
          "Controls permutation, SAGE, and the metric-specific diagnostic."
        ),
        choices = stats::setNames(names(metric_labels), metric_labels)
      ),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
        )
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    layout_columns(
      col_widths = c(6, 6, 6, 6), row_heights = c(1, 1), gap = "0.75rem",
      card(card_header(uiOutput("permutation_title")),
        highchartOutput("permutation_plot", height = "100%")),
      card(card_header(uiOutput("metric_title")),
        highchartOutput("metric_plot", height = "100%")),
      card(card_header("SAGE decomposition"),
        highchartOutput("sage_plot", height = "100%")),
      card(card_header("Cumulative gains · test"),
        highchartOutput("gains_plot", height = "100%"))
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  output$permutation_title <- renderUI({
    value <- permutation_data(input$model, input$quality_metric)$model_quality
    tagList(
      "Permutation loss",
      tags$small(
        sprintf("Full model · %.3f", value),
        class = "text-muted float-end"
      )
    )
  })

  output$metric_title <- renderUI({
    switch(
      input$quality_metric,
      auc = "ROC curve · train vs test",
      gini = "CAP / Lorenz curve · train vs test",
      log_loss = "Individual log-loss · train vs test",
      ks = "KS curves · train vs test"
    )
  })

  output$permutation_plot <- renderHighchart({
    permutation_chart(
      permutation_data(input$model, input$quality_metric),
      input$quality_metric
    )
  })

  output$metric_plot <- renderHighchart({
    metric_chart(input$model, input$quality_metric)
  })

  output$sage_plot <- renderHighchart({
    sage_chart(sage_data(input$model, input$quality_metric), input$quality_metric)
  })

  output$gains_plot <- renderHighchart({
    gains_chart(input$model)
  })
}

shinyApp(ui, server)
