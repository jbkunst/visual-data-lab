# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(highcharter)
library(tibble)
library(markdown)

# theme -------------------------------------------------------------------
apptheme <- bs_theme(primary = "#007BC2")

sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

# app options -------------------------------------------------------------
mean_negative_choices <- c("-4", "-3", "-2", "-1", "-0.5", "0")
mean_positive_choices <- c("0", "0.5", "1", "2", "3", "4")
sd_choices <- c("0.5", "1", "1.5", "2")
n_choices <- c("100", "500", "1000", "5000")
proportion_choices <- as.character(seq(10, 90, by = 10))

class_palette <- c(
  "Negative" = "#d98f8f",
  "Positive" = "#8f95d9"
)

region_palette <- c(
  "TN" = "#d98f8f",
  "FP" = "#c96f6f",
  "FN" = "#747bd0",
  "TP" = "#8f95d9"
)

region_labels <- c(
  "TN" = "True negative",
  "FP" = "False positive",
  "FN" = "False negative",
  "TP" = "True positive"
)

metric_labels <- c(
  "TPR" = "True positive rate",
  "FPR" = "False positive rate",
  "Specificity" = "Specificity",
  "Precision" = "Precision",
  "F1" = "F1 score",
  "Accuracy" = "Accuracy",
  "AUC" = "Area under the curve"
)

metric_descriptions <- c(
  "TPR" = "Among observed positives, the proportion correctly predicted as positive.",
  "FPR" = "Among observed negatives, the proportion incorrectly predicted as positive.",
  "Specificity" = "Among observed negatives, the proportion correctly predicted as negative.",
  "Precision" = "Among predicted positives, the proportion that is actually positive.",
  "F1" = "The harmonic mean of precision and true positive rate.",
  "Accuracy" = "The proportion of all observations classified correctly.",
  "AUC" = "The probability that a positive observation receives a higher score than a negative one."
)



threshold_color <- "#D9A441"

options(
  highcharter.theme = hc_theme_smpl(
    exporting = list(enabled = FALSE),
    credits = list(enabled = FALSE),
    plotOptions = list(
      series = list(
        dataLabels = list(style = list(fontWeight = "normal", textOutline = "none"))
      )
    )
  )
)

# helpers -----------------------------------------------------------------
safe_divide <- function(x, y) {
  if (y == 0) return(NA_real_)
  x / y
}

xy_data <- function(x, y, digits = 3) {
  Map(function(.x, .y) list(round(.x, digits), round(.y, digits)), x, y)
}

metric_chart_data <- function(data) {
  lapply(seq_len(nrow(data)), function(i) {
    metric <- data$metric[[i]]

    list(
      name = metric_labels[[metric]],
      y = round(data$value[[i]], 3),
      description = metric_descriptions[[metric]]
    )
  })
}



# development input -------------------------------------------------------
# input <- list(mean_1 = "-1", sd_1 = "1", mean_2 = "1", sd_2 = "1", threshold = 0, n = "500", p_1 = "50")

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "ROC Curve",

      shinyWidgets::radioGroupButtons(
        "mode",
        label = NULL,
        choices = c("Theoretical", "Simulated"),
        selected = "Theoretical",
        justified = TRUE,
        status = "primary"
      ),
      tags$small("Positive distribution"),
      layout_columns(
        col_widths = c(6, 6),
        shinyWidgets::sliderTextInput("mean_2", tags$small("Mean"), choices = mean_positive_choices, selected = "1", grid = FALSE, hide_min_max = TRUE, dragRange = FALSE),
        shinyWidgets::sliderTextInput("sd_2", tags$small("SD"), choices = sd_choices, selected = "1", grid = FALSE, hide_min_max = TRUE, dragRange = FALSE)
      ),

      tags$small("Negative distribution"),
      layout_columns(
        col_widths = c(6, 6),
        shinyWidgets::sliderTextInput("mean_1", tags$small("Mean"), choices = mean_negative_choices, selected = "-1", grid = FALSE, hide_min_max = TRUE, dragRange = FALSE),
        shinyWidgets::sliderTextInput("sd_1", tags$small("SD"), choices = sd_choices, selected = "1", grid = FALSE, hide_min_max = TRUE, dragRange = FALSE)
      ),

      sliderInput("threshold", tags$small("Threshold"), min = -5, max = 5, value = 0, step = 0.1, ticks = FALSE),
      shinyWidgets::sliderTextInput("n", tags$small("Number of observations"), choices = n_choices, selected = "1000", grid = FALSE, hide_min_max = TRUE, dragRange = FALSE),
      shinyWidgets::sliderTextInput("p_1", tags$small("Positive proportion"), choices = proportion_choices, selected = "50", grid = FALSE, hide_min_max = TRUE, dragRange = FALSE, post = "%"),
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
      col_widths = c(6, 6, 6, 6),
      row_heights = c(1, 0.72),
      card(
        card_header("Score distributions"),
        card_body(highchartOutput("distribution_chart", height = "100%"))
      ),
      card(
        card_header("ROC curve"),
        card_body(highchartOutput("roc_chart", height = "100%"))
      ),
      card(
        card_header("Confusion matrix"),
        card_body(highchartOutput("confusion_chart", height = "100%"))
      ),
      card(
        card_header("Metrics at threshold"),
        card_body(highchartOutput("metrics_chart", height = "100%"))
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  data_sample <- reactive({
    set.seed(1)

    n <- as.integer(input$n[[1]])
    p_positive <- as.numeric(input$p_1[[1]]) / 100
    n_positive <- round(n * p_positive)
    n_negative <- n - n_positive

    tibble(
      score = c(
        rnorm(n_negative, as.numeric(input$mean_1[[1]]), as.numeric(input$sd_1[[1]])),
        rnorm(n_positive, as.numeric(input$mean_2[[1]]), as.numeric(input$sd_2[[1]]))
      ),
      observed = factor(
        c(rep("Negative", n_negative), rep("Positive", n_positive)),
        levels = names(class_palette)
      )
    )
  })
  density_data <- reactive({
    threshold <- as.numeric(input$threshold)

    if (input$mode == "Simulated") {
      data <- data_sample()
      score_limits <- range(data$score)
      padding <- diff(score_limits) * 0.05
      score_grid <- seq(
        min(score_limits[[1]] - padding, threshold),
        max(score_limits[[2]] + padding, threshold),
        length.out = 300
      )
      score_grid <- sort(unique(c(score_grid, threshold)))

      densities <- do.call(
        rbind,
        lapply(names(class_palette), function(observed) {
          scores <- data$score[data$observed == observed]
          estimate <- density(scores, from = min(score_grid), to = max(score_grid), n = 512)
          tibble(
            score = score_grid,
            observed = factor(observed, levels = names(class_palette)),
            density = approx(estimate$x, estimate$y, xout = score_grid)$y * length(scores) / nrow(data)
          )
        })
      )
    } else {
      mean_1 <- as.numeric(input$mean_1[[1]])
      mean_2 <- as.numeric(input$mean_2[[1]])
      sd_1 <- as.numeric(input$sd_1[[1]])
      sd_2 <- as.numeric(input$sd_2[[1]])
      p_positive <- as.numeric(input$p_1[[1]]) / 100

      score_grid <- seq(
        min(mean_1 - 4 * sd_1, mean_2 - 4 * sd_2, threshold),
        max(mean_1 + 4 * sd_1, mean_2 + 4 * sd_2, threshold),
        length.out = 300
      )
      score_grid <- sort(unique(c(score_grid, threshold)))

      densities <- tibble(
        score = rep(score_grid, 2),
        observed = factor(rep(names(class_palette), each = length(score_grid)), levels = names(class_palette))
      )
      densities$density <- ifelse(
        densities$observed == "Negative",
        dnorm(densities$score, mean_1, sd_1) * (1 - p_positive),
        dnorm(densities$score, mean_2, sd_2) * p_positive
      )
    }

    densities$region <- ifelse(
      densities$observed == "Negative" & densities$score < threshold,
      "TN",
      ifelse(
        densities$observed == "Negative",
        "FP",
        ifelse(densities$score < threshold, "FN", "TP")
      )
    )

    densities
  })

  roc_data <- reactive({
    if (input$mode == "Simulated") {
      data <- data_sample()
      data <- data[order(data$score, decreasing = TRUE), , drop = FALSE]
      positive <- data$observed == "Positive"

      return(tibble(
        threshold = c(Inf, data$score),
        fpr = c(0, cumsum(!positive) / sum(!positive)),
        tpr = c(0, cumsum(positive) / sum(positive))
      ))
    }

    mean_1 <- as.numeric(input$mean_1[[1]])
    mean_2 <- as.numeric(input$mean_2[[1]])
    sd_1 <- as.numeric(input$sd_1[[1]])
    sd_2 <- as.numeric(input$sd_2[[1]])
    threshold <- seq(
      max(mean_1 + 5 * sd_1, mean_2 + 5 * sd_2),
      min(mean_1 - 5 * sd_1, mean_2 - 5 * sd_2),
      length.out = 300
    )

    tibble(
      threshold = threshold,
      fpr = 1 - pnorm(threshold, mean_1, sd_1),
      tpr = 1 - pnorm(threshold, mean_2, sd_2)
    )
  })

  confusion <- reactive({
    threshold <- as.numeric(input$threshold)

    if (input$mode == "Simulated") {
      data <- data_sample()
      predicted_positive <- data$score >= threshold
      observed_positive <- data$observed == "Positive"

      tn <- sum(!predicted_positive & !observed_positive)
      fp <- sum(predicted_positive & !observed_positive)
      fn <- sum(!predicted_positive & observed_positive)
      tp <- sum(predicted_positive & observed_positive)
      n <- nrow(data)
    } else {
      n <- as.integer(input$n[[1]])
      p_positive <- as.numeric(input$p_1[[1]]) / 100
      mean_1 <- as.numeric(input$mean_1[[1]])
      mean_2 <- as.numeric(input$mean_2[[1]])
      sd_1 <- as.numeric(input$sd_1[[1]])
      sd_2 <- as.numeric(input$sd_2[[1]])
      n_positive <- round(n * p_positive)
      n_negative <- n - n_positive

      tn <- round(n_negative * pnorm(threshold, mean_1, sd_1))
      fp <- n_negative - tn
      fn <- round(n_positive * pnorm(threshold, mean_2, sd_2))
      tp <- n_positive - fn
    }

    tibble(
      cell = c("TN", "FP", "FN", "TP"),
      observed = c("Negative", "Negative", "Positive", "Positive"),
      predicted = c("Negative", "Positive", "Negative", "Positive"),
      value = c(tn, fp, fn, tp),
      percent = c(tn, fp, fn, tp) / n
    )
  })

  metrics <- reactive({
    cm <- confusion()

    tp <- cm$value[cm$cell == "TP"]
    fp <- cm$value[cm$cell == "FP"]
    tn <- cm$value[cm$cell == "TN"]
    fn <- cm$value[cm$cell == "FN"]

    tpr <- safe_divide(tp, tp + fn)
    fpr <- safe_divide(fp, fp + tn)
    precision <- safe_divide(tp, tp + fp)
    accuracy <- safe_divide(tp + tn, tp + tn + fp + fn)
    specificity <- safe_divide(tn, tn + fp)
    f1 <- safe_divide(2 * precision * tpr, precision + tpr)

    if (input$mode == "Simulated") {
      roc <- roc_data()
      auc <- sum(diff(roc$fpr) * (head(roc$tpr, -1) + tail(roc$tpr, -1)) / 2)
    } else {
      mean_1 <- as.numeric(input$mean_1[[1]])
      mean_2 <- as.numeric(input$mean_2[[1]])
      sd_1 <- as.numeric(input$sd_1[[1]])
      sd_2 <- as.numeric(input$sd_2[[1]])
      auc <- pnorm((mean_2 - mean_1) / sqrt(sd_1^2 + sd_2^2))
    }

    tibble(
      metric = c("TPR", "FPR", "Specificity", "Precision", "F1", "Accuracy", "AUC"),
      value = c(tpr, fpr, specificity, precision, f1, accuracy, auc)
    )
  })

  output$distribution_chart <- renderHighchart({
    density <- isolate(density_data())
    threshold <- isolate(as.numeric(input$threshold))
    y_max <- max(density$density)

    hc <- highchart() |>
      hc_chart(type = "area", spacing = c(8, 8, 8, 8)) |>
      hc_title(text = NULL) |>
      hc_xAxis(title = list(text = "Score")) |>
      hc_yAxis(title = list(text = ""), min = 0, max = y_max * 1.08) |>
      hc_tooltip(shared = TRUE, valueDecimals = 3) |>
      hc_plotOptions(
        area = list(marker = list(enabled = FALSE), lineWidth = 0, stacking = NULL),
        line = list(marker = list(enabled = FALSE), lineWidth = 2)
      ) |>
      hc_legend(align = "center", verticalAlign = "bottom")

    for (region in c("TN", "FP", "FN", "TP")) {
      region_data <- density[density$region == region, , drop = FALSE]
      hc <- hc |>
        hc_add_series(
          id = region,
          name = region_labels[[region]],
          type = "area",
          data = xy_data(region_data$score, region_data$density),
          color = region_palette[[region]],
          enableMouseTracking = FALSE
        )
    }


    hc |>
      hc_add_series(
        id = "threshold",
        name = "Threshold",
        type = "line",
        data = xy_data(c(threshold, threshold), c(0, y_max * 1.05)),
        color = threshold_color,
        dashStyle = "ShortDash",
        marker = list(enabled = FALSE),
        enableMouseTracking = FALSE,
        zIndex = 5
      )
  })

  output$roc_chart <- renderHighchart({
    roc <- isolate(roc_data())
    current <- isolate(metrics())
    current_fpr <- current$value[current$metric == "FPR"]
    current_tpr <- current$value[current$metric == "TPR"]

    highchart() |>
      hc_chart(type = "line", spacing = c(8, 8, 8, 8)) |>
      hc_title(text = NULL) |>
      hc_xAxis(title = list(text = "False positive rate"), min = 0, max = 1) |>
      hc_yAxis(title = list(text = "True positive rate"), min = 0, max = 1) |>
      hc_add_series(
        name = "Random classifier",
        type = "line",
        data = xy_data(c(0, 1), c(0, 1)),
        color = "#b8bec6",
        dashStyle = "ShortDot",
        marker = list(enabled = FALSE),
        enableMouseTracking = FALSE
      ) |>
      hc_add_series(
        id = "roc",
        name = "ROC",
        type = "line",
        data = xy_data(roc$fpr, roc$tpr),
        color = "#4f56b3",
        marker = list(enabled = FALSE),
        lineWidth = 2.5
      ) |>
      hc_add_series(
        id = "current",
        name = "Current threshold",
        type = "scatter",
        data = xy_data(current_fpr, current_tpr),
        color = threshold_color,
        marker = list(radius = 6, symbol = "circle")
      ) |>
      hc_tooltip(valueDecimals = 3) |>
      hc_legend(align = "center", verticalAlign = "bottom")
  })

  output$confusion_chart <- renderHighchart({
    cm <- confusion()
    n <- sum(cm$value)

    heatmap_data <- list(
      list(x = 0, y = 0, value = cm$value[cm$cell == "TN"], percent = 100 * cm$percent[cm$cell == "TN"], name = region_labels[["TN"]], color = region_palette[["TN"]]),
      list(x = 1, y = 0, value = cm$value[cm$cell == "FP"], percent = 100 * cm$percent[cm$cell == "FP"], name = region_labels[["FP"]], color = region_palette[["FP"]]),
      list(x = 0, y = 1, value = cm$value[cm$cell == "FN"], percent = 100 * cm$percent[cm$cell == "FN"], name = region_labels[["FN"]], color = region_palette[["FN"]]),
      list(x = 1, y = 1, value = cm$value[cm$cell == "TP"], percent = 100 * cm$percent[cm$cell == "TP"], name = region_labels[["TP"]], color = region_palette[["TP"]])
    )

    highchart() |>
      hc_chart(type = "heatmap", spacing = c(8, 8, 8, 8)) |>
      hc_title(text = NULL) |>
      hc_xAxis(categories = c("Predicted<br>negative", "Predicted<br>positive"), title = list(text = NULL), opposite = TRUE, labels = list(useHTML = TRUE)) |>
      hc_yAxis(categories = c("Observed<br>negative", "Observed<br>positive"), title = list(text = ""), reversed = TRUE, labels = list(useHTML = TRUE)) |>
      hc_add_series(
        name = "Confusion matrix",
        type = "heatmap",
        data = heatmap_data,
        borderWidth = 2,
        borderColor = "#ffffff",
        dataLabels = list(enabled = TRUE, format = "{point.name}<br>{point.value}<br>{point.percent:.1f}%")
      ) |>
      hc_tooltip(pointFormat = "<b>{point.name}</b><br>Count: {point.value}<br>Share: {point.percent:.1f}%") |>
      hc_legend(enabled = FALSE) |>
      hc_colorAxis(enabled = FALSE)
  })

  output$metrics_chart <- renderHighchart({
    data <- isolate(metrics())

    highchart() |>
      hc_chart(type = "bar", spacing = c(8, 8, 8, 8)) |>
      hc_title(text = NULL) |>
      hc_xAxis(title = list(text = ""), categories = unname(metric_labels[data$metric]), title = list(text = NULL)) |>
      hc_yAxis(title = list(text = ""), min = 0, max = 1) |>
      hc_add_series(
        id = "metrics",
        name = "Value",
        data = metric_chart_data(data),
        color = "#007BC2",
        dataLabels = list(enabled = TRUE, format = "{point.y:.3f}")
      ) |>
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<span>{point.name}</span><br><span style='display:block;width:220px;white-space:normal'>{point.description}</span><br>Value: {point.y:.3f}"
      ) |>
      hc_legend(enabled = FALSE)
  })

  observeEvent(
    list(input$mode, input$mean_1, input$sd_1, input$mean_2, input$sd_2, input$p_1, input$n),
    {
      density <- density_data()
      y_max <- max(density$density)
      distribution_proxy <- highchartProxy("distribution_chart")

      distribution_proxy |>
        hcpxy_update(yAxis = list(max = y_max * 1.08))

      for (region in names(region_palette)) {
        region_data <- density[density$region == region, , drop = FALSE]

        distribution_proxy |>
          hcpxy_update_series(
            id = region,
            data = xy_data(region_data$score, region_data$density)
          )
      }


      distribution_proxy |>
        hcpxy_update_point(id = "threshold", id_point = 0, x = input$threshold, y = 0) |>
        hcpxy_update_point(id = "threshold", id_point = 1, x = input$threshold, y = y_max * 1.05)

      roc <- roc_data()

      highchartProxy("roc_chart") |>
        hcpxy_update_series(
          id = "roc",
          data = xy_data(roc$fpr, roc$tpr)
        )

      current <- metrics()

      highchartProxy("roc_chart") |>
        hcpxy_update_point(
          id = "current",
          id_point = 0,
          x = current$value[current$metric == "FPR"],
          y = current$value[current$metric == "TPR"]
        )

      highchartProxy("metrics_chart") |>
        hcpxy_update_series(
          id = "metrics",
          data = metric_chart_data(current)
        )
    },
    ignoreInit = TRUE
  )
  observeEvent(input$threshold, {
    density <- density_data()
    y_max <- max(density$density)
    distribution_proxy <- highchartProxy("distribution_chart")

    for (region in names(region_palette)) {
      region_data <- density[density$region == region, , drop = FALSE]

      distribution_proxy |>
        hcpxy_update_series(
          id = region,
          data = xy_data(region_data$score, region_data$density)
        )
    }

    distribution_proxy |>
      hcpxy_update_point(
        id = "threshold",
        id_point = 0,
        x = input$threshold,
        y = 0
      ) |>
      hcpxy_update_point(
        id = "threshold",
        id_point = 1,
        x = input$threshold,
        y = y_max * 1.05
      )

    current <- metrics()

    highchartProxy("roc_chart") |>
      hcpxy_update_point(
        id = "current",
        id_point = 0,
        x = current$value[current$metric == "FPR"],
        y = current$value[current$metric == "TPR"]
      )

    highchartProxy("metrics_chart") |>
      hcpxy_update_series(
        id = "metrics",
        data = metric_chart_data(current)
      )
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)