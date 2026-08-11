# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(markdown)
library(highcharter)

# data --------------------------------------------------------------------
app_dir <- if (file.exists("credit-importance.rds")) "." else "global-feature-importance"
importance_data <- readRDS(file.path(app_dir, "credit-importance.rds"))

model_labels <- importance_data$metadata$model_labels
variable_labels <- c(
  seniority = "Seniority", amount = "Loan amount", income = "Income",
  assets = "Assets", price = "Price", expenses = "Expenses",
  time = "Loan term", age = "Age", debt = "Debt"
)
variable_order <- names(variable_labels)

# chart helpers -----------------------------------------------------------
summarize_importance <- function(values, method, metric, scale = 1) {
  selected <- values[values$method == method & values$metric == metric, ]
  summary <- stats::aggregate(importance ~ variable, selected, mean)
  summary$importance <- scale * summary$importance
  summary
}

importance_points <- function(summary) {
  values <- stats::setNames(summary$importance, summary$variable)[variable_order]
  unname(Map(function(variable, value) {
    list(
      name = unname(variable_labels[[variable]]), y = unname(value),
      color = if (value >= 0) primary_color else danger_color
    )
  }, variable_order, values))
}

importance_chart <- function(points, axis_title, series_id, decimals = 4) {
  highchart() |>
    hc_chart(type = "bar") |>
    hc_xAxis(
      categories = unname(variable_labels[variable_order]),
      reversed = TRUE
    ) |>
    hc_yAxis(
      title = list(text = axis_title),
      plotLines = list(list(value = 0, color = "#dee2e6", width = 1, zIndex = 3))
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(pointFormat = paste0("{point.y:.", decimals, "f}")) |>
    hc_plotOptions(series = list(
      animation = list(duration = 300), borderWidth = 0,
      dataLabels = list(enabled = FALSE)
    )) |>
    hc_add_series(id = series_id, name = "Importance", data = points)
}

sage_decomposition_chart <- function(decomposition) {
  highchart() |>
    hc_add_dependency("modules/waterfall.js") |>
    hc_chart(type = "waterfall") |>
    hc_xAxis(type = "category", categories = decomposition$categories) |>
    hc_yAxis(title = list(text = decomposition$axis_title)) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(pointFormat = "{point.y:.4f}") |>
    hc_plotOptions(series = list(
      animation = list(duration = 300), borderWidth = 0,
      dataLabels = list(
        enabled = TRUE, inside = FALSE,
        style = list(color = "#495057", fontWeight = "normal", textOutline = "none"),
        formatter = JS(paste(
          "function () {",
          "  if (this.point.isSum || this.point.index === 0)",
          "    return Highcharts.numberFormat(this.y, 4);",
          "  return null;",
          "}"
        ))
      )
    )) |>
    hc_add_series(
      id = "sage_decomposition", name = "Loss",
      data = decomposition$points
    )
}

# ui ---------------------------------------------------------------------
apptheme <- bs_theme()
primary_color <- unname(bs_get_variables(apptheme, "primary"))
danger_color <- unname(bs_get_variables(apptheme, "danger"))
sidebar <- purrr::partial(sidebar, width = 300)
card <- purrr::partial(card, full_screen = TRUE,
  wrapper = purrr::partial(card_body, padding = 0))

options(
  highcharter.theme = hc_theme(
    chart = list(style = list(fontFamily = "system-ui")),
    legend = list(itemStyle = list(fontWeight = "normal")),
    colors = unname(bs_get_variables(
      apptheme,
      c("primary", "danger", "warning", "success", "info", "secondary")
    )),
    tooltip = list(valueDecimals = 3, shared = TRUE,
      style = list(fontWeight = "normal")),
    xAxis = list(
      gridLineWidth = 1,
      labels = list(style = list(fontWeight = "normal", textOutline = "none")),
      title = list(style = list(fontWeight = "normal"))
    ),
    yAxis = list(
      labels = list(style = list(fontWeight = "normal", textOutline = "none")),
      title = list(style = list(fontWeight = "normal"))
    )
  )
)

ui <- page_fillable(
  theme = apptheme, padding = 0,
  layout_sidebar(
    fillable = TRUE, padding = "0.75rem",
    sidebar = sidebar(
      title = "Global Feature Importance",
      radioButtons(
        "model", tags$small("Model"),
        choices = stats::setNames(names(model_labels), model_labels),
        selected = "logistic"
      ),
      selectInput(
        "permutation_metric", tags$small("Permutation metric"),
        choices = c("Log-loss" = "log_loss", "1 − AUC ROC" = "1_minus_auc_roc")
      ),
      selectInput(
        "sage_metric", tags$small("SAGE metric"),
        choices = c("Log-loss" = "log_loss", "1 − AUC ROC" = "1_minus_auc_roc")
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
      card(card_header("Permutation importance"),
        highchartOutput("permutation_plot", height = "100%")),
      card(card_header("Drop-column importance"),
        highchartOutput("drop_column_plot", height = "100%")),
      navset_card_tab(
        id = "sage_view", title = "SAGE", full_screen = TRUE,
        wrapper = purrr::partial(card_body, padding = 0),
        nav_panel("Importance", highchartOutput("sage_plot", height = "100%")),
        nav_panel(
          "Loss decomposition",
          highchartOutput("sage_decomposition_plot", height = "100%")
        )
      ),
      card(card_header("Global SHAP importance"),
        highchartOutput("shap_plot", height = "100%"))
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  chart_data <- reactive({
    values <- importance_data$importance_values
    values <- values[values$model == input$model, ]

    permutation_axis <- if (input$permutation_metric == "log_loss") {
      "Increase in log-loss"
    } else {
      "Increase in 1 − AUC ROC"
    }
    sage_axis <- if (input$sage_metric == "log_loss") {
      "Reduction in log-loss"
    } else {
      "Reduction in 1 − AUC ROC"
    }
    decomposition_axis <- if (input$sage_metric == "log_loss") {
      "Log-loss"
    } else {
      "1 − AUC ROC"
    }

    permutation <- summarize_importance(
      values, "permutation", input$permutation_metric
    )
    drop_column <- summarize_importance(values, "drop_column", "log_loss")
    sage <- summarize_importance(values, "sage", input$sage_metric)
    shap <- summarize_importance(
      values, "shap_global", "prediction_change", scale = 100
    )

    sage_rows <- values[
      values$method == "sage" & values$metric == input$sage_metric,
    ]
    empty_loss <- mean(sage_rows$loss_before[sage_rows$position == 1L])
    sage_values <- stats::setNames(sage$importance, sage$variable)[variable_order]
    decomposition_points <- unname(c(
      list(list(name = "No variables", y = empty_loss, color = "#e0e0e0")),
      Map(function(name, value) {
        list(name = name, y = unname(value),
          color = if (value <= 0) "#4c91d9" else "#d95f59")
      }, unname(variable_labels[variable_order]), -sage_values),
      list(list(name = "Full model", isSum = TRUE, color = "#34495e"))
    ))

    list(
      permutation = importance_points(permutation),
      permutation_axis = permutation_axis,
      drop_column = importance_points(drop_column),
      sage = importance_points(sage), sage_axis = sage_axis,
      shap = importance_points(shap),
      decomposition = list(
        points = decomposition_points,
        categories = c(
          "No variables", unname(variable_labels[variable_order]), "Full model"
        ),
        axis_title = decomposition_axis
      )
    )
  })

  output$permutation_plot <- renderHighchart({
    data <- isolate(chart_data())
    importance_chart(
      data$permutation, data$permutation_axis, "permutation"
    )
  })

  output$drop_column_plot <- renderHighchart({
    data <- isolate(chart_data())
    importance_chart(data$drop_column, "Increase in log-loss", "drop_column")
  })

  output$sage_plot <- renderHighchart({
    data <- isolate(chart_data())
    importance_chart(data$sage, data$sage_axis, "sage")
  })

  output$sage_decomposition_plot <- renderHighchart({
    sage_decomposition_chart(isolate(chart_data())$decomposition)
  })

  output$shap_plot <- renderHighchart({
    data <- isolate(chart_data())
    importance_chart(
      data$shap, "Mean absolute SHAP contribution (pp)", "shap",
      decimals = 2
    )
  })

  observeEvent(
    list(input$model, input$permutation_metric, input$sage_metric),
    {
      data <- chart_data()

      highchartProxy("permutation_plot") |>
        hcpxy_update(yAxis = list(title = list(text = data$permutation_axis))) |>
        hcpxy_update_series(id = "permutation", data = data$permutation)
      highchartProxy("drop_column_plot") |>
        hcpxy_update_series(id = "drop_column", data = data$drop_column)
      highchartProxy("sage_plot") |>
        hcpxy_update(yAxis = list(title = list(text = data$sage_axis))) |>
        hcpxy_update_series(id = "sage", data = data$sage)
      highchartProxy("sage_decomposition_plot") |>
        hcpxy_update(
          xAxis = list(categories = data$decomposition$categories),
          yAxis = list(title = list(text = data$decomposition$axis_title))
        ) |>
        hcpxy_update_series(
          id = "sage_decomposition", data = data$decomposition$points
        )
      highchartProxy("shap_plot") |>
        hcpxy_update_series(id = "shap", data = data$shap)
    },
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)
