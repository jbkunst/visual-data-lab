# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(markdown)
library(highcharter)

# data --------------------------------------------------------------------
app_dir <- if (file.exists("credit-effects.rds")) "." else "variable-effects"
effects_data <- readRDS(file.path(app_dir, "credit-effects.rds"))

model_labels <- effects_data$metadata$model_labels
variable_labels <- c(
  seniority = "Seniority", amount = "Loan amount", income = "Income",
  assets = "Assets", price = "Price", expenses = "Expenses",
  time = "Loan term", age = "Age", debt = "Debt"
)

# Mean absolute SHAP order from the shared credit-analysis sample.
variable_order <- names(variable_labels)

set.seed(2026L)
ice_rows <- sample(unique(effects_data$ice_values$row_id), 80L)

# chart helpers -----------------------------------------------------------
effect_chart <- function(x_title, y_title, reference = NULL) {
  plot_lines <- if (!is.null(reference)) {
    list(list(value = reference, color = "#dee2e6", width = 1, zIndex = 3))
  }

  highchart() |>
    hc_chart(type = "line") |>
    hc_xAxis(title = list(text = x_title)) |>
    hc_yAxis(title = list(text = y_title), plotLines = plot_lines) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(shared = TRUE, valueDecimals = 3)
}

xy_points <- function(x, y) list_parse2(data.frame(x, y))

# ui ---------------------------------------------------------------------
apptheme <- bs_theme()
sidebar <- purrr::partial(sidebar, width = 300)
card <- purrr::partial(
  card, full_screen = TRUE,
  wrapper = purrr::partial(card_body, padding = 0)
)

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
      title = "Variable Effects",
      radioButtons(
        "model", tags$small("Model"),
        choices = stats::setNames(names(model_labels), model_labels),
        selected = "logistic"
      ),
      selectInput(
        "variable", tags$small("Variable"),
        choices = stats::setNames(variable_order, variable_labels[variable_order]),
        selected = variable_order[[1L]]
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
      col_widths = c(6, 6, 6, 6),
      row_heights = c(1, 1),
      gap = "0.75rem",
      card(card_header("Partial dependence (PDP)"),
        highchartOutput("pdp_plot", height = "100%")),
      card(card_header(
        class = "d-flex justify-content-between align-items-center",
        "Accumulated local effects (ALE)",
        input_switch(
          "ale_pd_scale", tags$small("PD scale"),
          value = TRUE, width = "auto"
        )
      ),
        highchartOutput("ale_plot", height = "100%")),
      card(card_header("Individual conditional expectation (ICE)"),
        highchartOutput("ice_plot", height = "100%")),
      card(card_header("Observed distribution"),
        highchartOutput("distribution_plot", height = "100%"))
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  selected_effects <- reactive({
    ice <- effects_data$ice_values
    ice <- ice[ice$model == input$model & ice$variable == input$variable, ]
    ale <- effects_data$ale_values
    ale <- ale[ale$model == input$model & ale$variable == input$variable, ]
    pdp <- stats::aggregate(estimate ~ x, ice, mean)

    list(
      ice = ice[ice$row_id %in% ice_rows, ],
      pdp = pdp,
      ale = ale,
      baseline = unname(effects_data$baseline[[input$model]]),
      observed = effects_data$test[[input$variable]],
      x_title = unname(variable_labels[[input$variable]])
    )
  })

  output$pdp_plot <- renderHighchart({
    data <- selected_effects()

    effect_chart(data$x_title, "Predicted probability of default") |>
      hc_add_series(
        name = "PDP", data = xy_points(data$pdp$x, data$pdp$estimate),
        lineWidth = 3,
        marker = list(enabled = TRUE, radius = 3)
      )
  })

  output$ale_plot <- renderHighchart({
    data <- selected_effects()
    pd_scale <- isTRUE(input$ale_pd_scale)
    estimate <- data$ale$estimate + if (pd_scale) data$baseline else 0
    ale <- data.frame(
      x = data$ale$x,
      estimate = if (pd_scale) estimate else 100 * estimate
    )
    y_title <- if (pd_scale) {
      "ALE-adjusted probability of default"
    } else {
      "Centered effect on PD (pp)"
    }
    reference <- if (pd_scale) data$baseline else 0

    effect_chart(data$x_title, y_title, reference = reference) |>
      hc_add_series(
        name = "ALE", data = xy_points(ale$x, ale$estimate),
        lineWidth = 3,
        marker = list(enabled = TRUE, radius = 3)
      )
  })

  output$ice_plot <- renderHighchart({
    data <- selected_effects()
    series <- unname(lapply(split(data$ice, data$ice$row_id), function(curve) {
      list(
        type = "line", data = xy_points(curve$x, curve$estimate),
        color = "rgba(224, 224, 224, 0.55)", lineWidth = 1,
        marker = list(enabled = FALSE), enableMouseTracking = FALSE,
        showInLegend = FALSE, zIndex = 1
      )
    }))

    effect_chart(data$x_title, "Predicted probability of default") |>
      hc_add_series_list(series) |>
      hc_add_series(
        name = "PDP", data = xy_points(data$pdp$x, data$pdp$estimate),
        lineWidth = 3, marker = list(enabled = FALSE),
        zIndex = 2
      )
  })

  output$distribution_plot <- renderHighchart({
    data <- selected_effects()

    hchart(
      data$observed, breaks = 28, name = "Clients", color = "#e0e0e0"
    ) |>
      hc_xAxis(title = list(text = data$x_title)) |>
      hc_yAxis(title = list(text = "Clients"), min = 0) |>
      hc_legend(enabled = FALSE) |>
      hc_tooltip(valueDecimals = 0) |>
      hc_plotOptions(column = list(borderWidth = 0, groupPadding = 0, pointPadding = 0))
  })
}

shinyApp(ui, server)
