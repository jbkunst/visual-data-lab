# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(highcharter)
library(markdown)
library(shinymodist)
library(vdltheme)

source("calculations.R", local = TRUE)

# theme -------------------------------------------------------------------
apptheme <- theme_vdl()
sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(bslib::card, full_screen = TRUE, min_height = 250,
  wrapper = purrr::partial(bslib::card_body, padding = 0))
options(highcharter.theme = highcharter_theme_vdl())

xy_data <- function(x, y) Map(function(x, y) list(x, y), x, y)
grid_p <- (seq_len(50) - 0.5) / 50
quantile_p <- (seq_len(1000) - 0.5) / 1000

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme, title = "Income Inequality", padding = 0,
  layout_sidebar(
    fillable = TRUE, padding = "0.75rem",
    sidebar = sidebar(
      title = "Income Inequality",
      shinyWidgets::radioGroupButtons("mode", label = NULL,
        choices = c("Theoretical", "Simulated"), selected = "Theoretical",
        justified = TRUE, status = "primary"),
      selectInput("family", tags$small("Income distribution"),
        c("Lognormal" = "lognormal", "Gamma" = "gamma", "Exponential" = "exponential", "Beta" = "beta")),
      uiOutput("distribution_input"),
      conditionalPanel("input.mode == 'Simulated'",
        actionButton("resample", "New sample", class = "btn-primary btn-sm")),
      accordion(open = FALSE,
        accordion_panel("How it works", tags$small(htmltools::includeMarkdown("readme.md"))),
        accordion_panel("Inspiration and resources", tags$small(htmltools::includeMarkdown("resources.md")))),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = 6), row_heights = c(1, 1), gap = "0.75rem",
      card(card_header("Income distribution"), highchartOutput("density_chart", height = "100%")),
      card(card_header("Lorenz curve"), highchartOutput("lorenz_chart", height = "100%")),
      card(card_header("Pairwise income differences"), highchartOutput("pairs_chart", height = "100%")),
      card(card_header("Income by percentile"), highchartOutput("quantiles_chart", height = "100%"))
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  output$distribution_input <- renderUI({
    # Render just the visible family. Preserve its last parameters on return.
    id <- paste0("dist_", input$family)
    value <- isolate(input[[id]])
    if (!is.null(value)) value$family <- NULL
    modist_input(id, family = input$family, value = value)
  })
  parameters <- reactive({
    req(input$family)
    par <- input[[paste0("dist_", input$family)]]
    if (is.null(par)) par <- switch(input$family,
      lognormal = list(mu = 0, sigma = 1), gamma = list(alpha = 2, beta = 2),
      exponential = list(lam = 1), beta = list(alpha = 2, beta = 2))
    required <- switch(input$family, lognormal = c("mu", "sigma"),
      gamma = c("alpha", "beta"), exponential = "lam", beta = c("alpha", "beta"))
    req(all(required %in% names(par)), all(is.finite(unlist(par[required]))),
      all(unlist(par[setdiff(required, "mu")]) > 0))
    par
  })
  distribution <- reactive(income_distribution(input$family, parameters()))
  seed <- reactive(20261029L + input$resample)
  uniforms <- reactive(withr::with_seed(seed(), runif(1000)))
  sample_income <- reactive({
    x <- distribution()$quantile(uniforms())
    validate(need(all(is.finite(x)) && is.finite(sum(x)),
      "These parameters exceed numerical precision. Try less extreme values."))
    validate(need(max(x) > 0, "All incomes are zero: Gini and Lorenz are undefined."))
    x
  })
  curve <- reactive({
    if (input$mode == "Theoretical") distribution()$curve else lorenz(sample_income())
  })
  current_gini <- reactive({
    if (input$mode == "Theoretical") distribution()$gini else gini(sample_income())
  })
  curve_label <- reactive({
    if (input$mode == "Theoretical") sprintf("Gini %.4f", current_gini()) else {
      sprintf("Gini %.4f (theory %.4f)", current_gini(), distribution()$gini)
    }
  })
  pairs <- reactive({
    x <- if (input$mode == "Theoretical") distribution()$quantile(grid_p) else {
      as.numeric(quantile(sample_income(), grid_p))
    }
    validate(need(all(is.finite(x)) && max(x) > 0,
      "These parameters exceed numerical precision. Try less extreme values."))
    quantile_pairs(x)
  })
  density_data <- reactive({
    if (input$mode == "Theoretical") {
      # Display the central 99.9%; the Gini and Lorenz use the entire distribution.
      limits <- distribution()$quantile(c(0.00001, 0.999))
      validate(need(all(is.finite(limits)) && diff(limits) > 0, "Try less extreme parameters."))
      x <- seq(limits[1], limits[2], length.out = 500)
      list(data = xy_data(x, distribution()$density(x)), width = NULL)
    } else {
      h <- hist(sample_income(), breaks = 35, plot = FALSE)
      list(data = xy_data(h$mids, h$density), width = diff(h$breaks)[1])
    }
  })
  quantile_data <- reactive({
    theoretical <- distribution()$quantile(quantile_p)
    validate(need(all(is.finite(theoretical)), "Try less extreme parameters."))
    observed <- if (input$mode == "Theoretical") theoretical else sort(sample_income())
    list(observed = xy_data(100 * quantile_p, observed),
      theoretical = xy_data(100 * quantile_p, theoretical))
  })

  output$density_chart <- renderHighchart({
    # Create once; mode, parameters and samples all use the same series update.
    mode <- isolate(input$mode)
    d <- isolate(density_data())
    highchart() |> hc_chart(animation = FALSE) |>
      hc_xAxis(title = list(text = "Income"), min = 0) |>
      hc_yAxis(title = list(text = "Density"), min = 0) |>
      hc_plotOptions(series = list(animation = FALSE)) |>
      hc_add_series(id = "density", name = mode,
        type = if (mode == "Theoretical") "area" else "column", fillOpacity = 0.15,
        data = d$data, pointRange = d$width,
        pointPadding = 0, groupPadding = 0, marker = list(enabled = FALSE)) |>
      hc_tooltip(valueDecimals = 4) |> hc_legend(enabled = FALSE)
  })
  output$lorenz_chart <- renderHighchart({
    # Switching modes changes the curve from population to empirical points.
    # Initialize each view from its current data, then proxy ordinary edits.
    input$mode
    d <- isolate(curve())
    highchart() |> hc_chart(type = "line", animation = FALSE) |>
      hc_title(text = NULL) |>
      hc_subtitle(text = NULL) |>
      hc_xAxis(title = list(text = "Cumulative share of people"), min = 0, max = 1) |>
      hc_yAxis(title = list(text = "Cumulative share of income"), min = 0, max = 1) |>
      hc_plotOptions(series = list(marker = list(enabled = FALSE), animation = FALSE)) |>
      hc_add_series(id = "lorenz", name = isolate(curve_label()), data = xy_data(d$p, d$L)) |>
      hc_add_series(name = "Equality", data = list(c(0, 0), c(1, 1)), dashStyle = "Dash") |>
      hc_tooltip(shared = TRUE, valueDecimals = 4)
  })
  output$pairs_chart <- renderHighchart({
    input$mode
    highchart() |> hc_chart(type = "heatmap", animation = FALSE) |>
      hc_xAxis(title = list(text = "First income percentile"), min = 0, max = 100) |>
      hc_yAxis(title = list(text = "Second income percentile"), min = 0, max = 100, reversed = TRUE) |>
      hc_colorAxis(min = 0) |>
      hc_add_series(id = "pairs", name = "Absolute difference", data = isolate(pairs()),
        colsize = 2, rowsize = 2, borderWidth = 0, turboThreshold = 0) |>
      hc_tooltip(headerFormat = "", pointFormat = paste0("Income groups: {point.group1} and {point.group2}<br>",
        "Incomes: {point.income1:.3f} and {point.income2:.3f}<br>",
        "Absolute difference: {point.value:.4f}<br>Grid Gini contribution: {point.contribution:.6f}"))
  })
  output$quantiles_chart <- renderHighchart({
    mode <- input$mode
    d <- isolate(quantile_data())
    highchart() |> hc_chart(type = "line", animation = FALSE) |>
      hc_xAxis(title = list(text = "Income percentile"), min = 0, max = 100) |>
      hc_yAxis(title = list(text = "Income"), min = 0) |>
      hc_plotOptions(series = list(marker = list(enabled = FALSE), animation = FALSE)) |>
      hc_add_series(id = "quantiles", name = mode, data = d$observed) |>
      hc_add_series(id = "reference", name = "Theoretical reference", data = d$theoretical,
        dashStyle = "Dash", visible = mode == "Simulated", showInLegend = mode == "Simulated") |>
      hc_tooltip(shared = TRUE, valueDecimals = 3, headerFormat = "Percentile ~{point.key:.0f}<br>")
  })
  observeEvent(density_data(), {
    d <- density_data()
    highchartProxy("density_chart") |>
      hcpxy_update_series(id = "density", name = input$mode,
        type = if (input$mode == "Theoretical") "area" else "column",
        data = d$data, pointRange = d$width, fillOpacity = 0.15,
        animation = FALSE, marker = list(enabled = FALSE), pointPadding = 0, groupPadding = 0)
  }, ignoreInit = TRUE)
  observeEvent(list(curve(), curve_label()), {
    l <- curve()
    highchartProxy("lorenz_chart") |>
      hcpxy_update_series(id = "lorenz", name = curve_label(), data = xy_data(l$p, l$L))
  }, ignoreInit = TRUE)
  observeEvent(pairs(), {
    highchartProxy("pairs_chart") |>
      hcpxy_update_series(id = "pairs", data = pairs())
  }, ignoreInit = TRUE)
  observeEvent(quantile_data(), {
    d <- quantile_data()
    highchartProxy("quantiles_chart") |>
      hcpxy_update_series(id = "quantiles", name = input$mode, data = d$observed) |>
      hcpxy_update_series(id = "reference", data = d$theoretical,
        visible = input$mode == "Simulated", showInLegend = input$mode == "Simulated")
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
