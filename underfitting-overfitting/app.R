# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(highcharter)
library(markdown)
library(shinyWidgets)

# theme options -----------------------------------------------------------
apptheme <- bs_theme()

sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

options(
  highcharter.theme = hc_theme(
    chart = list(style = list(fontFamily = "system-ui")),
    legend = list(itemStyle = list(fontWeight = "normal")),
    colors = unname(bs_get_variables(apptheme, c("primary", "danger", "warning", "success", "info", "secondary"))),
    tooltip = list(valueDecimals = 3, shared = TRUE),
    xAxis = list(gridLineWidth = 1),
    plotOptions = list(
      spline = list(marker = list(enabled = FALSE, symbol = "circle")),
      line = list(marker = list(enabled = FALSE, symbol = "circle")),
      scatter = list(marker = list(symbol = "circle"))
    )
  )
)

# app options -------------------------------------------------------------
bandwidth_grid <- round(exp(seq(log(0.1), log(20), length.out = 25)), 2)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Underfitting and Overfitting",
      shinyWidgets::sliderTextInput(
        "n",
        tags$small("Training observations"),
        choices = c(50, 100, 250, 500, 1000),
        selected = 250,
        grid = TRUE,
        force_edges = TRUE
      ),
      shinyWidgets::sliderTextInput(
        "bandwidth",
        tags$small("Bandwidth"),
        choices = c("0.1", "0.5", "1", "2", "3", "4", "5", "10", "20"),
        selected = "2",
        grid = TRUE,
        force_edges = TRUE
      ),
      tags$small("Smaller values create a more flexible model."),
      checkboxInput("show_test", tags$small("Show test data"), value = FALSE),
      checkboxInput("show_truth", tags$small("Show true relationship"), value = FALSE),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
        ),
        accordion_panel(
          "Inspiration and resources",
          tags$small(htmltools::includeMarkdown("resources.md"))
        )
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    layout_columns(
      col_widths = c(12, 6, 6),
      row_heights = c(3, 2),
      card(
        card_header("Model fit"),
        card_body(highchartOutput("chartdata"))
      ),
      card(
        card_header("RMSE at selected bandwidth"),
        card_body(highchartOutput("charterror"))
      ),
      card(
        card_header("RMSE across bandwidths"),
        card_body(highchartOutput("chartbandwidth"))
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {

  n_obs <- reactive(as.integer(input$n)) |>
    debounce(600)

  bandwidth <- reactive(as.numeric(input$bandwidth)) |>
    debounce(600)

  dxy <- reactive({
    n <- n_obs()
    x <- scales::rescale(seq_len(n) / n, to = c(0, 50))

    # Original relationship: x + 10 * sin(x / 5) + 20 * sin(x / 20)
    truth <- 20 + 0.5 * x + 12 * sin(x / 5)

    set.seed(1234)

    bind_rows(
      tibble(x = x, y = truth + rnorm(n, sd = 5), truth = truth, s = "train"),
      tibble(x = x, y = truth + rnorm(n, sd = 5), truth = truth, s = "test")
    ) |>
      mutate(across(c(x, y, truth), ~ round(.x, 3)))
  })

  dpred <- reactive({
    train <- dxy() |>
      filter(s == "train")

    fit <- ksmooth(
      train$x,
      train$y,
      kernel = "normal",
      bandwidth = bandwidth(),
      x.points = train$x
    )

    tibble(x = fit$x, y = fit$y)
  })

  derr <- reactive({
    dxy() |>
      left_join(dpred(), by = "x", suffix = c("_real", "_model")) |>
      group_by(s) |>
      summarise(error = Metrics::rmse(y_real, y_model), .groups = "drop")
  })

  derr_bw <- reactive({
    data <- dxy()
    train <- filter(data, s == "train")
    test <- filter(data, s == "test")

    purrr::map_dfr(bandwidth_grid, function(bw) {
      prediction <- ksmooth(
        train$x,
        train$y,
        kernel = "normal",
        bandwidth = bw,
        x.points = train$x
      )$y

      tibble(
        bw = bw,
        train = Metrics::rmse(train$y, prediction),
        test = Metrics::rmse(test$y, prediction)
      )
    }) |>
      tidyr::pivot_longer(c(train, test), names_to = "s", values_to = "error")
  })

  output$chartdata <- renderHighchart({
    data <- isolate(dxy())
    model <- isolate(dpred())

    datasets <- data |>
      select(x, y, s) |>
      group_nest(s) |>
      deframe()

    truth <- data |>
      distinct(x, truth) |>
      transmute(x, y = truth)

    highchart() |>
      hc_chart(type = "scatter") |>
      hc_xAxis(title = list(text = "Variable X")) |>
      hc_yAxis(title = list(text = "Variable Y")) |>
      hc_add_series(
        data = list_parse2(datasets$train),
        id = "train",
        name = "Training data",
        zIndex = 2,
        opacity = 0.5
      ) |>
      hc_add_series(
        data = list_parse2(datasets$test),
        id = "test",
        name = "Test data",
        visible = FALSE,
        showInLegend = FALSE,
        zIndex = 1,
        opacity = 0.5
      ) |>
      hc_add_series(
        data = list_parse2(model),
        id = "model",
        name = "Estimated model",
        type = "line",
        zIndex = 4,
        lineWidth = 10
      ) |>
      hc_add_series(
        data = list_parse2(truth),
        id = "truth",
        name = "True relationship",
        type = "line",
        dashStyle = "ShortDash",
        visible = FALSE,
        showInLegend = FALSE,
        zIndex = 3,
        lineWidth = 10
      )
  })

  observeEvent(list(n_obs(), bandwidth()), {
    data <- dxy()
    model <- dpred()

    datasets <- data |>
      select(x, y, s) |>
      group_nest(s) |>
      deframe()

    truth <- data |>
      distinct(x, truth) |>
      transmute(x, y = truth)

    highchartProxy("chartdata") |>
      hcpxy_update_series(id = "train", data = list_parse2(datasets$train)) |>
      hcpxy_update_series(id = "test", data = list_parse2(datasets$test)) |>
      hcpxy_update_series(id = "model", data = list_parse2(model)) |>
      hcpxy_update_series(id = "truth", data = list_parse2(truth))
  })

  output$charterror <- renderHighchart({
    errors <- as.list(deframe(isolate(derr())))

    highchart() |>
      hc_chart(type = "column") |>
      hc_xAxis(
        title = list(text = "Dataset"),
        type = "category",
        categories = c("Train", "Test")
      ) |>
      hc_yAxis(title = list(text = "RMSE"), max = 10) |>
      hc_plotOptions(
        series = list(
          stacking = "normal",
          minPointLength = 0,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("function () { return Highcharts.numberFormat(this.y, 3); }")
          )
        )
      ) |>
      hc_add_series(
        data = list_parse2(tibble(x = 0, y = errors$train)),
        id = "train",
        name = "Train error"
      ) |>
      hc_add_series(
        data = list_parse2(tibble(x = 1, y = errors$test)),
        id = "test",
        name = "Test error",
        visible = FALSE,
        showInLegend = FALSE
      )
  })

  observeEvent(list(n_obs(), bandwidth()), {
    errors <- as.list(deframe(derr()))

    highchartProxy("charterror") |>
      hcpxy_update_series(id = "train", data = list_parse2(tibble(x = 0, y = errors$train))) |>
      hcpxy_update_series(id = "test", data = list_parse2(tibble(x = 1, y = errors$test)))
  })

  output$chartbandwidth <- renderHighchart({
    errors <- isolate(derr_bw()) |>
      rename(x = bw, y = error) |>
      select(x, y, s) |>
      group_nest(s) |>
      deframe()

    bw <- isolate(bandwidth())
    ymax <- 1.05 * max(c(errors$train$y, errors$test$y))

    highchart() |>
      hc_chart(type = "line") |>
      hc_xAxis(title = list(text = "Bandwidth"), min = 0.1, max = 20) |>
      hc_yAxis(title = list(text = "RMSE")) |>
      hc_add_series(
        data = list_parse2(errors$train),
        id = "train",
        name = "Train error",
        zIndex = 2
      ) |>
      hc_add_series(
        data = list_parse2(errors$test),
        id = "test",
        name = "Test error",
        visible = FALSE,
        showInLegend = FALSE,
        zIndex = 1
      ) |>
      hc_add_series(
        data = list(list(x = bw, y = 0), list(x = bw, y = ymax)),
        id = "bandwidth",
        name = "Selected bandwidth",
        enableMouseTracking = FALSE,
        zIndex = 3
      )
  })

  observeEvent(n_obs(), {
    errors <- derr_bw() |>
      rename(x = bw, y = error) |>
      select(x, y, s) |>
      group_nest(s) |>
      deframe()

    bw <- bandwidth()
    ymax <- 1.05 * max(c(errors$train$y, errors$test$y))

    highchartProxy("chartbandwidth") |>
      hcpxy_update_series(id = "train", data = list_parse2(errors$train)) |>
      hcpxy_update_series(id = "test", data = list_parse2(errors$test)) |>
      hcpxy_update_series(id = "bandwidth", data = list(list(x = bw, y = 0), list(x = bw, y = ymax)))
  })

  observeEvent(bandwidth(), {
    ymax <- 1.05 * max(derr_bw()$error)

    highchartProxy("chartbandwidth") |>
      hcpxy_update_series(
        id = "bandwidth",
        data = list(
          list(x = bandwidth(), y = 0),
          list(x = bandwidth(), y = ymax)
        )
      )
  })

  observeEvent(input$show_test, {
    highchartProxy("chartdata") |>
      hcpxy_update_series(id = "test", visible = input$show_test, showInLegend = input$show_test)

    highchartProxy("charterror") |>
      hcpxy_update_series(id = "test", visible = input$show_test, showInLegend = input$show_test)

    highchartProxy("chartbandwidth") |>
      hcpxy_update_series(id = "test", visible = input$show_test, showInLegend = input$show_test)
  })

  observeEvent(input$show_truth, {
    highchartProxy("chartdata") |>
      hcpxy_update_series(id = "truth", visible = input$show_truth, showInLegend = input$show_truth)
  })
}

shinyApp(ui, server)