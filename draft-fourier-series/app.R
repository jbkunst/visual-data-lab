# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(highcharter)

# theme -------------------------------------------------------------------
apptheme <- bs_theme()
options(highcharter.theme = hc_theme(
  chart = list(style = list(fontFamily = "system-ui")),
  legend = list(itemStyle = list(fontWeight = "normal")),
  xAxis = list(gridLineWidth = 1),
  colors = unname(bs_get_variables(
    apptheme, c("primary", "danger", "success", "warning", "info", "secondary")
  )),
  plotOptions = list(line = list(marker = list(enabled = FALSE)))
))

# app options -------------------------------------------------------------
X_MAX <- 10; K_MAX <- 20; TOP_N <- 5; N <- 400

target_values <- function(name, x) switch(
  name,
  quadratic = (x - 5)^2 / 8 - 1,
  polynomial = x * (x - 5) * (x - 10) / 18,
  signal = sin(2 * pi * x / 10) +
    0.6 * cos(4 * pi * x / 10) +
    0.3 * sin(8 * pi * x / 10),
  floor = floor(x / 2) - 2
)

fourier_analysis <- function(name) {
  x <- seq(0, X_MAX, length.out = N + 1)
  x_fit <- x[-length(x)]
  y <- target_values(name, x)
  y_fit <- target_values(name, x_fit)
  k <- seq_len(K_MAX)
  angle_fit <- outer(x_fit, k, function(x, k) 2 * pi * k * x / X_MAX)
  a <- 2 / N * colSums(y_fit * cos(angle_fit))
  b <- 2 / N * colSums(y_fit * sin(angle_fit))
  angle <- outer(x, k, function(x, k) 2 * pi * k * x / X_MAX)
  parts <- sweep(cos(angle), 2, a, "*") + sweep(sin(angle), 2, b, "*")
  fits <- mean(y_fit) + t(apply(parts, 1, cumsum))
  errors <- fits[-nrow(fits), , drop = FALSE] - y_fit
  quality <- 100 * (1 - colSums(errors^2) / sum((y_fit - mean(y_fit))^2))
  list(
    x = x, y = y, parts = parts, fits = fits,
    amplitudes = sqrt(a^2 + b^2),
    quality = quality, gain = c(quality[1], diff(quality))
  )
}

xy_data <- function(x, y) list_parse2(data.frame(x = x, y = y))

component_data <- function(data, selected_k) {
  index <- head(
    order(data$amplitudes[seq_len(selected_k)], decreasing = TRUE), TOP_N
  )
  amplitudes <- data$amplitudes[index]
  widths <- if (length(unique(amplitudes)) == 1) {
    rep(4, length(amplitudes))
  } else {
    1.5 + 4.5 * (amplitudes - min(amplitudes)) /
      (max(amplitudes) - min(amplitudes))
  }
  lapply(seq_along(index), function(i) list(
    name = sprintf("k = %d | A = %.3f", index[i], amplitudes[i]),
    data = xy_data(data$x, data$parts[, index[i]]),
    lineWidth = widths[i]
  ))
}

app_card <- function(title, id) card(
  full_screen = TRUE,
  card_header(title),
  card_body(padding = 0, highchartOutput(id))
)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      width = 300,
      title = "Fourier Series",
      selectInput(
        "target", tags$small("Target function"),
        choices = c(
          "Quadratic" = "quadratic", "Polynomial" = "polynomial",
          "Periodic signal" = "signal", "Floor function" = "floor"
        ),
        selected = "signal"
      ),
      sliderInput(
        "components", tags$small("Number of components"),
        1, K_MAX, 5, step = 1, width = "100%"
      ),
      tags$small(uiOutput("formula", inline = TRUE)),
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
      app_card("Function and approximation", "approximation"),
      app_card("Leading Fourier components", "components_chart"),
      app_card("Fourier spectrum", "spectrum"),
      app_card("Reconstruction quality", "quality")
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  analysis <- reactive(fourier_analysis(input$target))
  selected <- reactive({
    data <- analysis()
    k <- input$components
    list(
      data = data,
      approximation = data$fits[, k],
      components = component_data(data, k),
      spectrum = data.frame(x = seq_len(k), y = data$amplitudes[seq_len(k)]),
      quality = data.frame(x = seq_len(K_MAX), y = data$quality, gain = data$gain)
    )
  })

  output$formula <- renderUI({
    formulas <- c(
      quadratic = "\\(f(x)=(x-5)^2/8-1\\)",
      polynomial = "\\(f(x)=x(x-5)(x-10)/18\\)",
      signal = "\\(f(x)=\\sin(2\\pi x/10)+0.6\\cos(4\\pi x/10)+0.3\\sin(8\\pi x/10)\\)",
      floor = "\\(f(x)=\\lfloor x/2 \\rfloor-2\\)"
    )
    withMathJax(HTML(formulas[[input$target]]))
  })

  output$approximation <- renderHighchart({
    view <- isolate(selected())
    highchart() |>
      hc_xAxis(title = list(text = "x"), min = 0, max = X_MAX) |>
      hc_yAxis(title = list(text = "f(x)")) |>
      hc_tooltip(shared = TRUE, valueDecimals = 3) |>
      hc_add_series(
        id = "target", name = "Target",
        data = xy_data(view$data$x, view$data$y), lineWidth = 4
      ) |>
      hc_add_series(
        id = "approximation", name = "Fourier approximation",
        data = xy_data(view$data$x, view$approximation),
        lineWidth = 3, dashStyle = "ShortDash"
      )
  })

  output$components_chart <- renderHighchart({
    components <- isolate(selected()$components)
    chart <- highchart() |>
      hc_xAxis(title = list(text = "x"), min = 0, max = X_MAX) |>
      hc_yAxis(title = list(text = "Component value")) |>
      hc_tooltip(shared = TRUE, valueDecimals = 3)
    for (i in seq_len(TOP_N)) {
      component <- components[[i]]
      chart <- chart |>
        hc_add_series(
          id = paste0("component", i), name = component$name,
          data = component$data, lineWidth = component$lineWidth
        )
    }
    chart
  })

  output$spectrum <- renderHighchart({
    spectrum <- isolate(selected()$spectrum)
    highchart() |>
      hc_chart(type = "column") |>
      hc_xAxis(title = list(text = "Frequency k"), allowDecimals = FALSE) |>
      hc_yAxis(title = list(text = "Amplitude")) |>
      hc_tooltip(pointFormat = "Amplitude: <b>{point.y:.3f}</b>") |>
      hc_add_series(
        id = "spectrum", name = "Amplitude",
        data = xy_data(spectrum$x, spectrum$y), showInLegend = FALSE
      )
  })

  output$quality <- renderHighchart({
    view <- isolate(selected())
    highchart() |>
      hc_xAxis(
        title = list(text = "Number of components"),
        min = 1, max = K_MAX, allowDecimals = FALSE
      ) |>
      hc_yAxis(
        title = list(text = "Reconstruction quality"),
        min = 0, max = 100, labels = list(format = "{value}%")
      ) |>
      hc_tooltip(pointFormatter = JS(
        "function () {
           return '<b>' + Highcharts.numberFormat(this.y, 2) +
             '%</b><br/>Gain: +' +
             Highcharts.numberFormat(this.gain, 2) + ' pp';
         }"
      )) |>
      hc_add_series(
        id = "quality_curve", name = "Quality",
        data = list_parse2(view$quality), lineWidth = 4
      ) |>
      hc_add_series(
        id = "selected_k", name = "Selected", type = "scatter",
        data = list(list(
          x = input$components, y = view$data$quality[input$components]
        )),
        marker = list(radius = 7), showInLegend = FALSE
      )
  })

  observeEvent(selected(), {
    view <- selected()
    highchartProxy("approximation") |>
      hcpxy_update_series(
        id = "target", data = xy_data(view$data$x, view$data$y)
      ) |>
      hcpxy_update_series(
        id = "approximation", data = xy_data(view$data$x, view$approximation)
      )

    proxy <- highchartProxy("components_chart")
    for (i in seq_len(TOP_N)) {
      if (i <= length(view$components)) {
        component <- view$components[[i]]
        proxy <- proxy |>
          hcpxy_update_series(
            id = paste0("component", i), name = component$name,
            data = component$data, lineWidth = component$lineWidth,
            visible = TRUE, showInLegend = TRUE
          )
      } else {
        proxy <- proxy |>
          hcpxy_update_series(
            id = paste0("component", i), data = list(),
            visible = FALSE, showInLegend = FALSE
          )
      }
    }

    highchartProxy("spectrum") |>
      hcpxy_update_series(
        id = "spectrum", data = xy_data(view$spectrum$x, view$spectrum$y)
      )
    highchartProxy("quality") |>
      hcpxy_update_series(id = "quality_curve", data = list_parse2(view$quality)) |>
      hcpxy_update_series(
        id = "selected_k",
        data = list(list(
          x = input$components, y = view$data$quality[input$components]
        ))
      )
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
