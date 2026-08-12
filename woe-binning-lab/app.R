# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(tibble)
library(markdown)
library(highcharter)
library(risk3r)
library(vdltheme)

# data --------------------------------------------------------------------
credit_data <- modeldata::credit_data |>
  as_tibble() |>
  rename_with(str_to_lower) |>
  select(status, where(is.numeric))

variable_labels <- c(
  seniority = "Seniority", amount = "Loan amount", income = "Income",
  assets = "Assets", price = "Price", expenses = "Expenses",
  time = "Loan term", age = "Age", debt = "Debt"
)
variable_order <- names(variable_labels)
method_labels <- c(tree = "Decision tree", chimerge = "Chi-square merging")

# helpers -----------------------------------------------------------------
bin_profile <- function(variable, method, max_bins, min_share, stop_limit) {
  bins <- risk3r::woebin2(
    dt = credit_data, y = "status", x = variable,
    method = method, positive = "bad", no_cores = 0,
    bin_num_limit = max_bins, count_distr_limit = min_share,
    stop_limit = stop_limit
  )
  values <- bins[[variable]] |>
    transmute(
      bin, good = neg, bad = pos, observations = count,
      population_share = count_distr, default_rate = posprob,
      woe, bin_iv, total_iv
    )
  summary <- risk3r::woebin_summary(bins, sort = FALSE)

  list(
    values = values,
    total_iv = values$total_iv[[1L]],
    iv_label = as.character(risk3r::iv_label(values$total_iv[[1L]])),
    ks = summary$ks[[1L]],
    monotone = summary$monotone[[1L]]
  )
}

woe_points <- function(data) {
  lapply(seq_len(nrow(data)), function(i) {
    list(
      y = data$woe[[i]],
      color = if (data$woe[[i]] >= 0) danger_color else primary_color,
      custom = list(
        good = data$good[[i]], bad = data$bad[[i]],
        default_rate = data$default_rate[[i]],
        population_share = data$population_share[[i]]
      )
    )
  })
}

iv_points <- function(data) {
  points <- lapply(seq_len(nrow(data)), function(i) {
    list(
      name = data$bin[[i]], y = data$bin_iv[[i]],
      color = if (data$woe[[i]] >= 0) danger_color else primary_color
    )
  })
  c(points, list(list(name = "Total IV", isSum = TRUE, color = "#34495e")))
}

# theme -------------------------------------------------------------------
apptheme <- theme_vdl()
primary_color <- unname(bs_get_variables(apptheme, "primary"))
danger_color <- unname(bs_get_variables(apptheme, "danger"))
sidebar <- purrr::partial(sidebar, width = 300)
card <- purrr::partial(
  card, full_screen = TRUE,
  wrapper = purrr::partial(card_body, padding = 0)
)
options(highcharter.theme = highcharter_theme_vdl())

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme, padding = 0,
  layout_sidebar(
    fillable = TRUE, padding = "0.75rem",
    sidebar = sidebar(
      title = "WoE Binning Lab",
      selectInput(
        "variable", tags$small("Variable"),
        choices = stats::setNames(variable_order, variable_labels[variable_order])
      ),
      radioButtons(
        "method", tags$small("Binning method"),
        choices = stats::setNames(names(method_labels), method_labels),
        selected = "tree"
      ),
      sliderInput(
        "max_bins",
        input_label_vdl(
          "Maximum bins",
          "Limits the number of groups retained by the binning algorithm."
        ),
        min = 3, max = 12, value = 8, step = 1
      ),
      sliderInput(
        "min_share",
        input_label_vdl(
          "Minimum bin share",
          "Prevents bins supported by too few observations."
        ),
        min = 0.02, max = 0.20, value = 0.05, step = 0.01
      ),
      sliderInput(
        "stop_limit",
        input_label_vdl(
          "Stopping threshold",
          "Higher values stop merging earlier; lower values allow finer bins."
        ),
        min = 0.01, max = 0.50, value = 0.10, step = 0.01
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
      col_widths = c(7, 5), gap = "0.75rem",
      card(
        card_header(uiOutput("profile_title", class = "d-block w-100")),
        highchartOutput("profile_plot", height = "100%")
      ),
      layout_columns(
        col_widths = 12, row_heights = c(1, 1), gap = "0.75rem",
        card(
          card_header("Weight of evidence"),
          highchartOutput("woe_plot", height = "100%")
        ),
        card(
          card_header(uiOutput("iv_title", class = "d-block w-100")),
          highchartOutput("iv_plot", height = "100%")
        )
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  profile <- reactive({
    bin_profile(
      input$variable, input$method, input$max_bins,
      input$min_share, input$stop_limit
    )
  })

  output$profile_title <- renderUI({
    tags$div(
      class = "d-flex justify-content-between align-items-center",
      tags$span("From observations to risk bins"),
      tags$small(
        sprintf("%s · %d bins", method_labels[[input$method]], nrow(profile()$values)),
        class = "text-muted fw-normal"
      )
    )
  })

  output$profile_plot <- renderHighchart({
    data <- profile()$values
    highchart() |>
      hc_chart(type = "column") |>
      hc_xAxis(categories = data$bin, title = list(text = variable_labels[[input$variable]])) |>
      hc_yAxis_multiples(
        list(title = list(text = "Clients"), min = 0),
        list(
          title = list(text = "Default rate"), min = 0, max = 100,
          opposite = TRUE, labels = list(format = "{value:.0f}%")
        )
      ) |>
      hc_tooltip(shared = TRUE) |>
      hc_plotOptions(
        column = list(stacking = "normal", borderWidth = 0),
        series = list(animation = list(duration = 300))
      ) |>
      hc_add_series(id = "good", name = "Good", data = data$good, color = primary_color) |>
      hc_add_series(id = "bad", name = "Bad", data = data$bad, color = danger_color) |>
      hc_add_series(
        id = "default_rate", name = "Default rate", type = "spline",
        data = 100 * data$default_rate, yAxis = 1, color = "#34495e",
        lineWidth = 3, marker = list(enabled = TRUE, symbol = "circle", radius = 3),
        tooltip = list(valueDecimals = 3)
      )
  })

  output$woe_plot <- renderHighchart({
    data <- profile()$values
    highchart() |>
      hc_chart(type = "column") |>
      hc_xAxis(categories = data$bin) |>
      hc_yAxis(
        title = list(text = "log(bad share / good share)"),
        plotLines = list(list(value = 0, color = "#dee2e6", width = 1, zIndex = 3))
      ) |>
      hc_legend(enabled = FALSE) |>
      hc_tooltip(pointFormat = paste0(
        "WoE: <b>{point.y:.3f}</b><br/>",
        "Default rate: {point.custom.default_rate:.1%}<br/>",
        "Portfolio: {point.custom.population_share:.1%}<br/>",
        "Good: {point.custom.good}<br/>Bad: {point.custom.bad}"
      )) |>
      hc_plotOptions(column = list(borderWidth = 0, animation = list(duration = 300))) |>
      hc_add_series(id = "woe", name = "WoE", data = woe_points(data))
  })

  output$iv_title <- renderUI({
    tags$div(
      class = "d-flex justify-content-between align-items-center",
      tags$span("Information Value decomposition"),
      tags$small(
        sprintf("IV · %.3f · %s", profile()$total_iv, str_to_title(profile()$iv_label)),
        class = "text-muted fw-normal"
      )
    )
  })

  output$iv_plot <- renderHighchart({
    data <- profile()$values
    highchart() |>
      hc_add_dependency("modules/waterfall.js") |>
      hc_chart(type = "waterfall") |>
      hc_xAxis(type = "category") |>
      hc_yAxis(title = list(text = "Information Value"), min = 0) |>
      hc_legend(enabled = FALSE) |>
      hc_tooltip(pointFormat = "IV contribution: <b>{point.y:.3f}</b>") |>
      hc_plotOptions(waterfall = list(
        borderWidth = 0, animation = list(duration = 300),
        dataLabels = list(
          enabled = TRUE, style = list(
            color = "#495057", fontWeight = "normal", textOutline = "none"
          ), format = "{point.y:.3f}"
        )
      )) |>
      hc_add_series(id = "iv", name = "IV contribution", data = iv_points(data))
  })
}

shinyApp(ui, server)
