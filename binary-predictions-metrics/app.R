# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(markdown)
library(highcharter)

# theme options -----------------------------------------------------------
apptheme <- bs_theme()

sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

options(
  highcharter.theme = hc_theme(
    chart = list(style = list(fontFamily =  "system-ui")),
    legend = list(itemStyle = list(fontWeight = "normal")),
    xAxis = list(gridLineWidth = 1),
    colors = unname(bs_get_variables(apptheme, c("primary", "danger", "success",  "warning", "info", "secondary")))
    )
  )

# data --------------------------------------------------------------------
credit_data <- modeldata::credit_data |> 
  as_tibble() |> 
  rename_all(str_to_lower) |> 
  select(status, where(is.numeric)) |> 
  
  # avoid logscale problems
  filter(debt > 1) |> 
  
  # fewer points
  group_by(status) |>
  sample_n(200) |>

  ungroup()

# credit_data |> count(status)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Binary predictions",
      withMathJax(),
      selectInput("variable", tags$small("Variable"), choices = names(credit_data)[-1]),
      checkboxInput("logscale", tags$small("Log-scale on \\(x\\)-axis")),
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
      col_widths = 12,
      gap = "0.75rem",
      card(highchartOutput("hcpoints"))
      )
    )
  )

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # input <- list(variable = "time"); input

  data <- reactive({
    set.seed(123)
    data <- credit_data |> 
      select(status, variable = !!input$variable) |> 
      mutate(
        jitter = rbeta(n(), shape1 = 5, shape2 = 5),
        jitter = round(jitter, 3),
        id     = row_number()
        )
    data
  })
  
  output$hcpoints <- renderHighchart({
    
    varname <- isolate(input$variable)
    
    data <- isolate(data())
    datas <- data |> 
      ungroup() |> 
      group_nest(status) |> 
      mutate(data = map(data, select, x = variable, y = jitter, id)) |> 
      deframe() 

    highchart() |> 
      hc_yAxis(visible = FALSE) |> 
      hc_xAxis(title = list(text = varname)) |> 
      hc_add_series(name = "good", id = "good", data = datas[["good"]], type = "scatter") |> 
      hc_add_series(name = "bad", id = "bad",  data = datas[["bad"]], type = "scatter")
  })
  
  # update scatter/jitter
  observe({
    
    invalidateLater(1000)
    
    data <- data()

    datas <- data |>
      group_by(status) |> 
      group_nest() |>
      mutate(
        data = map(data, select, x = variable, y = jitter, id),
        data = map(data, list_parse)
        ) |>
      deframe()

    highchartProxy("hcpoints") |>
      hcpxy_update_series(id = "good", data = datas[["good"]]) |>
      hcpxy_update_series(id = "bad",  data = datas[["bad"]]) |> 
      hcpxy_update(
        xAxis = list(
          type = ifelse(input$logscale, "logarithmic", "linear"),
          title = list(text = str_to_title(input$variable))
          )
      )
    
  }) |> bindEvent(c(input$variable, input$logscale))
    
  
}

shinyApp(ui, server)
