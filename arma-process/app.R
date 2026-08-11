# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(highcharter)
library(markdown)
library(vdltheme)

# theme options -----------------------------------------------------------
apptheme <- theme_vdl()

sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

options(highcharter.theme = highcharter_theme_vdl())

# app options -------------------------------------------------------------
LAG_MAX  <- 10
STR_OBS  <- 20
NOBS     <- 5000
AR       <- 0.5
MA       <- 0.00
SEED     <- 123
DURATION <- 100 # needs to be <= than min refresh interval

# start chart
set.seed(SEED)

ts_aux <- arima.sim(model = list(ar = AR, ma = MA), n = STR_OBS)

teoACF <- as.numeric(ARMAacf(ar = AR, ma = MA, lag.max = LAG_MAX, pacf = FALSE))
smpACF <- as.numeric(acf(ts_aux, lag.max = LAG_MAX, plot = FALSE)$acf)

# plot(smpACF, ylim = c(-1, 1))
# lines(teoACF)

teoPACF <- as.numeric(ARMAacf(ar = AR, ma = MA, lag.max = LAG_MAX, pacf = TRUE))
smpPACF <- as.numeric(pacf(ts_aux, lag.max = LAG_MAX, plot = FALSE)$acf)

# plot(smpPACF, ylim = c(-1, 1))
# lines(teoPACF)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "ARMA model Simulation",
      withMathJax(),
      sliderInput("ar", tags$span("AR coefficient ", HTML("\\(\\phi\\)")), -.9, .9, AR, .1, width = "100%"),
      sliderInput("ma", tags$span("MA coefficient ", HTML("\\(\\theta\\)")), -.9, .9, MA, .1, width = "100%"),
      sliderInput("interval", "Refresh (secs.)", 0.5, 2, value = 0.5, step = 0.5, width = "100%"),
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
      col_widths = c(12, 6, 6),
      row_heights = c(3, 2),
      gap = "0.75rem",
      card(card_header(uiOutput("model", inline = TRUE)), highchartOutput("ts")),
      card(card_header("ACF"), highchartOutput("acf")),
      card(card_header("PACF"), highchartOutput("pacf"))
      )
    )
  )

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # input <- list(ar = AR, ma = MA); input
  
  value <- reactiveVal(STR_OBS)
  
  ts <- reactive({
    
    value(STR_OBS)
    
    # input <- list(ar = 0.9, ma = 0.1, nobs = 200)
    set.seed(SEED)
    
    ts <- arima.sim(model = list(ar = input$ar, ma = input$ma), n = NOBS)
    
  })
  
  output$model <- renderUI({
    
    arp <- ifelse(input$ar != 0, paste0(input$ar, " \\times X_{t-1}"), "")
    map <- ifelse(input$ma != 0, paste0(" + ", input$ma, " \\times \\epsilon_{t-1}"), "")
    
    mod <- paste0("X_{t} = ",
                  arp,
                  ifelse(input$ar != 0, " + ", ""),
                  "\\epsilon_t",
                  map
    )
    
    mod <- paste0("$$", mod, "$$")
    
    tags$small(tags$p(withMathJax(mod)))
    
  })
  
  output$ts <- renderHighchart({
    
    ts <- ts()
    
    df <- data.frame(x = 1:STR_OBS, y = head(ts, STR_OBS))
    
    hchart(
      df,
      "line",
      id = "ts",
      name = "Time series",
      marker = list(enabled = FALSE),
      animation = list(duration = DURATION),
      tooltip = list(valueDecimals = 3)
    ) |>
      hc_navigator(
        enabled = TRUE,
        series = list(type = "line"),
        xAxis = list(labels = list(enabled = FALSE))
      ) |>
      hc_yAxis_multiples(
        # default axis
        list(title = list(text = "")),
        list(
          title = list(text = ""),
          linkedTo = 0,
          opposite = TRUE,
          tickPositioner = JS(
            "function(min,max){
               var data = this.chart.yAxis[0].series[0].processedYData;
               //last point
               return [Math.round(1000 * data[data.length-1])/1000];
            }"
          )
        )
      )
    
  })
  
  observeEvent(ts(), {
    
    # if ts change redraw the teo ACF
    ts <- ts()
        
    cli::cli_inform(input$ar)
    cli::cli_inform(input$ma)
    
    teoACF <- as.numeric(
      ARMAacf(
        # ar = 0.2,
        # ma = 0.2,
        ar = ifelse(!is.null(input$ar), input$ar, AR),
        ma = ifelse(!is.null(input$ma), input$ma, MA),
        lag.max = LAG_MAX,
        pacf = FALSE
      )
    )
    
    smpACF <- as.numeric(acf(head(ts, STR_OBS), lag.max = LAG_MAX, plot = FALSE)$acf)
    
    highchartProxy("acf") |>
      hcpxy_update_series(id = "tacf", data = teoACF) |>
      hcpxy_update_series(id = "sacf", data = smpACF)
    
    teoPACF <- as.numeric(
      ARMAacf(
        # ar = 0.2,
        # ma = 0.2,
        ar = ifelse(!is.null(input$ar), input$ar, AR),
        ma = ifelse(!is.null(input$ma), input$ma, MA),
        lag.max = LAG_MAX,
        pacf = TRUE
      )
    )
    
    smpPACF <- as.numeric(pacf(head(ts, STR_OBS), lag.max = LAG_MAX, plot = FALSE)$acf)
    
    highchartProxy("pacf") |>
      hcpxy_update_series(id = "tpacf", data = teoPACF) |>
      hcpxy_update_series(id = "spacf", data = smpPACF)
    
  })

  observe({

    interval <- max(as.numeric(input$interval), 0.25)

    invalidateLater(1000 * interval, session)

    animation <- TRUE

    value_to_add <- isolate(value()) + 1
    value(value_to_add)

    serie_actual <- head(ts(), value_to_add)

    smpACF <- as.numeric(
      acf(
        serie_actual,
        lag.max = LAG_MAX,
        plot = FALSE
      )$acf
    )

    smpPACF <- as.numeric(
      pacf(
        serie_actual,
        lag.max = LAG_MAX,
        plot = FALSE
      )$acf
    )

    highchartProxy("acf") |>
      hcpxy_update_series(
        id = "sacf",
        data = smpACF
      )

    highchartProxy("pacf") |>
      hcpxy_update_series(
        id = "spacf",
        data = smpPACF
      )

    highchartProxy("ts") |>
      hcpxy_add_point(
        id = "ts",
        point = list(
          x = value_to_add,
          y = ts()[value_to_add]
        ),
        animation = animation
      )
  })

  output$acf <- renderHighchart({
    
    highchart() |>
      hc_chart(type = "column") |>
      hc_yAxis(min = -1, max = 1) |>
      hc_add_series(
        data = smpACF,
        id = "sacf",
        name = "Estimated"
      ) |>
      hc_add_series(
        data = teoACF, 
        id = "tacf",
        name = "Theoretical"
      ) |>
      hc_tooltip(
        table = TRUE,
        headerFormat = "<small>Lag {point.key}</small><table>",
        valueDecimals = 3
      ) |>
      hc_plotOptions(
        series = list(
          pointWidth = 5,
          animation = list(duration = DURATION),
          marker = list(symbol = "circle")
        )
      )
    
  })
  
  output$pacf <- renderHighchart({
    
    highchart() |>
      hc_chart(type = "column") |>
      hc_yAxis(min = -1, max = 1) |>
      hc_add_series(
        data = smpPACF,
        id = "spacf",
        name = "Estimated"
      ) |>
      hc_add_series(
        data = teoPACF, 
        id = "tpacf",
        name = "Theoretical"
      ) |>
      hc_tooltip(
        table = TRUE,
        headerFormat = "<small>Lag {point.key}</small><table>",
        valueDecimals = 3
      ) |>
      hc_plotOptions(
        series = list(
          pointStart = 1,
          pointWidth = 5,
          animation = list(duration = DURATION),
          marker = list(symbol = "circle")
        )
      )
  })
  
}

shinyApp(ui, server)
