# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(dplyr)
library(markdown)
library(shinyWidgets)

# theme options -----------------------------------------------------------
apptheme <- bs_theme(
  "tooltip-bg" = "$white",
  "tooltip-color" = "$body-color",
  "tooltip-opacity" = 1
) |>
  bs_add_rules(
    ".tooltip-inner {
      border: 1px solid $border-color;
      box-shadow: $box-shadow-sm;
    }"
  )

sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

thematic::thematic_shiny(font = "auto")

theme_set(theme_minimal() + theme(legend.position = "none"))

# app options -------------------------------------------------------------
generate_lorenz <- function(sigma = 10, rho = 28, beta = 8/3, 
                             start = c(1, 1, 1), n = 1000, dt = 0.01) {
  x <- y <- z <- numeric(n)
  x[1] <- start[1]
  y[1] <- start[2]
  z[1] <- start[3]
  
  for (i in 2:n) {
    dx <- sigma * (y[i-1] - x[i-1])
    dy <- x[i-1] * (rho - z[i-1]) - y[i-1]
    dz <- x[i-1] * y[i-1] - beta * z[i-1]
    
    x[i] <- x[i-1] + dx * dt
    y[i] <- y[i-1] + dy * dt
    z[i] <- z[i-1] + dz * dt
  }
  
  data.frame(x = x, y = y, z = z, time = 1:n)
}

# ui ---------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Lorenz System Parameters",
      withMathJax(),
      sliderInput(
        "sigma",
        tagList(
          "\\( \\sigma \\) (sigma):",
          bslib::tooltip(
            bsicons::bs_icon("info-circle", class = "ms-1", title = "About sigma"),
            "Controls how quickly x follows y.",
            placement = "right",
            options = list(trigger = "hover focus click")
          )
        ),
        min = 1,
        max = 20,
        value = 10,
        step = 0.1
      ),
      sliderInput(
        "rho",
        tagList(
          "\\( \\rho \\) (rho):",
          bslib::tooltip(
            bsicons::bs_icon("info-circle", class = "ms-1", title = "About rho"),
            "Controls the system's driving strength and whether chaotic behavior emerges.",
            placement = "right",
            options = list(trigger = "hover focus click")
          )
        ),
        min = 1,
        max = 50,
        value = 28,
        step = 0.1
      ),
      sliderInput(
        "beta",
        tagList(
          "\\( \\beta \\) (beta):",
          bslib::tooltip(
            bsicons::bs_icon("info-circle", class = "ms-1", title = "About beta"),
            "Controls damping in the z direction.",
            placement = "right",
            options = list(trigger = "hover focus click")
          )
        ),
        min = 0.1,
        max = 10,
        value = 8/3,
        step = 0.1
      ),
      shinyWidgets::sliderTextInput(
        "n_points",
        "Number of points:",
        choices = c(100, 250, 500, 1000, 2000, 5000),
        selected = 1000,
        grid = TRUE,
        force_edges = TRUE
      ),
      shinyWidgets::sliderTextInput(
        "dt",
        tagList(
          "Time step (dt):",
          bslib::tooltip(
            bsicons::bs_icon("info-circle", class = "ms-1", title = "About time step"),
            "Larger steps cover more time per point but reduce numerical accuracy.",
            placement = "right",
            options = list(trigger = "hover focus click")
          )
        ),
        choices = c("0.001", "0.002", "0.005", "0.01", "0.02", "0.05"),
        selected = "0.01",
        grid = TRUE,
        force_edges = TRUE
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
      col_widths = c(12, 4, 4, 4),
      row_heights = c(3, 2),
      card(
        card_header("3D Lorenz Attractor"),
        plotlyOutput("lorenz3d", height = "100%")
      ),
      card(
        card_header("X-Y Projection"),
        plotOutput("xy_plot")
      ),
      card(
        card_header("X-Z Projection"),
        plotOutput("xz_plot")
      ),
      card(
        card_header("Y-Z Projection"),
        plotOutput("yz_plot")
      )
    )
  )
)

# server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  lorenz_data <- reactive({
    generate_lorenz(
      sigma = input$sigma,
      rho = input$rho,
      beta = input$beta,
      n = as.numeric(input$n_points),
      dt = as.numeric(input$dt)
    )
  })
  
  output$lorenz3d <- renderPlotly({
    df <- lorenz_data()
    
    plot_ly(df, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
            line = list(width = 2, color = ~time, colorscale = 'Viridis')) %>%
      layout(scene = list(
        xaxis = list(title = "X"),
        yaxis = list(title = "Y"),
        zaxis = list(title = "Z")
      ))
  })
  
  output$xy_plot <- renderPlot({
    df <- lorenz_data()
    ggplot(df, aes(x = x, y = y, color = time)) +
      geom_path() +
      scale_color_viridis_c() +
      labs(title = "X-Y Projection") +
      coord_equal()
  })
  
  output$xz_plot <- renderPlot({
    df <- lorenz_data()
    ggplot(df, aes(x = x, y = z, color = time)) +
      geom_path() +
      scale_color_viridis_c() +
      labs(title = "X-Z Projection") +
      coord_equal()
  })
  
  output$yz_plot <- renderPlot({
    df <- lorenz_data()
    ggplot(df, aes(x = y, y = z, color = time)) +
      geom_path() +
      scale_color_viridis_c() +
      labs(title = "Y-Z Projection") +
      coord_equal()
  })
}

shinyApp(ui, server)
