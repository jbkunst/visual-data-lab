# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(markdown)
library(broom)
library(risk3r)       # remotes::install_github("jbkunst/risk3r", force = TRUE)
library(klassets)     # remotes::install_github("jbkunst/klassets", force = TRUE)

# theme options -----------------------------------------------------------
apptheme <- bs_theme()

sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

thematic::thematic_shiny(font = "auto")

theme_set(theme_minimal(base_size = 14) + theme(legend.position = "bottom"))

primary_color <- unname(bs_get_variables(apptheme, c("primary")))

class_palette <- c("FALSE" = "#d98f8f", "TRUE" = "#8f95d9")

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Logistic Regression",
      withMathJax(),
      radioButtons(
        "relationship",
        tags$small("Relationship between \\(x\\), \\(y\\) and the response variable"),
        choices = list(
          "\\(x > y\\)" = "x > y",
          "\\(x^2 > y\\)" = "x^2 - y > 0",
          "\\(|x| > |y|\\)" = "abs(x) - abs(y) > 0",
          "\\(x^2 + y^2 < 0.5\\)" = "x^2 + y^2 < 0.5",
          "\\( \\sin(x \\cdot \\pi ) > \\sin(y \\cdot \\pi ) \\)" = "sin(x*pi) > sin(y*pi)"
          )
        ),
      sliderInput(
        "percent_noise",
        tags$small("Percent noise"),
        min = 0,
        max = 50,
        step = 5,
        value = 20,
        post = "%"
      ),
      sliderInput(
        "order",
        tags$span("Model Order"),
        min = 1,
        max = 4,
        step = 1,
        value = 1
      ),
      shinyWidgets::sliderTextInput(
        "n",
        tags$small("Number of observations"),
        choices = c("100", "500", "1000"),
        selected = "1000",
        grid = TRUE,
        force_edges = TRUE
      ),
      checkboxInput(
        "apply_stepwise", 
        tags$small("Apply stepwise"),
        value = FALSE
      ),
      checkboxInput(
        "show_model_field", 
        tags$small("Show model predictions"),
        value = TRUE
      ),
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
      col_widths = c(6, 6, 6, 6),
      row_heights = c(3, 2),
      gap = "0.75rem",
      card(plotOutput("join_dist", width = "100%", height = "100%")),
      card(plotOutput("coef_plot", width = "100%", height = "100%")),
      card(plotOutput("roc_plot", width = "100%", height = "100%")),
      card(plotOutput("score_plot", width = "100%", height = "100%"))
      )
    )
  )

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # input <- list(
  #   n = 500,
  #   relationship = "x > y",
  #   show_model_field = TRUE,
  #   show_xy_rel = TRUE,
  #   percent_noise = 10,
  #   order = 8,
  #   apply_stepwise = TRUE
  # ); input
  
  dxy <- reactive({
    
    dxy <- klassets::sim_response_xy(
      n = as.integer(input$n),
      x_dist = purrr::partial(runif, min = -1, max = 1),
      relationship = function(x, y) eval(parse(text = input$relationship)),
      noise = input$percent_noise/100
    )
    
    dxy <- klassets::fit_logistic_regression(
      dxy, 
      order = input$order, 
      stepwise = input$apply_stepwise
    )
    
    dxy
    
  })
  
  output$join_dist <- renderPlot({
    
    dxy <- dxy()
    
    p <- ggplot()
    
    if (input$show_model_field) {
      p <- plot(dxy)
      
    } else {
      p <- klassets:::plot.klassets_response_xy(dxy)
      
    }
    
    p
    
  })
  
  output$coef_plot <- renderPlot({
    
    dxy <- dxy()
    mod <- attr(dxy, "model")
    
    risk3r::gg_model_coef(mod) +
      labs(title = "Model coefficients", x = "Estimate", y = NULL) +
      theme(panel.grid.minor = element_blank())
    
  })
  
  output$roc_plot <- renderPlot({
    
    dxy <- dxy()
    actual <- as.integer(as.character(dxy$response) == "TRUE")
    
    droc <- risk3r::roc_data(
      actual = actual,
      predicted = dxy$prediction
    )
    
    aucroc <- Metrics::auc(
      actual = actual,
      predicted = dxy$prediction
    )
    
    ggplot(droc) +
      geom_line(aes(x, y), linewidth = 2, color = primary_color) +
      labs(title = str_glue("AUC: { percent(aucroc)  }")) +
      theme(legend.position = "none") +
      ggplot2::labs(x = "False positive rate (FPR)", 
                    y = "True positive rate (TPR)")
    
  })
  
  output$score_plot <- renderPlot({
    
    dxy <- dxy() |>
      mutate(
        response = factor(as.character(response), levels = c("FALSE", "TRUE"))
      )
    
    ggplot(dxy, aes(prediction, color = response, fill = response)) +
      geom_density(alpha = 0.25, linewidth = 1.1) +
      scale_x_continuous(labels = percent, limits = c(0, 1)) +
      scale_color_manual(values = class_palette, guide = "none") +
      scale_fill_manual(values = class_palette, guide = "none") +
      labs(
        title = "Score distribution",
        x = "Predicted probability",
        y = "Density"
      ) +
      coord_cartesian(xlim = c(0, 1)) +
      theme(panel.grid.minor = element_blank())
    
  })
  
}

shinyApp(ui, server)
