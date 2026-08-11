# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)
library(markdown)

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
      title = "Decision Tree",
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
        "depth",
        tags$small("Maximum depth of the tree"),
        min = 1,
        max = 8,
        step = 1,
        value = 2
      ),
      sliderInput(
        "alpha",
        tags$small("Significance level for variable selection \\( \\alpha \\)"),
        min = 0,
        max = 1,
        step = 0.05,
        value = .1
      ),
      checkboxInput(
        "show_tree_values",
        tags$small("Show split p-values"),
        value = FALSE
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
      card(plotOutput("tree_plot", width = "100%", height = "100%")),
      card(plotOutput("roc_plot", width = "100%", height = "100%")),
      card(plotOutput("confusion_plot", width = "100%", height = "100%"))
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
  #   depth = 8,
  #   alpha = 0.05,
  #   show_tree_values = FALSE
  # ); input
  
  dxy <- reactive({
    
    set.seed(1234)
    
    dxy <- klassets::sim_response_xy(
      n = as.integer(input$n),
      x_dist = purrr::partial(runif, min = -1, max = 1),
      relationship = function(x, y) eval(parse(text = input$relationship)),
      noise = input$percent_noise/100
    )
    
    dxy <- klassets::fit_classification_tree(
      dxy, 
      maxdepth = input$depth, 
      alpha = input$alpha
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
  
  output$tree_plot <- renderPlot({
    
    dxy <- dxy()
    
    tree_colors <- unname(class_palette[sort(unique(as.character(dxy$response)))])
    
    plot(
      attr(dxy, "model"),
      terminal_panel = partykit::node_barplot,
      tp_args = list(fill = tree_colors, col = tree_colors, id = FALSE),
      ip_args = list(id = FALSE, pval = input$show_tree_values, fill = "white")
    )
    
  })
  
  output$roc_plot <- renderPlot({
    
    dxy <- dxy()
    
    # `prediction` is aligned with the first response level; `1 - prediction`
    # plots the ROC curve using TRUE as the positive class.
    droc <- risk3r::roc_data(
      actual = as.numeric(dxy$response),
      predicted = 1 - dxy$prediction
    )
    
    aucroc <- Metrics::auc(
      actual = as.numeric(dxy$response),
      predicted = dxy$prediction
    )
    
    ggplot(droc) +
      geom_line(aes(x, y), linewidth = 2, color = primary_color) +
      labs(title = str_glue("AUC: { percent(aucroc)  }")) +
      theme(legend.position = "none") +
      ggplot2::labs(x = "False positive rate (FPR)", 
                    y = "True positive rate (TPR)")
    
  })
  
  output$confusion_plot <- renderPlot({
    
    dxy <- dxy()
    
    dxy2 <- klassets::fit_classification_tree(
      dxy, 
      type = "response",
      maxdepth = input$depth, 
      alpha = input$alpha
    )
    
    class_levels <- sort(unique(c(as.character(dxy2$response), as.character(dxy2$prediction))))
    class_colors <- class_palette[class_levels]
    class_symbols <- setNames(c("x", "o")[seq_along(class_levels)], class_levels)
    response_labels <- str_glue("{class_symbols[rev(class_levels)]}  {rev(class_levels)}")
    
    dcm <- dxy2 |> 
      mutate(
        response = factor(as.character(response), levels = rev(class_levels)),
        prediction = factor(as.character(prediction), levels = class_levels)
      ) |>
      count(response, prediction, name = "n") |> 
      complete(response, prediction, fill = list(n = 0)) |>
      mutate(
        p = n / sum(n),
        lbl = str_glue("{comma(n)}\n{percent(p)}"),
        x = as.integer(prediction),
        y = as.integer(response)
      )
    
    ggplot(dcm, aes(x, y)) +
      geom_tile(aes(fill = prediction, alpha = p), color = "white", linewidth = 1.5, width = 0.96, height = 0.96) +
      geom_text(aes(label = lbl), fontface = "bold", lineheight = 0.9, size = 4.5) +
      scale_fill_manual(values = class_colors, name = "Predicted") +
      scale_alpha(range = c(0.2, 0.75), guide = "none") +
      scale_x_continuous(breaks = seq_along(class_levels), labels = class_levels) +
      scale_y_continuous(breaks = seq_along(rev(class_levels)), labels = response_labels) +
      coord_equal() +
      labs(title = "Confusion matrix", x = "Predicted response", y = "Observed response") +
      theme(
        panel.grid = element_blank(),
        legend.position = "bottom"
      )
    
  })
  
}

shinyApp(ui, server)
