# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)
library(klassets) # remotes::install_github("jbkunst/klassets")
library(markdown)
library(ggforce)
library(deldir)

# theme options -----------------------------------------------------------
thematic::thematic_shiny(font = "auto")

theme_set(theme_minimal() + theme(legend.position = "bottom"))

apptheme <- bs_theme()

sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

primary_color <- unname(bs_get_variables(apptheme, c("primary")))

# app options -------------------------------------------------------------
KMAX <- 5

cluster_colors <- function(k) {
  colors <- viridisLite::viridis(k, begin = 0.1, end = 0.9)
  purrr::set_names(colors, LETTERS[seq_len(k)])
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "K-means",
      withMathJax(),
      # tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      
      # "Algorithm parameters",
      sliderInput(
        "k", tags$small("Parameter \\(k\\) for \\(K\\)-Means"), value = 4, min = 2, max = KMAX, ticks = TRUE
      ),
      # shinyWidgets::sliderTextInput(
      sliderInput(
        "iter",
        tags$small("Iterations of algorithm"), 
        # choices = 0:15,
        min = 1, max = 15,
        ticks = FALSE,
        value = 4, 
        animate = animationOptions(interval = 3000)
      ),
      accordion(
        multiple = FALSE,
        open = FALSE,
        accordion_panel(
          "Simulate data",
          sliderInput(
            "n_groups", tags$small("Number of groups to simulate"), value = 3, min = 1, max = KMAX
          ),
          sliderInput(
            "n", tags$small("Number of points to simulate"), value = 200, min = 100, max = 500, step = 100
          ),
          actionButton("button", "Generate", class = "btn-primary btn-sm")
        ),
        accordion_panel(
          "About K-means",
          tags$small(htmltools::includeMarkdown("about.md"))
        )
      ),
      tags$small(htmltools::includeMarkdown("resources.md"))
      ),
    
    layout_columns(
      col_widths = c(8, 4, 4, 4, 4),
      row_heights = c(3, 2),
      card(card_header(uiOutput("iter_title")), card_body(plotOutput("iter_plot"))),
      card(card_header("Generated vs assigned"), card_body(plotOutput("assignment_heatmap"))),
      card(card_header("Within-cluster variation"), card_body(plotOutput("wc"))),
      card(card_header("Convergence"), card_body(plotOutput("convergence"))),
      card(card_header("Elbow plot"), card_body(plotOutput("elbow")))
      )
    )
  )

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # input <- list(n_groups = 4, n = 150, k = 3, iter = 2); input
  # input <- list(n_groups = 3, n = 500, k = 5, iter = 2); input
  # input <- list(n_groups = 3, n = 200, k = 4, iter = 2); input
  
  simulated_data <- eventReactive(input$button, {
    set.seed(123)
    
    list(
      id = input$button,
      n = input$n,
      n_groups = input$n_groups,
      data = klassets::sim_groups(n = input$n, groups = input$n_groups)
    )
  }, ignoreNULL = FALSE)
  
  output$iter_title <- renderUI({
    simulated_data <- simulated_data()
    
    str_glue("Iteration #{input$iter}: {input$k} means ({simulated_data$n_groups} groups)")
  })
  
  # k means iterations all
  kmi_all <- reactive({
    
    simulated_data <- simulated_data()
    data <- simulated_data$data
    
    showNotification(str_glue("Calculating internal iterations"))
    
    kmi_all <- map(1:KMAX, function(k){
      cli::cli_inform(str_glue("{k} centers"))
      klassets::kmeans_iterations(df = data, centers = k)
    })
    
    kmi_all
    
  }) |> bindCache(simulated_data()$n, simulated_data()$n_groups)
  
  # k means iteration (for actual selected k)
  kmi <- reactive({
    
    kmi_all <- kmi_all()
    
    kmi_all[[input$k]]
    
  })
  
  last_simulated_id <- reactiveVal(NULL)
  last_k <- reactiveVal(NULL)
  
  observeEvent(kmi(), {
    
    kmi <- kmi()
    simulated_data <- simulated_data()
    max_iter <- max(kmi$centers$iteration)
    is_new_simulation <- !identical(simulated_data$id, last_simulated_id())
    is_new_k <- !identical(input$k, last_k())
    selected_iter <- ifelse(is_new_simulation || is_new_k, 1, min(input$iter, max_iter))
    
    updateSliderInput(
      inputId = "iter",
      max = max_iter,
      value = selected_iter
    )
    
    last_simulated_id(simulated_data$id)
    last_k(input$k)
  })
  
  # kmi_plot <- reactive({
  #   
  #   kmi <- kmi()
  #   
  #   kmi_plot <- plot(kmi)
  #   
  #   kmi_plot
  #   
  # })
  
  data_hist_all <- reactive({
    
    kmi_all <- kmi_all()
    
    ks <- 1:KMAX
    
    data_hist_all <- kmi_all |> 
      map(pluck, "points") |> 
      # map(filter, iteration == max(iteration)) |> 
      # map(select, -iteration, -id) |> 
      map2_df(ks, ~ mutate(.x, k = .y, .before = 1))
    
    data_hist_all <- data_hist_all |> 
      # filter(k == 2) |> 
      # distance from cluster
      group_by(k, iteration, cluster) |> 
      mutate(xc = mean(x), yc = mean(y)) |> 
      mutate(dc = (x - xc)^2 + (y - yc)^2) |> 
      # distance from total center
      ungroup() |> 
      group_by(k, iteration) |> 
      mutate(xt = mean(x), yt = mean(y)) |> 
      mutate(dt = (x - xt)^2 + (y - yt)^2) |> 
      ungroup()
    
    data_hist_all
    
  }) |> bindCache(simulated_data()$n, simulated_data()$n_groups)
  
  data_elbow <- reactive({
    
    data_hist_all <- data_hist_all()
    
    data_elbow <- data_hist_all |> 
      group_by(k, iteration) |> 
      summarise(dc = sum(dc), dt = sum(dt), .groups = "drop") |> 
      mutate(wc = 1 - dc/dt) |> 
      ungroup()
    
    data_elbow
    
  }) |> bindCache(simulated_data()$n, simulated_data()$n_groups)
  
  output$iter_plot <- renderPlot({
    
    # kmi_plot <- kmi_plot()
    # 
    # kmi_plot +
    #   ggforce::facet_wrap_paginate(
    #     vars(iteration),
    #     nrow = 1,
    #     ncol = 1,
    #     page = input$iter + 0
    #   )
    
    kmi <- kmi()
    
    dpoints  <- kmi$points  |> filter(iteration == input$iter)
    dcenters <- kmi$centers |> filter(iteration == input$iter - 1)
    
    # xmin, xmax, ymin, ymax.
    bnd <- dpoints |>
      summarise(
        x1 = min(pretty(dpoints$x)),
        x2 = max(pretty(dpoints$x)),
        y1 = min(pretty(dpoints$y)),
        y2 = max(pretty(dpoints$y))
      ) |> 
      pivot_longer(cols = everything()) |> 
      pull(value)
    
    bnd
    
    k      <- nrow(dplyr::count(dcenters, cluster))
    colors <- cluster_colors(k)
    
    p <- ggplot() +
      ggforce::geom_voronoi_segment(
        data = dcenters,
        aes(cx, cy),
        alpha = 0.2, bound = bnd
      ) +
      geom_point(
        data = dpoints,
        aes(x, y, group = id, color = cluster, shape = group),
        size = 2#, alpha = 0.5
        ) + 
      geom_point(
        data = dcenters,
        aes(cx, cy, group = cluster, fill = cluster),
        size = 7, alpha = 1, shape = 21, show.legend = FALSE, color = "white"
        ) + 
      labs(
        shape = "Original\nGroup",
        color = "Assigned\nCluster",
        x = NULL,
        y = NULL
        ) +
      scale_color_manual(values = colors, name = "Assigned\nCluster", na.value = "gray70") +
      scale_fill_manual(values = colors, name = "Assigned\nCluster", na.value = "gray70") +
      theme(legend.position = "right")
    
    p
    
  }) |> bindCache(simulated_data()$n, simulated_data()$n_groups, input$k, input$iter)
  
  output$assignment_heatmap <- renderPlot({
    
    kmi <- kmi()
    
    dpoints  <- kmi$points  |> filter(iteration == input$iter)
    
    group_levels <- str_glue("G{sort(unique(dpoints$group))}")
    cluster_levels <- LETTERS[seq_len(input$k)]
    colors <- cluster_colors(input$k)
    
    dpoints |>
      count(group, cluster) |> 
      mutate(
        group = factor(str_glue("G{group}"), levels = group_levels),
        cluster = factor(cluster, levels = cluster_levels)
      ) |> 
      complete(group, cluster, fill = list(n = 0)) |> 
      ggplot(aes(group, cluster)) +
      geom_tile(aes(fill = cluster, alpha = n), color = "white", linewidth = 1) +
      geom_text(aes(label = ifelse(n == 0, "", n)), fontface = "bold") +
      scale_fill_manual(values = colors, guide = "none") +
      scale_alpha_continuous(range = c(0.15, 1), guide = "none") +
      labs(x = "Generated group", y = "Assigned cluster") +
      coord_equal() +
      theme(
        panel.grid = element_blank(),
        legend.position = "none"
      )
    
  }) |> bindCache(simulated_data()$n, simulated_data()$n_groups, input$k, input$iter)
  
  output$wc <- renderPlot({
    
    data_hist_all <- data_hist_all()
    
    dwck <- data_hist_all |>
      filter(k == input$k, iteration > 1) |>
      group_by(iteration, cluster) |>
      summarise(wck = sum(dc), .groups = "drop")
    
    k      <- input$k
    colors <- cluster_colors(k)
    
    ggplot(dwck) +
      geom_vline(aes(xintercept = input$iter), linewidth = 3, alpha = 0.25, color = primary_color) +
      geom_col(aes(iteration, wck, fill = cluster), position = position_stack()) +
      scale_x_continuous(breaks = unique(dwck$iteration), minor_breaks = NULL) +
      scale_fill_manual(values = colors, name = "", na.value = "gray90") +
      labs(y = expression("Sum of W(C_k)")) +
      theme(legend.position = "none")
    
  }) |> bindCache(simulated_data()$n, simulated_data()$n_groups, input$k, input$iter)
  
  output$convergence <- renderPlot({
    
    data_elbow <- data_elbow()
    
    daux <- data_elbow |> 
      filter(k == input$k, iteration > 0) |> 
      ungroup()
    
    # ggplot(data_elbow, aes(iteration, 1 - wc)) +
    #   geom_line(size = 1.2, color = "gray60") +
    #   facet_wrap(vars(k)) +
    #   scale_y_continuous(limits = c(0, 1))
    
    ggplot(daux, aes(iteration, 1 - wc)) +
      geom_line(linewidth = 1.2, color = "gray60") +
      geom_point(
        data = filter(daux, iteration == input$iter),
        shape = 21, 
        size = 5,
        color = "white", 
        fill = primary_color
      ) +
      scale_x_continuous(breaks = daux$iteration, minor_breaks = NULL) +
      scale_y_continuous(limits = c(0, 1))
    
  }) |> bindCache(simulated_data()$n, simulated_data()$n_groups, input$k, input$iter)
  
  output$elbow <- renderPlot({
    
    data_elbow <- data_elbow()
    
    daux <- data_elbow |> 
      group_by(k) |> 
      filter(iteration == max(iteration)) |> 
      ungroup()
    
    # ggplot(data_elbow, aes(k, 1 - wc)) +
    #   geom_line(size = 1.2, color = "gray60") +
    #   # kunstomverse::geom_point2
    #   geom_point(
    #     data = filter(daux, k == input$k),
    #     shape = 21, 
    #     size = 5,
    #     color = "white", 
    #     fill = primary_color
    #   ) +
    #   scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
    #   facet_wrap(vars(iteration))
   
    
    ggplot(daux, aes(k, 1 - wc)) +
      geom_line(linewidth = 1.2, color = "gray60") +
      # kunstomverse::geom_point2
      geom_point(
        data = filter(daux, k == input$k),
        shape = 21, 
        size = 5,
        color = "white", 
        fill = primary_color
      ) +
      scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
      scale_y_continuous(limits = c(0, 1))
    
  }) |> bindCache(simulated_data()$n, simulated_data()$n_groups, input$k)
  
}

shinyApp(ui, server)