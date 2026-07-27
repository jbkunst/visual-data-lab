# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(klassets)
library(deldir)
library(markdown)

# theme options -----------------------------------------------------------
thematic::thematic_shiny(font = "auto")

apptheme <- bs_theme()

sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

primary_color <- unname(bs_get_variables(apptheme, c("primary")))

# app options -------------------------------------------------------------
KMAX <- 10

# helpers -----------------------------------------------------------------
include_md <- function(path) {
  if (file.exists(path)) {
    htmltools::includeMarkdown(path)
  } else {
    NULL
  }
}

cluster_colors <- function(k) {
  colors <- viridisLite::viridis(k, begin = 0.1, end = 0.9)
  purrr::set_names(colors, LETTERS[seq_len(k)])
}

group_shapes <- function(groups) {
  group_values <- sort(unique(stats::na.omit(groups)))
  pch_values <- c(16, 17, 15, 3, 4, 8, 7, 9)
  
  pch <- pch_values[seq_along(group_values)]
  names(pch) <- as.character(group_values)
  
  pch
}

alpha_col <- function(col, alpha = 0.25) {
  if (length(col) == 1 && length(alpha) > 1) {
    col <- rep(col, length(alpha))
  }
  
  if (length(alpha) == 1 && length(col) > 1) {
    alpha <- rep(alpha, length(col))
  }
  
  mapply(
    grDevices::adjustcolor,
    col = col,
    alpha.f = alpha,
    USE.NAMES = FALSE
  )
}

get_bounds <- function(dpoints) {
  dpoints <- dpoints |>
    filter(!is.na(x), !is.na(y))
  
  if (nrow(dpoints) == 0) {
    return(c(-1, 1, -1, 1))
  }
  
  c(
    min(pretty(dpoints$x)),
    max(pretty(dpoints$x)),
    min(pretty(dpoints$y)),
    max(pretty(dpoints$y))
  )
}

get_voronoi_segments <- function(dcenters, bnd) {
  dcenters <- dcenters |>
    filter(!is.na(cx), !is.na(cy))
  
  if (nrow(dcenters) < 2) {
    return(tibble(x1 = numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric()))
  }
  
  out <- tryCatch(
    deldir::deldir(x = dcenters$cx, y = dcenters$cy, rw = bnd),
    error = function(e) NULL
  )
  
  if (is.null(out)) {
    return(tibble(x1 = numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric()))
  }
  
  out$dirsgs |>
    as_tibble() |>
    transmute(x1, y1, x2, y2)
}

plot_kmeans_tiny <- function(
  dpoints,
  dcenters,
  dvor,
  colors,
  keep_aspect = FALSE,
  show_legend = TRUE
) {
  bnd <- get_bounds(dpoints)
  pch_map <- group_shapes(dpoints$group)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mar = c(2.6, 2.8, 1.2, 8), xpd = NA)
  
  plot_args <- list(
    x = dpoints$x,
    y = dpoints$y,
    type = "n",
    xlim = bnd[1:2],
    ylim = bnd[3:4],
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  
  if (keep_aspect) {
    plot_args$asp <- 1
  }
  
  do.call(plot, plot_args)
  
  xat <- pretty(bnd[1:2])
  yat <- pretty(bnd[3:4])
  
  abline(v = xat, col = "gray92", lty = "dotted", lwd = 1)
  abline(h = yat, col = "gray92", lty = "dotted", lwd = 1)
  
  axis(1, at = xat, labels = xat, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75)
  axis(2, at = yat, labels = yat, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75, las = 1)
  
  box(col = "gray85")
  
  if (nrow(dvor) > 0) {
    segments(dvor$x1, dvor$y1, dvor$x2, dvor$y2, col = alpha_col("gray40", 0.25), lwd = 1)
  }
  
  point_cols <- colors[as.character(dpoints$cluster)]
  point_cols[is.na(point_cols)] <- "gray70"
  
  points(
    dpoints$x, dpoints$y,
    pch = pch_map[as.character(dpoints$group)],
    col = point_cols,
    cex = 1.15
  )
  
  center_cols <- colors[as.character(dcenters$cluster)]
  center_cols[is.na(center_cols)] <- "gray70"
  
  points(
    dcenters$cx, dcenters$cy,
    pch = 21,
    bg = center_cols,
    col = "white",
    cex = 2.9,
    lwd = 1.5
  )
  
  if (show_legend) {
    usr <- par("usr")
    x_leg <- usr[2] + diff(usr[1:2]) * 0.035
    
    legend(
      x = x_leg,
      y = usr[4] - diff(usr[3:4]) * 0.02,
      legend = names(colors),
      title = "Assigned\nCluster",
      col = colors,
      pch = 16,
      bty = "n",
      cex = 0.8,
      pt.cex = 0.9,
      y.intersp = 0.9,
      x.intersp = 0.6
    )
    
    legend(
      x = x_leg,
      y = usr[3] + diff(usr[3:4]) * 0.24,
      legend = paste0("G", names(pch_map)),
      title = "Original\nGroup",
      col = "gray25",
      pch = unname(pch_map),
      bty = "n",
      cex = 0.8,
      pt.cex = 0.9,
      y.intersp = 0.9,
      x.intersp = 0.6
    )
  }
}

plot_assignment_initial_tiny <- function(dpoints) {
  group_levels <- paste0("G", sort(unique(dpoints$group)))
  
  counts <- dpoints |>
    count(group, name = "n") |>
    mutate(group = paste0("G", group)) |>
    right_join(tibble(group = group_levels), by = "group") |>
    mutate(n = coalesce(n, 0L))
  
  max_n <- max(c(counts$n, 1))
  alpha_vals <- 0.15 + 0.85 * counts$n / max_n
  fills <- alpha_col("gray45", alpha_vals)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mar = c(3.2, 3.4, 1, 1))
  
  plot(
    NA,
    xlim = c(0.5, length(group_levels) + 0.5),
    ylim = c(0.5, 1.5),
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  
  ix <- seq_along(group_levels)
  
  rect(ix - 0.5, 0.5, ix + 0.5, 1.5, col = fills, border = "white", lwd = 2)
  text(ix, 1, labels = counts$n, font = 2, cex = 0.9)
  
  axis(1, at = ix, labels = group_levels, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.8)
  axis(2, at = 1, labels = "Initial", col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.8, las = 1)
  
  mtext("Generated group", side = 1, line = 2.1, cex = 0.8)
  mtext("Before assignment", side = 2, line = 2.1, cex = 0.8)
}

plot_assignment_heatmap_tiny <- function(dpoints, k, colors, iter) {
  cluster_ok <- !all(is.na(dpoints$cluster))
  cluster_ok <- cluster_ok && any(as.character(dpoints$cluster) %in% LETTERS[seq_len(k)], na.rm = TRUE)
  
  if (iter == 1 || !cluster_ok) {
    plot_assignment_initial_tiny(dpoints)
    return(invisible(NULL))
  }
  
  group_levels <- paste0("G", sort(unique(dpoints$group)))
  cluster_levels <- LETTERS[seq_len(k)]
  
  counts <- dpoints |>
    mutate(cluster = as.character(cluster)) |>
    filter(cluster %in% cluster_levels) |>
    count(group, cluster, name = "n") |>
    mutate(group = paste0("G", group))
  
  heat <- tidyr::expand_grid(group = group_levels, cluster = cluster_levels) |>
    left_join(counts, by = c("group", "cluster")) |>
    mutate(n = coalesce(n, 0L))
  
  max_n <- max(c(heat$n, 1))
  alpha_vals <- 0.15 + 0.85 * heat$n / max_n
  fills <- mapply(alpha_col, colors[heat$cluster], alpha_vals, USE.NAMES = FALSE)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mar = c(3.2, 3.4, 1, 1))
  
  plot(
    NA,
    xlim = c(0.5, length(group_levels) + 0.5),
    ylim = c(0.5, length(cluster_levels) + 0.5),
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  
  ix <- match(heat$group, group_levels)
  iy <- match(heat$cluster, cluster_levels)
  
  rect(ix - 0.5, iy - 0.5, ix + 0.5, iy + 0.5, col = fills, border = "white", lwd = 2)
  text(ix, iy, labels = ifelse(heat$n == 0, "", heat$n), font = 2, cex = 0.9)
  
  axis(1, at = seq_along(group_levels), labels = group_levels, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.8)
  axis(2, at = seq_along(cluster_levels), labels = cluster_levels, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.8, las = 1)
  
  mtext("Generated group", side = 1, line = 2.1, cex = 0.8)
  mtext("Assigned cluster", side = 2, line = 2.1, cex = 0.8)
}

plot_wc_tiny <- function(dwck, iter, colors, primary_color) {
  cluster_levels <- names(colors)
  iteration_levels <- seq(1, max(c(dwck$iteration, iter, 1), na.rm = TRUE))
  
  dwck_complete <- tidyr::expand_grid(
    cluster = cluster_levels,
    iteration = iteration_levels
  ) |>
    left_join(
      dwck |>
        mutate(cluster = as.character(cluster)) |>
        filter(cluster %in% cluster_levels) |>
        group_by(cluster, iteration) |>
        summarise(wck = sum(wck), .groups = "drop"),
      by = c("cluster", "iteration")
    ) |>
    mutate(wck = coalesce(wck, 0))
  
  ymax <- max(c(dwck_complete$wck, 1)) * 1.15
  x_pos <- seq_along(iteration_levels)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mar = c(3.4, 4.1, 1, 1))
  
  plot(
    NA,
    xlim = c(0.5, length(iteration_levels) + 0.5),
    ylim = c(0, ymax),
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  
  yat <- pretty(c(0, ymax))
  abline(h = yat, col = "gray92", lty = "dotted", lwd = 1)
  
  if (iter %in% iteration_levels) {
    x_current <- match(iter, iteration_levels)
    rect(x_current - 0.32, 0, x_current + 0.32, ymax, col = alpha_col(primary_color, 0.25), border = NA)
  }
  
  for (j in seq_along(iteration_levels)) {
    bottom <- 0
    iteration_j <- iteration_levels[j]
    
    for (cluster_j in cluster_levels) {
      value <- dwck_complete |>
        filter(iteration == iteration_j, cluster == cluster_j) |>
        pull(wck)
      
      if (length(value) == 0 || is.na(value)) {
        value <- 0
      }
      
      top <- bottom + value
      
      if (value > 0) {
        rect(
          j - 0.32,
          bottom,
          j + 0.32,
          top,
          col = colors[[cluster_j]],
          border = NA
        )
      }
      
      bottom <- top
    }
  }
  
  axis(1, at = x_pos, labels = iteration_levels, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75)
  axis(2, at = yat, labels = yat, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75, las = 1)
  
  box(col = "gray85")
  mtext("Iterations", side = 1, line = 2.1, cex = 0.8)
  mtext(expression("Sum of W(C"[k]*")"), side = 2, line = 2.7, cex = 0.8)
}

plot_convergence_tiny <- function(daux, iter, primary_color) {
  daux <- daux |>
    mutate(error = 1 - wc)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mar = c(3.4, 3.4, 1, 1))
  
  plot(
    x = daux$iteration,
    y = daux$error,
    type = "n",
    xlim = range(daux$iteration),
    ylim = c(0, 1),
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  
  xat <- sort(unique(daux$iteration))
  yat <- pretty(c(0, 1))
  
  abline(v = xat, col = "gray92", lty = "dotted", lwd = 1)
  abline(h = yat, col = "gray92", lty = "dotted", lwd = 1)
  
  lines(daux$iteration, daux$error, lwd = 2, col = "gray60")
  
  dsel <- daux |>
    filter(iteration == iter)
  
  if (nrow(dsel) > 0) {
    points(dsel$iteration, dsel$error, pch = 21, bg = primary_color, col = "white", cex = 2.2, lwd = 1.5)
  }
  
  axis(1, at = xat, labels = xat, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75)
  axis(2, at = yat, labels = yat, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75, las = 1)
  
  mtext("Iterations", side = 1, line = 2.1, cex = 0.8)
  box(col = "gray85")
}

plot_elbow_tiny <- function(daux, k_selected, primary_color) {
  daux <- daux |>
    mutate(error = 1 - wc)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mar = c(3.4, 3.4, 1, 1))
  
  plot(
    x = daux$k,
    y = daux$error,
    type = "n",
    xlim = range(daux$k),
    ylim = c(0, 1),
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  
  xat <- sort(unique(daux$k))
  yat <- pretty(c(0, 1))
  
  abline(v = xat, col = "gray92", lty = "dotted", lwd = 1)
  abline(h = yat, col = "gray92", lty = "dotted", lwd = 1)
  
  lines(daux$k, daux$error, lwd = 2, col = "gray60")
  
  dsel <- daux |>
    filter(k == k_selected)
  
  if (nrow(dsel) > 0) {
    points(dsel$k, dsel$error, pch = 21, bg = primary_color, col = "white", cex = 2.2, lwd = 1.5)
  }
  
  axis(1, at = xat, labels = xat, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75)
  axis(2, at = yat, labels = yat, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75, las = 1)
  
  mtext("Number of means", side = 1, line = 2.1, cex = 0.8)
  box(col = "gray85")
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
      
      sliderInput(
        "k",
        tags$small("Parameter \\(k\\) for \\(K\\)-Means"),
        value = 4,
        min = 2,
        max = KMAX,
        ticks = TRUE
      ),
      
      sliderInput(
        "iter",
        tags$small("Iterations of algorithm"),
        min = 1,
        max = 15,
        ticks = FALSE,
        value = 2,
        animate = animationOptions(interval = 1000)
      ),
      
      accordion(
        multiple = FALSE,
        open = FALSE,
        accordion_panel(
          "Simulate data",
          sliderInput(
            "n_groups",
            tags$small("Number of groups to simulate"),
            value = 3,
            min = 1,
            max = KMAX
          ),
          sliderInput(
            "n",
            tags$small("Number of points to simulate"),
            value = 200,
            min = 100,
            max = 1000,
            step = 100
          ),
          actionButton("button", "Generate", class = "btn-primary btn-sm")
        ),
        accordion_panel(
          "How it works",
          tags$small(include_md("readme.md"))
        ),
        accordion_panel(
          "Inspiration and resources",
          tags$small(include_md("resources.md"))
        )
      ),
      
      tags$small(include_md("credits.md"))
    ),
    
    layout_columns(
      col_widths = c(8, 4, 4, 4, 4),
      row_heights = c(3, 2),
      card(card_header(uiOutput("iter_title")), card_body(plotOutput("iter_plot", height = "100%"))),
      card(card_header("Generated vs assigned"), card_body(plotOutput("assignment_heatmap", height = "100%"))),
      card(card_header("Within-cluster variation"), card_body(plotOutput("wc", height = "100%"))),
      card(card_header("Convergence"), card_body(plotOutput("convergence", height = "100%"))),
      card(card_header("Elbow plot"), card_body(plotOutput("elbow", height = "100%")))
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  simulated_data <- eventReactive(input$button, {
    
    set.seed(1234)
    
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
  
  kmi_all <- reactive({
    simulated_data <- simulated_data()
    data <- simulated_data$data
    
    showNotification(str_glue("Calculating internal iterations"), duration = 1)
    
    map(1:KMAX, function(k) {
      klassets::kmeans_iterations(df = data, centers = k)
    })
  })
  
  kmi <- reactive({
    kmi_all()[[input$k]]
  })
  
  last_simulated_id <- reactiveVal(NULL)
  last_k <- reactiveVal(NULL)
  
  observeEvent(kmi(), {
    kmi <- kmi()
    simulated_data <- simulated_data()
    
    max_iter <- max(kmi$centers$iteration)
    is_new_simulation <- !identical(simulated_data$id, last_simulated_id())
    is_new_k <- !identical(input$k, last_k())
    selected_iter <- ifelse(is_new_simulation || is_new_k, min(2, max_iter), min(input$iter, max_iter))
    
    updateSliderInput(
      session = session,
      inputId = "iter",
      max = max_iter,
      value = selected_iter
    )
    
    last_simulated_id(simulated_data$id)
    last_k(input$k)
  })
  
  iter_data <- reactive({
    kmi <- kmi()
    
    valid_iters <- sort(unique(kmi$points$iteration))
    selected_iter <- input$iter
    
    if (!selected_iter %in% valid_iters) {
      selected_iter <- max(valid_iters[valid_iters <= selected_iter], na.rm = TRUE)
    }
    
    if (!is.finite(selected_iter)) {
      selected_iter <- min(valid_iters)
    }
    
    dpoints <- kmi$points |>
      filter(iteration == selected_iter)
    
    dcenters <- kmi$centers |>
      filter(iteration == selected_iter - 1)
    
    if (nrow(dcenters) == 0) {
      dcenters <- kmi$centers |>
        filter(iteration == min(iteration))
    }
    
    bnd <- get_bounds(dpoints)
    dvor <- get_voronoi_segments(dcenters, bnd)
    
    list(
      iter = selected_iter,
      dpoints = dpoints,
      dcenters = dcenters,
      dvor = dvor,
      colors = cluster_colors(input$k)
    )
  })
  
  data_hist_all <- reactive({
    kmi_all <- kmi_all()
    ks <- 1:KMAX
    
    kmi_all |>
      map(pluck, "points") |>
      map2_df(ks, ~ mutate(.x, k = .y, .before = 1)) |>
      group_by(k, iteration, cluster) |>
      mutate(xc = mean(x), yc = mean(y)) |>
      mutate(dc = (x - xc)^2 + (y - yc)^2) |>
      ungroup() |>
      group_by(k, iteration) |>
      mutate(xt = mean(x), yt = mean(y)) |>
      mutate(dt = (x - xt)^2 + (y - yt)^2) |>
      ungroup()
  })
  
  data_elbow <- reactive({
    data_hist_all() |>
      group_by(k, iteration) |>
      summarise(dc = sum(dc), dt = sum(dt), .groups = "drop") |>
      mutate(wc = 1 - dc / dt) |>
      ungroup()
  })
  
  output$iter_plot <- renderPlot({
    d <- iter_data()
    
    plot_kmeans_tiny(
      dpoints = d$dpoints,
      dcenters = d$dcenters,
      dvor = d$dvor,
      colors = d$colors,
      keep_aspect = FALSE
    )
  }, res = 96)
  
  output$assignment_heatmap <- renderPlot({
    d <- iter_data()
    
    plot_assignment_heatmap_tiny(
      dpoints = d$dpoints,
      k = input$k,
      colors = cluster_colors(input$k),
      iter = d$iter
    )
  }, res = 96)
  
  output$wc <- renderPlot({
    dwck <- data_hist_all() |>
      filter(k == input$k, iteration > 1) |>
      group_by(iteration, cluster) |>
      summarise(wck = sum(dc), .groups = "drop")
    
    plot_wc_tiny(
      dwck = dwck,
      iter = input$iter,
      colors = cluster_colors(input$k),
      primary_color = primary_color
    )
  }, res = 96)
  
  output$convergence <- renderPlot({
    daux <- data_elbow() |>
      filter(k == input$k, iteration > 0) |>
      ungroup()
    
    plot_convergence_tiny(
      daux = daux,
      iter = input$iter,
      primary_color = primary_color
    )
  }, res = 96)
  
  output$elbow <- renderPlot({
    daux <- data_elbow() |>
      group_by(k) |>
      filter(iteration == max(iteration)) |>
      ungroup()
    
    plot_elbow_tiny(
      daux = daux,
      k_selected = input$k,
      primary_color = primary_color
    )
  }, res = 96)
}

shinyApp(ui, server)