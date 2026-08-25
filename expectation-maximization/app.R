# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(purrr)
library(vdltheme)

# theme -------------------------------------------------------------------
thematic::thematic_shiny(font = "auto")

apptheme <- theme_vdl()

sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(
  bslib::card,
  full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 0)
)

group_colors <- bs_get_variables(apptheme, c("primary", "danger"))
group_colors <- setNames(unname(group_colors), c("A", "B"))

# app options -------------------------------------------------------------
PAGE_SIZE <- 20L
TOLERANCE <- 0.005
MAX_ITERATIONS <- 100L

# helpers -----------------------------------------------------------------
include_md <- function(path) {
  if (file.exists(path)) {
    htmltools::includeMarkdown(path)
  } else {
    NULL
  }
}

generate_heights <- function(n, seed = 0L) {
  set.seed(20260825L + as.integer(n) + as.integer(seed))

  n_a <- floor(n / 2)
  n_b <- n - n_a

  sort(round(c(
    rnorm(n_a, mean = 159, sd = 5.5),
    rnorm(n_b, mean = 172, sd = 6.0)
  ), 1))
}

initial_parameters <- function(x) {
  initial_sigma <- max(sd(x) * 0.8, 3)

  list(
    pi = c(A = 0.5, B = 0.5),
    mu = c(
      A = unname(quantile(x, 0.25)),
      B = unname(quantile(x, 0.75))
    ),
    sigma = c(A = initial_sigma, B = initial_sigma)
  )
}

expectation_step <- function(x, parameters) {
  density <- cbind(
    A = dnorm(x, parameters$mu[["A"]], parameters$sigma[["A"]]),
    B = dnorm(x, parameters$mu[["B"]], parameters$sigma[["B"]])
  )

  weighted_density <- sweep(density, 2, parameters$pi, FUN = "*")
  denominator <- pmax(rowSums(weighted_density), .Machine$double.xmin)
  responsibility <- weighted_density / denominator

  list(
    density = density,
    weighted_density = weighted_density,
    responsibility = responsibility,
    parameters = parameters
  )
}

maximization_step <- function(x, responsibility) {
  effective_n <- pmax(colSums(responsibility), .Machine$double.eps)
  weighted_x <- sweep(responsibility, 1, x, FUN = "*")
  mu <- colSums(weighted_x) / effective_n

  x_matrix <- matrix(x, nrow = length(x), ncol = ncol(responsibility))
  centered <- sweep(x_matrix, 2, mu, FUN = "-")
  variance <- colSums(responsibility * centered^2) / effective_n

  list(
    pi = effective_n / length(x),
    mu = mu,
    sigma = sqrt(pmax(variance, 0.25^2))
  )
}

parameter_change <- function(old, new) {
  max(abs(c(
    old$pi - new$pi,
    old$mu - new$mu,
    old$sigma - new$sigma
  )))
}

history_row <- function(iteration, parameters, change = NA_real_) {
  data.frame(
    iteration = iteration,
    pi_a = parameters$pi[["A"]],
    pi_b = parameters$pi[["B"]],
    mu_a = parameters$mu[["A"]],
    mu_b = parameters$mu[["B"]],
    sigma_a = parameters$sigma[["A"]],
    sigma_b = parameters$sigma[["B"]],
    change = change
  )
}

format_number <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, decimal.mark = ".")
}

format_probability <- function(x) {
  if (is.na(x)) {
    return("—")
  }

  paste0(format_number(100 * x, 1), "%")
}

probability_cell <- function(value, color) {
  if (is.na(value)) {
    return(tags$td(class = "probability-cell", "—"))
  }

  percentage <- max(0, min(100, 100 * value))

  tags$td(
    class = "probability-cell",
    tags$div(
      class = "probability-bar",
      style = paste0(
        "--probability:", format_number(percentage, 2), "%;",
        "--bar-color:", color, "2F;"
      ),
      format_probability(value)
    )
  )
}

parameter_panel <- function(group, parameters, effective_n) {
  tags$div(
    class = paste("parameter-group", paste0("parameter-", tolower(group))),
    tags$div(class = "parameter-title", paste("Group", group)),
    tags$div(
      class = "parameter-row",
      tags$span("Proportion π"),
      tags$strong(format_number(parameters$pi[[group]], 3))
    ),
    tags$div(
      class = "parameter-row",
      tags$span("Mean μ"),
      tags$strong(format_number(parameters$mu[[group]], 2))
    ),
    tags$div(
      class = "parameter-row",
      tags$span("Std. deviation σ"),
      tags$strong(format_number(parameters$sigma[[group]], 2))
    ),
    tags$div(
      class = "effective-n",
      if (is.na(effective_n[[group]])) {
        "Effective cases: calculated during E"
      } else {
        paste0("Effective cases from last E: ", format_number(effective_n[[group]], 2))
      }
    )
  )
}

plot_parameter_panel <- function(iteration, value_a, value_b, title, colors) {
  x_range <- range(iteration)
  if (diff(x_range) == 0) {
    x_range <- x_range + c(-0.5, 0.5)
  }

  y_range <- range(c(value_a, value_b), finite = TRUE)
  if (diff(y_range) == 0) {
    padding <- max(abs(y_range[1]) * 0.08, 0.05)
    y_range <- y_range + c(-padding, padding)
  } else {
    y_range <- y_range + c(-1, 1) * diff(y_range) * 0.12
  }

  plot(
    NA,
    xlim = x_range,
    ylim = y_range,
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  x_ticks <- pretty(range(iteration))
  y_ticks <- pretty(y_range)
  abline(h = y_ticks, col = "gray92", lty = "dotted")

  if (length(iteration) > 1) {
    lines(iteration, value_a, col = colors[["A"]], lwd = 2)
    lines(iteration, value_b, col = colors[["B"]], lwd = 2)
  }

  points(iteration, value_a, pch = 16, col = colors[["A"]], cex = 0.9)
  points(iteration, value_b, pch = 16, col = colors[["B"]], cex = 0.9)

  axis(1, at = x_ticks, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75)
  axis(2, at = y_ticks, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.75, las = 1)
  box(col = "gray85")
  title(main = title, line = 0.5, cex.main = 0.9)
  mtext("Iteration", side = 1, line = 2, cex = 0.75)
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  tags$head(
    tags$style(HTML(paste0(
      "
      .em-main {
        display: flex;
        flex-direction: column;
        height: 100%;
        gap: 0.75rem;
      }

      .flow-band {
        display: grid;
        grid-template-columns: 1fr minmax(250px, auto) 1fr;
        align-items: center;
        gap: 0.75rem;
        padding: 0.55rem 0.8rem;
        border: 1px solid var(--bs-border-color);
        border-radius: var(--bs-border-radius-lg);
        background: var(--bs-body-bg);
      }

      .flow-label:first-child { text-align: right; }
      .flow-label:last-child { text-align: left; }

      .flow-arrow {
        padding: 0.35rem 0.75rem;
        border-radius: 999px;
        text-align: center;
        font-weight: 600;
      }

      .flow-e { color: ", group_colors[["A"]], "; background: ", group_colors[["A"]], "18; }
      .flow-m { color: ", group_colors[["B"]], "; background: ", group_colors[["B"]], "18; }
      .flow-done { color: #3D683D; background: #E5F2E5; }
      .flow-message { color: var(--bs-secondary-color); font-size: 0.82rem; text-align: center; }

      .parameter-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 0.6rem;
        padding: 0.75rem;
      }

      .parameter-group {
        padding: 0.65rem 0.75rem;
        border: 1px solid;
        border-radius: var(--bs-border-radius);
      }

      .parameter-a { border-color: ", group_colors[["A"]], "55; background: ", group_colors[["A"]], "0C; }
      .parameter-b { border-color: ", group_colors[["B"]], "55; background: ", group_colors[["B"]], "0C; }
      .parameter-title { font-weight: 650; margin-bottom: 0.25rem; }
      .parameter-row { display: flex; justify-content: space-between; gap: 0.5rem; }
      .parameter-row span { color: var(--bs-secondary-color); }
      .effective-n { margin-top: 0.3rem; color: var(--bs-secondary-color); font-size: 0.75rem; }
      .iteration-note { padding: 0 0.75rem 0.65rem; color: var(--bs-secondary-color); font-size: 0.78rem; }

      .table-shell { height: 100%; display: flex; flex-direction: column; }
      .table-scroll { flex: 1; overflow: auto; }
      .em-table { width: 100%; border-collapse: collapse; font-size: 0.82rem; }
      .em-table th {
        position: sticky;
        top: 0;
        z-index: 1;
        padding: 0.42rem 0.5rem;
        background: var(--bs-tertiary-bg);
        border-bottom: 1px solid var(--bs-border-color);
        white-space: nowrap;
      }
      .em-table td {
        padding: 0.34rem 0.5rem;
        border-bottom: 1px solid var(--bs-border-color-translucent);
        text-align: right;
        white-space: nowrap;
      }
      .em-table th:first-child, .em-table td:first-child { text-align: center; }
      .em-table th:last-child, .em-table td:last-child { text-align: center; }
      .em-table tbody tr:hover { background: var(--bs-tertiary-bg); }

      .probability-cell { min-width: 88px; }
      .probability-bar {
        padding: 0.15rem 0.35rem;
        border-radius: 0.25rem;
        background: linear-gradient(
          90deg,
          var(--bar-color) 0%,
          var(--bar-color) var(--probability),
          transparent var(--probability),
          transparent 100%
        );
      }

      .table-navigation {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 0.5rem;
        padding: 0.5rem;
        border-top: 1px solid var(--bs-border-color);
      }
      .table-navigation .form-group { margin: 0; }
      .table-navigation select { min-width: 70px; }
      .table-count { color: var(--bs-secondary-color); font-size: 0.78rem; }

      @media (max-width: 900px) {
        .flow-band { grid-template-columns: 1fr; }
        .flow-label { text-align: center !important; }
        .parameter-grid { grid-template-columns: 1fr; }
      }
      "
    )))
  ),
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Expectation-Maximization",
      withMathJax(),

      selectInput(
        "n",
        "Number of observations",
        choices = c(20, 100, 500),
        selected = 20
      ),

      div(
        class = "d-grid gap-2",
        actionButton("step_e", "E step", class = "btn-primary btn-sm"),
        actionButton("step_m", "M step", class = "btn-outline-primary btn-sm"),
        actionButton("iterate", "Complete iteration", class = "btn-outline-secondary btn-sm"),
        actionButton("converge", "Run to convergence", class = "btn-outline-secondary btn-sm")
      ),

      div(
        class = "d-flex gap-2 mt-2",
        actionButton("reset", "Reset", class = "btn-light btn-sm flex-fill"),
        actionButton("simulate", "New data", class = "btn-light btn-sm flex-fill")
      ),

      checkboxInput(
        "show_calculation",
        "Show density calculation columns",
        value = FALSE
      ),

      accordion(
        multiple = FALSE,
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(include_md("readme.md"))
        )
      ),

      tags$small(include_md("credits.md"))
    ),

    div(
      class = "em-main",
      uiOutput("flow"),
      layout_columns(
        col_widths = c(7, 5),
        gap = "0.75rem",
        card(
          card_header(uiOutput("table_title")),
          div(
            class = "table-shell",
            div(class = "table-scroll", uiOutput("observations")),
            div(
              class = "table-navigation",
              actionButton("previous_page", "←", class = "btn-light btn-sm"),
              selectInput("table_page", NULL, choices = 1, selected = 1, width = "80px"),
              actionButton("next_page", "→", class = "btn-light btn-sm"),
              uiOutput("table_count")
            )
          )
        ),
        layout_columns(
          col_widths = 12,
          row_heights = c(1, 2, 2),
          gap = "0.75rem",
          card(
            card_header(uiOutput("parameter_title")),
            uiOutput("parameters")
          ),
          card(
            card_header("Observed data and estimated densities"),
            plotOutput("distribution", height = "100%")
          ),
          card(
            card_header("Parameter trajectories"),
            plotOutput("trajectory", height = "100%")
          )
        )
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  x_initial <- generate_heights(20)
  parameters_initial <- initial_parameters(x_initial)

  state <- reactiveValues(
    x = x_initial,
    parameters = parameters_initial,
    e_result = NULL,
    iteration = 0L,
    phase = "ready_e",
    message = "Apply the current parameters to the observations.",
    history = history_row(0L, parameters_initial),
    seed = 0L
  )

  reset_state <- function(new_data = FALSE) {
    if (new_data) {
      state$x <- generate_heights(as.integer(input$n), state$seed)
    }

    state$parameters <- initial_parameters(state$x)
    state$e_result <- NULL
    state$iteration <- 0L
    state$phase <- "ready_e"
    state$message <- "Apply the current parameters to the observations."
    state$history <- history_row(0L, state$parameters)
    updateSelectInput(session, "table_page", choices = 1, selected = 1)
  }

  apply_m_step <- function(parameters, change) {
    state$parameters <- parameters
    state$iteration <- state$iteration + 1L
    state$history <- rbind(
      state$history,
      history_row(state$iteration, parameters, change)
    )
  }

  observeEvent(input$n, {
    state$seed <- 0L
    reset_state(new_data = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$simulate, {
    state$seed <- state$seed + 1L
    reset_state(new_data = TRUE)
  })

  observeEvent(input$reset, {
    reset_state(new_data = FALSE)
  })

  observeEvent(input$step_e, {
    if (identical(state$phase, "ready_m")) {
      showNotification("The current E step is already complete. Run M next.")
      return()
    }

    state$e_result <- expectation_step(state$x, state$parameters)
    state$phase <- "ready_m"
    state$message <- paste0(
      "E", state$iteration + 1L,
      " calculated the membership probabilities. M can now update the parameters."
    )
  })

  observeEvent(input$step_m, {
    if (!identical(state$phase, "ready_m") || is.null(state$e_result)) {
      showNotification("Run the E step first.", type = "warning")
      return()
    }

    old_parameters <- state$parameters
    new_parameters <- maximization_step(
      state$x,
      state$e_result$responsibility
    )
    change <- parameter_change(old_parameters, new_parameters)
    apply_m_step(new_parameters, change)

    state$phase <- "ready_e"
    state$message <- paste0(
      "M", state$iteration,
      " updated the parameters. Maximum change: ",
      format_number(change, 4), "."
    )
  })

  observeEvent(input$iterate, {
    old_parameters <- state$parameters
    state$e_result <- expectation_step(state$x, old_parameters)
    new_parameters <- maximization_step(
      state$x,
      state$e_result$responsibility
    )
    change <- parameter_change(old_parameters, new_parameters)
    apply_m_step(new_parameters, change)

    state$phase <- "ready_e"
    state$message <- paste0(
      "Iteration ", state$iteration,
      " is complete. Maximum change: ",
      format_number(change, 4), "."
    )
  })

  observeEvent(input$converge, {
    parameters <- state$parameters
    history <- state$history
    iteration <- state$iteration
    final_change <- Inf
    completed <- 0L

    for (i in seq_len(MAX_ITERATIONS)) {
      e_result <- expectation_step(state$x, parameters)
      new_parameters <- maximization_step(
        state$x,
        e_result$responsibility
      )
      final_change <- parameter_change(parameters, new_parameters)
      parameters <- new_parameters
      iteration <- iteration + 1L
      completed <- completed + 1L
      history <- rbind(
        history,
        history_row(iteration, parameters, final_change)
      )

      if (final_change < TOLERANCE) {
        break
      }
    }

    converged <- final_change < TOLERANCE

    state$parameters <- parameters
    state$iteration <- iteration
    state$history <- history
    state$e_result <- expectation_step(state$x, parameters)
    state$phase <- if (converged) "converged" else "ready_e"
    state$message <- if (converged) {
      paste0(
        "Converged after ", completed,
        " additional iterations. Maximum change: ",
        format_number(final_change, 5), "."
      )
    } else {
      paste0(
        "Stopped after ", completed,
        " additional iterations. Maximum change remains ",
        format_number(final_change, 5), "."
      )
    }
  })

  observe({
    pages <- max(1L, ceiling(length(state$x) / PAGE_SIZE))
    current <- suppressWarnings(as.integer(input$table_page))
    if (length(current) == 0 || is.na(current)) {
      current <- 1L
    }

    updateSelectInput(
      session,
      "table_page",
      choices = seq_len(pages),
      selected = min(current, pages)
    )
  })

  observeEvent(input$previous_page, {
    current <- suppressWarnings(as.integer(input$table_page))
    if (length(current) == 0 || is.na(current)) {
      current <- 1L
    }
    updateSelectInput(session, "table_page", selected = max(1L, current - 1L))
  })

  observeEvent(input$next_page, {
    pages <- max(1L, ceiling(length(state$x) / PAGE_SIZE))
    current <- suppressWarnings(as.integer(input$table_page))
    if (length(current) == 0 || is.na(current)) {
      current <- 1L
    }
    updateSelectInput(session, "table_page", selected = min(pages, current + 1L))
  })

  output$flow <- renderUI({
    if (identical(state$phase, "ready_m")) {
      arrow <- "Observations + probabilities  →  Parameters"
      arrow_class <- "flow-arrow flow-m"
      next_step <- "Next: M step"
    } else if (identical(state$phase, "converged")) {
      arrow <- "Probabilities and parameters stabilized"
      arrow_class <- "flow-arrow flow-done"
      next_step <- "Convergence reached"
    } else {
      arrow <- "Observations  ←  Parameters"
      arrow_class <- "flow-arrow flow-e"
      next_step <- "Next: E step"
    }

    tagList(
      div(
        class = "flow-band",
        div(class = "flow-label", "Observation table"),
        div(class = arrow_class, arrow),
        div(class = "flow-label", "Groups A and B")
      ),
      div(class = "flow-message", next_step, " · ", state$message)
    )
  })

  output$table_title <- renderUI({
    tagList(
      "Observations and memberships",
      tags$small(
        class = "text-muted fw-normal ms-2",
        paste0(length(state$x), " cases used by EM")
      )
    )
  })

  output$parameter_title <- renderUI({
    paste0("Current parameters · iteration ", state$iteration)
  })

  output$parameters <- renderUI({
    if (is.null(state$e_result)) {
      effective_n <- c(A = NA_real_, B = NA_real_)
    } else {
      effective_n <- colSums(state$e_result$responsibility)
    }

    tagList(
      div(
        class = "parameter-grid",
        parameter_panel("A", state$parameters, effective_n),
        parameter_panel("B", state$parameters, effective_n)
      ),
      div(
        class = "iteration-note",
        "E changes the row probabilities. M changes these parameters and the density curves."
      )
    )
  })

  output$observations <- renderUI({
    n <- length(state$x)
    pages <- max(1L, ceiling(n / PAGE_SIZE))
    current_page <- suppressWarnings(as.integer(input$table_page))
    if (length(current_page) == 0 || is.na(current_page)) {
      current_page <- 1L
    }
    current_page <- min(max(1L, current_page), pages)

    first <- (current_page - 1L) * PAGE_SIZE + 1L
    last <- min(current_page * PAGE_SIZE, n)
    rows <- seq.int(first, last)

    if (is.null(state$e_result)) {
      density <- matrix(NA_real_, nrow = n, ncol = 2)
      weighted_density <- matrix(NA_real_, nrow = n, ncol = 2)
      responsibility <- matrix(NA_real_, nrow = n, ncol = 2)
      calculation_parameters <- state$parameters
    } else {
      density <- state$e_result$density
      weighted_density <- state$e_result$weighted_density
      responsibility <- state$e_result$responsibility
      calculation_parameters <- state$e_result$parameters
    }

    detailed <- isTRUE(input$show_calculation)

    headers <- if (detailed) {
      c(
        "#", "Height", "π A", "f A(x)", "π A × f A(x)",
        "π B", "f B(x)", "π B × f B(x)", "P(A)", "P(B)", "Likely"
      )
    } else {
      c("#", "Height", "P(A)", "P(B)", "Likely")
    }

    table_rows <- lapply(rows, function(i) {
      likely <- if (is.na(responsibility[i, 1])) {
        "—"
      } else if (responsibility[i, 1] >= responsibility[i, 2]) {
        "A"
      } else {
        "B"
      }

      if (detailed) {
        tags$tr(
          tags$td(i),
          tags$td(format_number(state$x[i], 1)),
          tags$td(format_number(calculation_parameters$pi[["A"]], 3)),
          tags$td(ifelse(is.na(density[i, 1]), "—", format_number(density[i, 1], 5))),
          tags$td(ifelse(is.na(weighted_density[i, 1]), "—", format_number(weighted_density[i, 1], 5))),
          tags$td(format_number(calculation_parameters$pi[["B"]], 3)),
          tags$td(ifelse(is.na(density[i, 2]), "—", format_number(density[i, 2], 5))),
          tags$td(ifelse(is.na(weighted_density[i, 2]), "—", format_number(weighted_density[i, 2], 5))),
          probability_cell(responsibility[i, 1], group_colors[["A"]]),
          probability_cell(responsibility[i, 2], group_colors[["B"]]),
          tags$td(tags$strong(likely))
        )
      } else {
        tags$tr(
          tags$td(i),
          tags$td(format_number(state$x[i], 1)),
          probability_cell(responsibility[i, 1], group_colors[["A"]]),
          probability_cell(responsibility[i, 2], group_colors[["B"]]),
          tags$td(tags$strong(likely))
        )
      }
    })

    tags$table(
      class = "em-table",
      tags$thead(tags$tr(lapply(headers, tags$th))),
      tags$tbody(table_rows)
    )
  })

  output$table_count <- renderUI({
    n <- length(state$x)
    pages <- max(1L, ceiling(n / PAGE_SIZE))
    current_page <- suppressWarnings(as.integer(input$table_page))
    if (length(current_page) == 0 || is.na(current_page)) {
      current_page <- 1L
    }
    current_page <- min(max(1L, current_page), pages)

    first <- (current_page - 1L) * PAGE_SIZE + 1L
    last <- min(current_page * PAGE_SIZE, n)

    tags$span(
      class = "table-count",
      paste0(first, "–", last, " of ", n)
    )
  })

  output$distribution <- renderPlot({
    x <- state$x
    parameters <- state$parameters

    x_grid <- seq(
      min(x, parameters$mu - 3 * parameters$sigma),
      max(x, parameters$mu + 3 * parameters$sigma),
      length.out = 400
    )

    component_a <- parameters$pi[["A"]] * dnorm(
      x_grid,
      parameters$mu[["A"]],
      parameters$sigma[["A"]]
    )
    component_b <- parameters$pi[["B"]] * dnorm(
      x_grid,
      parameters$mu[["B"]],
      parameters$sigma[["B"]]
    )
    mixture <- component_a + component_b

    breaks <- max(8L, min(30L, round(sqrt(length(x)))))
    histogram <- hist(x, breaks = breaks, plot = FALSE)
    y_max <- max(c(histogram$density, mixture)) * 1.12

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mar = c(3.4, 3.7, 1.2, 1))

    plot(
      histogram,
      freq = FALSE,
      col = "gray88",
      border = "white",
      ylim = c(0, y_max),
      main = "",
      xlab = "",
      ylab = "",
      axes = FALSE
    )

    y_ticks <- pretty(c(0, y_max))
    abline(h = y_ticks, col = "gray94", lty = "dotted")
    plot(
      histogram,
      freq = FALSE,
      col = "gray88",
      border = "white",
      axes = FALSE,
      add = TRUE
    )

    lines(x_grid, component_a, col = group_colors[["A"]], lwd = 2.2)
    lines(x_grid, component_b, col = group_colors[["B"]], lwd = 2.2)
    lines(x_grid, mixture, col = "gray25", lwd = 1.8, lty = 2)
    abline(v = parameters$mu, col = group_colors, lty = 3, lwd = 1)
    rug(x, col = grDevices::adjustcolor("gray20", alpha.f = 0.25))

    axis(1, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.8)
    axis(2, at = y_ticks, col = NA, col.ticks = NA, col.axis = "gray35", cex.axis = 0.8, las = 1)
    box(col = "gray85")
    mtext("Height", side = 1, line = 2.2, cex = 0.8)
    mtext("Density", side = 2, line = 2.5, cex = 0.8)

    legend(
      "topright",
      legend = c("Group A", "Group B", "Mixture"),
      col = c(group_colors, "gray25"),
      lty = c(1, 1, 2),
      lwd = c(2.2, 2.2, 1.8),
      bty = "n",
      cex = 0.75
    )
  }, res = 96)

  output$trajectory <- renderPlot({
    history <- state$history

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfrow = c(1, 3), mar = c(3.4, 3.4, 2, 0.8))

    plot_parameter_panel(
      history$iteration,
      history$mu_a,
      history$mu_b,
      "Means",
      group_colors
    )
    plot_parameter_panel(
      history$iteration,
      history$sigma_a,
      history$sigma_b,
      "Std. deviations",
      group_colors
    )
    plot_parameter_panel(
      history$iteration,
      history$pi_a,
      history$pi_b,
      "Proportions",
      group_colors
    )

    legend(
      "bottomright",
      legend = c("A", "B"),
      col = group_colors,
      lty = 1,
      pch = 16,
      bty = "n",
      cex = 0.75
    )
  }, res = 96)
}

shinyApp(ui, server)
