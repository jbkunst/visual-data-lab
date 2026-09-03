# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(reactable)
library(vdltheme)

# theme -------------------------------------------------------------------
thematic::thematic_shiny(font = "auto")

app_theme <- theme_vdl()

sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(
  bslib::card,
  full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 0)
)

group_colors <- setNames(
  unname(bs_get_variables(app_theme, c("primary", "danger"))),
  c("A", "B")
)

# helpers -----------------------------------------------------------------
em_e <- function(x, parameters) {
  weighted_density <- cbind(
    A = parameters$pi[["A"]] * dnorm(x, parameters$mu[["A"]], parameters$sigma[["A"]]),
    B = parameters$pi[["B"]] * dnorm(x, parameters$mu[["B"]], parameters$sigma[["B"]])
  )

  weighted_density / pmax(rowSums(weighted_density), .Machine$double.xmin)
}

em_m <- function(x, responsibility) {
  effective_n <- colSums(responsibility)
  mu <- colSums(responsibility * x) / effective_n

  list(
    pi = effective_n / length(x),
    mu = mu,
    sigma = sqrt(pmax(
      colSums(responsibility * sweep(matrix(x, nrow = length(x), ncol = 2), 2, mu)^2) / effective_n,
      0.25^2
    ))
  )
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = app_theme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Expectation-Maximization",
      withMathJax(),
      selectInput(
        "n",
        input_label_vdl(
          "Number of observations",
          "Changes the size of the simulated sample used by the algorithm."
        ),
        choices = c(20, 100, 500),
        selected = 20
      ),
      uiOutput("step_button"),
      actionButton("converge", "Run to convergence", class = "btn-outline-secondary w-100"),
      layout_columns(
        actionButton("reset", "Reset", class = "btn-light w-100"),
        actionButton("simulate", "New data", class = "btn-light w-100")
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
      col_widths = c(6, 6, 6, 6, 12),
      row_heights = c(1.5, 5, 3.5),
      gap = "0.75rem",
      uiOutput("parameters_a", fill = TRUE),
      uiOutput("parameters_b", fill = TRUE),
      card(
        card_header(
          withMathJax(
            tagList(
              "Gaussian mixture model",
              tags$small(
                class = "text-muted ms-2",
                "\\(p(x) = \\pi_A \\mathcal{N}_A(x) + \\pi_B \\mathcal{N}_B(x)\\)"
              )
            )
          )
        ),
        plotOutput("distribution", width = "100%", height = "100%")
      ),
      card(
        card_header("Parameter trajectories"),
        plotOutput("trajectory", width = "100%", height = "100%")
      ),
      card(
        card_header("Observations and memberships"),
        reactableOutput("observations", height = "100%")
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  state <- reactiveValues(seed = 0L)

  reset <- function(new_data = FALSE) {
    if (new_data || is.null(state$x)) {
      n <- if (is.null(input$n)) 20L else as.integer(input$n)
      set.seed(20260825L + n + state$seed)
      n_a <- floor(n / 2)
      state$x <- sort(round(c(
        rnorm(n_a, 159, 5.5),
        rnorm(n - n_a, 172, 6)
      ), 1))
    }

    initial_sigma <- max(sd(state$x) * 0.8, 3)
    state$parameters <- list(
      pi = c(A = 0.5, B = 0.5),
      mu = c(A = unname(quantile(state$x, 0.25)), B = unname(quantile(state$x, 0.75))),
      sigma = c(A = initial_sigma, B = initial_sigma)
    )
    state$responsibility <- matrix(
      NA_real_,
      nrow = length(state$x),
      ncol = 2,
      dimnames = list(NULL, c("A", "B"))
    )
    state$iteration <- 0L
    state$phase <- "E"
    state$history <- data.frame(
      iteration = 0L,
      pi_a = 0.5,
      pi_b = 0.5,
      mu_a = state$parameters$mu[["A"]],
      mu_b = state$parameters$mu[["B"]],
      sigma_a = initial_sigma,
      sigma_b = initial_sigma
    )
  }

  complete_iteration <- function() {
    old <- state$parameters
    state$responsibility <- em_e(state$x, old)
    state$parameters <- em_m(state$x, state$responsibility)
    state$iteration <- state$iteration + 1L
    change <- max(abs(c(
      old$pi - state$parameters$pi,
      old$mu - state$parameters$mu,
      old$sigma - state$parameters$sigma
    )))
    state$history <- rbind(state$history, data.frame(
      iteration = state$iteration,
      pi_a = state$parameters$pi[["A"]],
      pi_b = state$parameters$pi[["B"]],
      mu_a = state$parameters$mu[["A"]],
      mu_b = state$parameters$mu[["B"]],
      sigma_a = state$parameters$sigma[["A"]],
      sigma_b = state$parameters$sigma[["B"]]
    ))
    change
  }

  isolate(reset(new_data = TRUE))

  observeEvent(input$n, {
    state$seed <- 0L
    reset(new_data = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$simulate, {
    state$seed <- state$seed + 1L
    reset(new_data = TRUE)
  })

  observeEvent(input$reset, reset())

  observeEvent(input$step, {
    if (state$phase != "M") {
      state$responsibility <- em_e(state$x, state$parameters)
      state$phase <- "M"
    } else {
      complete_iteration()
      state$phase <- "E"
    }
  })

  observeEvent(input$converge, {
    for (i in seq_len(100)) {
      change <- complete_iteration()
      if (change < 0.005) break
    }

    state$responsibility <- em_e(state$x, state$parameters)
    state$phase <- "Done"
  })

  output$step_button <- renderUI({
    actionButton(
      "step",
      if (state$phase == "M") {
        "M step (values → pars)"
      } else {
        "E step (pars → values)"
      },
      class = "btn-primary w-100"
    )
  })

  output$parameters_a <- renderUI({
    effective_n <- colSums(state$responsibility)

    withMathJax(
      value_box(
        title = paste("Component A · iteration", state$iteration),
        value = paste0(
          "\\(f_A(x) = \\mathcal{N}(x \\mid ",
          round(state$parameters$mu[["A"]], 2),
          ", ",
          round(state$parameters$sigma[["A"]], 2),
          "^2)\\)"
        ),
        showcase = paste0("\\(\\pi_A = ", round(state$parameters$pi[["A"]], 3), "\\)"),
        tags$small(paste("Effective cases:", ifelse(is.na(effective_n[["A"]]), "—", round(effective_n[["A"]], 2))))
      )
    )
  })

  output$parameters_b <- renderUI({
    effective_n <- colSums(state$responsibility)

    withMathJax(
      value_box(
        title = paste("Component B · iteration", state$iteration),
        value = paste0(
          "\\(f_B(x) = \\mathcal{N}(x \\mid ",
          round(state$parameters$mu[["B"]], 2),
          ", ",
          round(state$parameters$sigma[["B"]], 2),
          "^2)\\)"
        ),
        showcase = paste0("\\(\\pi_B = ", round(state$parameters$pi[["B"]], 3), "\\)"),
        tags$small(paste("Effective cases:", ifelse(is.na(effective_n[["B"]]), "—", round(effective_n[["B"]], 2))))
      )
    )
  })

  output$observations <- renderReactable({
    p <- state$parameters
    responsibility <- state$responsibility
    observations <- data.frame(
      index = seq_along(state$x),
      height = state$x,
      weighted_a = p$pi[["A"]] * dnorm(state$x, p$mu[["A"]], p$sigma[["A"]]),
      weighted_b = p$pi[["B"]] * dnorm(state$x, p$mu[["B"]], p$sigma[["B"]]),
      probability_a = responsibility[, "A"],
      probability_b = responsibility[, "B"],
      likely = ifelse(
        is.na(responsibility[, "A"]),
        "—",
        ifelse(responsibility[, "A"] >= responsibility[, "B"], "A", "B")
      )
    )

    table <- reactable(
      observations,
      height = "100%",
      pagination = FALSE,
      compact = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultColDef = colDef(sortable = FALSE, align = "center", na = "—"),
      columns = list(
        index = colDef(name = "#", width = 55),
        height = colDef(name = "Height", format = colFormat(digits = 2)),
        weighted_a = colDef(
          header = function(value) tags$span("\\(\\pi_A f_A(x)\\)"),
          format = colFormat(digits = 5)
        ),
        weighted_b = colDef(
          header = function(value) tags$span("\\(\\pi_B f_B(x)\\)"),
          format = colFormat(digits = 5)
        ),
        probability_a = colDef(
          header = function(value) tags$span(
            "\\(P(A \\mid x) = \\frac{\\pi_A f_A(x)}{\\pi_A f_A(x) + \\pi_B f_B(x)}\\)"
          ),
          format = colFormat(digits = 3),
          minWidth = 240
        ),
        probability_b = colDef(
          header = function(value) tags$span(
            "\\(P(B \\mid x) = \\frac{\\pi_B f_B(x)}{\\pi_A f_A(x) + \\pi_B f_B(x)}\\)"
          ),
          format = colFormat(digits = 3),
          minWidth = 240
        ),
        likely = colDef(name = "Likely", width = 75)
      )
    )

    htmlwidgets::onRender(
      table,
      "function(el) { if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub, el]); }"
    )
  })

  output$distribution <- renderPlot({
    p <- state$parameters
    x_grid <- seq(
      min(state$x, p$mu - 3 * p$sigma),
      max(state$x, p$mu + 3 * p$sigma),
      length.out = 400
    )
    component_a <- p$pi[["A"]] * dnorm(x_grid, p$mu[["A"]], p$sigma[["A"]])
    component_b <- p$pi[["B"]] * dnorm(x_grid, p$mu[["B"]], p$sigma[["B"]])

    hist(state$x, probability = TRUE, col = "gray90", border = "white", main = "", xlab = "Height")
    lines(x_grid, component_a, col = group_colors[["A"]], lwd = 2)
    lines(x_grid, component_b, col = group_colors[["B"]], lwd = 2)
    lines(x_grid, component_a + component_b, lwd = 2, lty = 2)
    legend(
      "topright",
      c("Group A", "Group B", "Mixture"),
      col = c(group_colors, "black"),
      lty = c(1, 1, 2),
      bty = "n"
    )
  }, res = 96)

  output$trajectory <- renderPlot({
    history <- state$history
    par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

    for (values in list(
      c("mu_a", "mu_b", "Means"),
      c("sigma_a", "sigma_b", "Std. deviations"),
      c("pi_a", "pi_b", "Proportions")
    )) {
      matplot(
        history$iteration,
        history[, values[1:2]],
        type = "o",
        pch = 16,
        lty = 1,
        col = group_colors,
        xlab = "Iteration",
        ylab = "",
        main = values[[3]]
      )
    }
  }, res = 96)
}

shinyApp(ui, server)
