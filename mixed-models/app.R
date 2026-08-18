# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(lme4)
library(vdltheme)

# theme -------------------------------------------------------------------
apptheme <- theme_vdl()

thematic::thematic_shiny(font = "auto")

sidebar <- purrr::partial(bslib::sidebar, width = 320)

card <- purrr::partial(
  bslib::card,
  full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 0)
)

primary_color <- unname(bs_get_variables(apptheme, "primary"))

# helpers -----------------------------------------------------------------
group_colors <- function(groups) {
  palette <- hcl.colors(256, "viridis")
  index <- round(seq(1 + 0.12 * 255, 1 + 0.88 * 255, length.out = length(groups)))
  setNames(palette[index], groups)
}

simulate_grouped_data <- function(structure, n_groups, n_per_group, seed) {
  set.seed(seed)

  beta0 <- 2
  beta1 <- 1
  sigma_b0 <- if (structure %in% c("intercept", "both")) 1.5 else 0
  sigma_b1 <- if (structure %in% c("slope", "both")) 0.65 else 0
  sigma_e <- 0.8
  rho <- if (structure == "both") 0.35 else 0

  groups <- paste0("G", seq_len(n_groups))
  z0 <- rnorm(n_groups)
  z1 <- rnorm(n_groups)
  b0 <- sigma_b0 * z0
  b1 <- sigma_b1 * (rho * z0 + sqrt(1 - rho^2) * z1)

  x_centers <- if (structure %in% c("intercept", "both")) {
    seq(-1.4, 1.4, length.out = n_groups)
  } else {
    rep(0, n_groups)
  }

  effects <- data.frame(
    group = factor(groups, levels = groups),
    b0 = b0,
    b1 = b1
  )

  dat <- do.call(
    rbind,
    lapply(seq_len(n_groups), function(i) {
      x <- if (structure %in% c("intercept", "both")) {
        sort(x_centers[i] + runif(n_per_group, -0.85, 0.85))
      } else {
        sort(runif(n_per_group, -2, 2))
      }

      epsilon <- rnorm(n_per_group, 0, sigma_e)
      y <- beta0 + b0[i] + (beta1 + b1[i]) * x + epsilon

      data.frame(
        group = factor(groups[i], levels = groups),
        x = x,
        y = y
      )
    })
  )

  list(
    data = dat,
    effects = effects,
    truth = list(
      beta0 = beta0,
      beta1 = beta1,
      sigma_b0 = sigma_b0,
      sigma_b1 = sigma_b1,
      sigma_e = sigma_e,
      rho = rho
    )
  )
}

fit_selected_model <- function(dat, model) {
  switch(
    model,
    pooled = lm(y ~ x, data = dat),
    separate = lm(y ~ x * group, data = dat),
    ri = lmer(y ~ x + (1 | group), data = dat, REML = TRUE),
    rs = lmer(y ~ x + (0 + x | group), data = dat, REML = TRUE),
    ris = lmer(y ~ x + (1 + x | group), data = dat, REML = TRUE)
  )
}

truth_lines <- function(sim) {
  groups <- levels(sim$data$group)

  do.call(
    rbind,
    lapply(groups, function(g) {
      effect <- sim$effects[sim$effects$group == g, ]
      observed <- sim$data[sim$data$group == g, ]
      x_grid <- seq(min(observed$x), max(observed$x), length.out = 100)

      data.frame(
        group = factor(g, levels = groups),
        x = x_grid,
        y = sim$truth$beta0 + effect$b0 +
          (sim$truth$beta1 + effect$b1) * x_grid
      )
    })
  )
}

fitted_lines <- function(dat, fit) {
  groups <- levels(dat$group)

  do.call(
    rbind,
    lapply(groups, function(g) {
      observed <- dat[dat$group == g, ]
      grid <- data.frame(
        x = seq(min(observed$x), max(observed$x), length.out = 100),
        group = factor(g, levels = groups)
      )
      grid$y <- predict(fit, newdata = grid)
      grid
    })
  )
}

no_pool_coefficients <- function(dat) {
  fits <- lapply(split(dat, dat$group), function(d) coef(lm(y ~ x, data = d)))
  out <- do.call(rbind, fits)
  colnames(out) <- c("intercept", "slope")
  out
}

truth_coefficients <- function(sim) {
  out <- cbind(
    intercept = sim$truth$beta0 + sim$effects$b0,
    slope = sim$truth$beta1 + sim$effects$b1
  )
  rownames(out) <- as.character(sim$effects$group)
  out
}

current_group_coefficients <- function(dat, fit, model) {
  groups <- levels(dat$group)

  if (model == "pooled") {
    coefs <- coef(fit)[c("(Intercept)", "x")]
    out <- matrix(rep(coefs, each = length(groups)), ncol = 2)
    rownames(out) <- groups
    colnames(out) <- c("intercept", "slope")
    return(out)
  }

  if (model == "separate") {
    return(no_pool_coefficients(dat))
  }

  out <- as.matrix(coef(fit)$group[, c("(Intercept)", "x"), drop = FALSE])
  colnames(out) <- c("intercept", "slope")
  out
}

population_coefficients <- function(dat, fit, model) {
  if (inherits(fit, "merMod")) {
    out <- fixef(fit)[c("(Intercept)", "x")]
  } else if (model == "pooled") {
    out <- coef(fit)[c("(Intercept)", "x")]
  } else {
    out <- coef(lm(y ~ x, data = dat))[c("(Intercept)", "x")]
  }

  unname(out)
}

estimated_sds <- function(fit) {
  out <- c(intercept = NA_real_, slope = NA_real_, residual = sigma(fit))

  if (!inherits(fit, "merMod")) {
    return(out)
  }

  vc <- VarCorr(fit)$group
  sd_re <- attr(vc, "stddev")

  if ("(Intercept)" %in% names(sd_re)) {
    out["intercept"] <- unname(sd_re["(Intercept)"])
  }

  if ("x" %in% names(sd_re)) {
    out["slope"] <- unname(sd_re["x"])
  }

  out
}

coefficient_recovery <- function(sim, fit, model) {
  truth <- truth_coefficients(sim)
  no_pool <- no_pool_coefficients(sim$data)
  selected <- current_group_coefficients(sim$data, fit, model)
  groups <- rownames(truth)

  list(
    table = data.frame(
      Group = groups,
      `True int.` = truth[groups, "intercept"],
      `No pool int.` = no_pool[groups, "intercept"],
      `Model int.` = selected[groups, "intercept"],
      `True slope` = truth[groups, "slope"],
      `No pool slope` = no_pool[groups, "slope"],
      `Model slope` = selected[groups, "slope"],
      check.names = FALSE
    ),
    no_pool_rmse = c(
      intercept = sqrt(mean((no_pool[, "intercept"] - truth[, "intercept"])^2)),
      slope = sqrt(mean((no_pool[, "slope"] - truth[, "slope"])^2))
    ),
    selected_rmse = c(
      intercept = sqrt(mean((selected[, "intercept"] - truth[, "intercept"])^2)),
      slope = sqrt(mean((selected[, "slope"] - truth[, "slope"])^2))
    )
  )
}

model_labels <- c(
  pooled = "Complete pooling",
  separate = "No pooling",
  ri = "Random intercept",
  rs = "Random slope",
  ris = "Random intercept + slope"
)

structure_labels <- c(
  none = "No group differences",
  intercept = "Different intercepts",
  slope = "Different slopes",
  both = "Different intercepts + slopes"
)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  tags$head(
    tags$style(HTML("
      .formula-block {
        margin: -0.15rem 0 0.75rem 0;
        padding: 0.55rem 0.65rem;
        border-left: 3px solid var(--bs-primary);
        background: color-mix(in srgb, var(--bs-primary) 5%, transparent);
        font-size: 0.82rem;
      }

      .formula-block code {
        display: block;
        margin-top: 0.25rem;
        white-space: normal;
      }

      .mixed-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        grid-template-rows: repeat(3, minmax(0, 1fr));
        gap: 0.75rem;
        width: 100%;
        height: calc(100vh - 1.5rem);
        min-height: 720px;
      }

      .mixed-main { grid-column: 1 / span 2; grid-row: 1 / span 2; }
      .mixed-residuals { grid-column: 3; grid-row: 1; }
      .mixed-qq { grid-column: 3; grid-row: 2; }
      .mixed-random { grid-column: 1; grid-row: 3; }
      .mixed-shrinkage { grid-column: 2; grid-row: 3; }
      .mixed-recovery { grid-column: 3; grid-row: 3; }

      .mixed-grid .card { min-width: 0; min-height: 0; }
      .mixed-grid .shiny-plot-output { height: 100% !important; min-height: 0; }
      .recovery-wrap { overflow: auto; padding: 0.25rem 0.4rem; font-size: 0.72rem; }
      .recovery-wrap table { white-space: nowrap; margin-bottom: 0; }
      .recovery-summary { padding: 0.25rem 0.4rem 0; font-size: 0.72rem; }

      @media (max-width: 1100px) {
        .mixed-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          grid-template-rows: auto;
          width: 100%;
          height: auto;
          min-height: 0;
        }

        .mixed-main { grid-column: 1 / -1; grid-row: auto; min-height: 65vh; }
        .mixed-residuals, .mixed-qq, .mixed-random, .mixed-shrinkage, .mixed-recovery {
          grid-column: auto;
          grid-row: auto;
          min-height: 260px;
        }
      }
    "))
  ),
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Mixed Models Explorer",
      radioButtons(
        "data_structure",
        input_label_vdl(
          "1. Data structure",
          "Changes the process that generates the points."
        ),
        choices = setNames(names(structure_labels), structure_labels),
        selected = "both"
      ),
      tags$div(
        class = "formula-block",
        tags$strong("Underlying model"),
        uiOutput("truth_formula")
      ),
      radioButtons(
        "model",
        input_label_vdl(
          "2. Fitted model",
          "Keeps the data fixed and changes how group structure is modeled."
        ),
        choices = setNames(names(model_labels), model_labels),
        selected = "ri"
      ),
      tags$div(
        class = "formula-block",
        tags$strong("Fitted model"),
        uiOutput("fit_formula"),
        uiOutput("pooling_note")
      ),
      sliderInput(
        "n_groups",
        tags$small("Groups"),
        min = 4,
        max = 10,
        value = 6,
        step = 1
      ),
      sliderInput(
        "n_per_group",
        tags$small("Observations per group"),
        min = 5,
        max = 40,
        value = 18,
        step = 1
      ),
      actionButton("resimulate", "Resimulate data", width = "100%"),
      uiOutput("model_check"),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
        )
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    tags$div(
      class = "mixed-grid",
      card(
        class = "mixed-main",
        card_header(uiOutput("main_title")),
        plotOutput("main_plot", width = "100%", height = "100%")
      ),
      card(
        class = "mixed-residuals",
        card_header("Residuals vs fitted"),
        plotOutput("residual_plot", width = "100%", height = "100%")
      ),
      card(
        class = "mixed-qq",
        card_header("Normal Q-Q"),
        plotOutput("qq_plot", width = "100%", height = "100%")
      ),
      card(
        class = "mixed-random",
        card_header("Random effects"),
        plotOutput("random_effects_plot", width = "100%", height = "100%")
      ),
      card(
        class = "mixed-shrinkage",
        card_header("Pooling / shrinkage"),
        plotOutput("shrinkage_plot", width = "100%", height = "100%")
      ),
      card(
        class = "mixed-recovery",
        card_header("Coefficient recovery"),
        tags$div(class = "recovery-summary", uiOutput("recovery_summary")),
        tags$div(class = "recovery-wrap", tableOutput("recovery_table"))
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  seed <- reactiveVal(100L)

  observeEvent(input$resimulate, {
    seed(seed() + 1L)
  })

  sim <- reactive({
    simulate_grouped_data(
      structure = input$data_structure,
      n_groups = input$n_groups,
      n_per_group = input$n_per_group,
      seed = seed()
    )
  })

  fit <- reactive({
    fit_selected_model(sim()$data, input$model)
  })

  recovery <- reactive({
    coefficient_recovery(sim(), fit(), input$model)
  })

  output$truth_formula <- renderUI({
    truth <- sim()$truth

    formula <- switch(
      input$data_structure,
      none = "y_{ij} = \\beta_0 + \\beta_1 x_{ij} + \\varepsilon_{ij}",
      intercept = "y_{ij} = \\beta_0 + b_{0j} + \\beta_1 x_{ij} + \\varepsilon_{ij}",
      slope = "y_{ij} = \\beta_0 + (\\beta_1 + b_{1j})x_{ij} + \\varepsilon_{ij}",
      both = "y_{ij} = \\beta_0 + b_{0j} + (\\beta_1 + b_{1j})x_{ij} + \\varepsilon_{ij}"
    )

    random_assumption <- switch(
      input$data_structure,
      none = "",
      intercept = "<div>\\(b_{0j} \\sim N(0, \\sigma^2_{b0})\\)</div>",
      slope = "<div>\\(b_{1j} \\sim N(0, \\sigma^2_{b1})\\)</div>",
      both = "<div>\\(\\mathbf b_j \\sim N(\\mathbf 0, \\Sigma)\\)</div>"
    )

    withMathJax(
      HTML(
        paste0(
          "<div>\\(", formula, "\\)</div>",
          random_assumption,
          "<div>\\(\\varepsilon_{ij} \\sim N(0, \\sigma^2)\\)</div>",
          sprintf(
            "<div><small>True SDs: intercept %.2f · slope %.2f · residual %.2f</small></div>",
            truth$sigma_b0,
            truth$sigma_b1,
            truth$sigma_e
          )
        )
      )
    )
  })

  output$fit_formula <- renderUI({
    math <- switch(
      input$model,
      pooled = "y_{ij} = \\beta_0 + \\beta_1x_{ij} + \\varepsilon_{ij}",
      separate = "y_{ij} = \\alpha_{0j} + \\alpha_{1j}x_{ij} + \\varepsilon_{ij}",
      ri = "y_{ij} = \\beta_0 + b_{0j} + \\beta_1x_{ij} + \\varepsilon_{ij}",
      rs = "y_{ij} = \\beta_0 + (\\beta_1 + b_{1j})x_{ij} + \\varepsilon_{ij}",
      ris = "y_{ij} = \\beta_0 + b_{0j} + (\\beta_1 + b_{1j})x_{ij} + \\varepsilon_{ij}"
    )

    code <- switch(
      input$model,
      pooled = "lm(y ~ x, data = dat)",
      separate = "lm(y ~ x * group, data = dat)",
      ri = "lmer(y ~ x + (1 | group), data = dat)",
      rs = "lmer(y ~ x + (0 + x | group), data = dat)",
      ris = "lmer(y ~ x + (1 + x | group), data = dat)"
    )

    tagList(
      withMathJax(HTML(paste0("<div>\\(", math, "\\)</div>"))),
      tags$code(code)
    )
  })

  output$pooling_note <- renderUI({
    text <- switch(
      input$model,
      pooled = "Complete pooling · one relationship is shared by every group.",
      separate = "No pooling · each group gets its own OLS relationship.",
      "Partial pooling · group deviations are estimated jointly and shrink toward zero."
    )

    tags$div(class = "small text-muted mt-1", text)
  })

  output$model_check <- renderUI({
    mod <- fit()
    residual_mean <- mean(residuals(mod))

    if (!inherits(mod, "merMod")) {
      return(
        tags$div(
          class = "small mt-2 mb-2",
          tags$strong("Model check"),
          tags$div("Random effects: not modeled"),
          tags$div(sprintf("Mean residual: %.3f", residual_mean))
        )
      )
    }

    convergence_messages <- mod@optinfo$conv$lme4$messages
    converged <- is.null(convergence_messages)
    singular <- isSingular(mod, tol = 1e-4)
    sds <- estimated_sds(mod)

    tags$div(
      class = "small mt-2 mb-2",
      tags$strong("Model check"),
      tags$div(if (converged) "✓ optimizer converged" else "⚠ convergence warning"),
      tags$div(
        if (singular) {
          "⚠ singular fit: random-effect covariance is on a boundary"
        } else {
          "✓ random-effect covariance is full rank"
        }
      ),
      tags$div(
        sprintf(
          "Estimated SDs: intercept %s · slope %s",
          ifelse(is.na(sds["intercept"]), "—", sprintf("%.3f", sds["intercept"])),
          ifelse(is.na(sds["slope"]), "—", sprintf("%.3f", sds["slope"]))
        )
      ),
      tags$div(sprintf("Mean residual: %.3f", residual_mean))
    )
  })

  output$main_title <- renderUI({
    tags$span(
      structure_labels[[input$data_structure]],
      tags$span(class = "text-muted", "  →  "),
      model_labels[[input$model]]
    )
  })

  output$main_plot <- renderPlot({
    simulation <- sim()
    dat <- simulation$data
    mod <- fit()
    truth <- truth_lines(simulation)
    fitted <- fitted_lines(dat, mod)
    groups <- levels(dat$group)
    cols <- group_colors(groups)

    plot(
      dat$x,
      dat$y,
      col = adjustcolor(cols[as.character(dat$group)], alpha.f = 0.7),
      pch = 16,
      xlab = "x",
      ylab = "y",
      main = ""
    )

    for (g in groups) {
      dtruth <- truth[truth$group == g, ]
      lines(dtruth$x, dtruth$y, col = adjustcolor(cols[g], alpha.f = 0.5), lty = 2, lwd = 2)
    }

    if (input$model == "pooled") {
      abline(mod, col = primary_color, lwd = 3)
    } else {
      for (g in groups) {
        dfit <- fitted[fitted$group == g, ]
        lines(dfit$x, dfit$y, col = cols[g], lwd = 2.5)
      }
    }

    legend(
      "topleft",
      legend = c("Simulated truth", "Fitted model"),
      lty = c(2, 1),
      lwd = c(2, 2.5),
      bty = "n",
      cex = 0.85
    )
  })

  output$residual_plot <- renderPlot({
    dat <- sim()$data
    mod <- fit()
    groups <- levels(dat$group)
    cols <- group_colors(groups)
    x <- fitted(mod)
    y <- residuals(mod)

    plot(
      x,
      y,
      pch = 16,
      col = adjustcolor(cols[as.character(dat$group)], alpha.f = 0.72),
      xlab = "Fitted",
      ylab = "Residual",
      main = ""
    )
    abline(h = 0, lty = 2)
    lines(lowess(x, y), lwd = 2)
  })

  output$qq_plot <- renderPlot({
    dat <- sim()$data
    r <- residuals(fit())
    groups <- levels(dat$group)
    cols <- group_colors(groups)
    order_r <- order(r)
    qq <- qqnorm(r, plot.it = FALSE)

    plot(
      qq$x,
      qq$y,
      pch = 16,
      col = adjustcolor(cols[as.character(dat$group)[order_r]], alpha.f = 0.72),
      xlab = "Theoretical Quantiles",
      ylab = "Sample Quantiles",
      main = ""
    )
    qqline(r, lwd = 2)
  })

  output$random_effects_plot <- renderPlot({
    mod <- fit()

    if (!inherits(mod, "merMod")) {
      plot.new()
      text(0.5, 0.55, "No random effects in this model", cex = 1.05)
      text(0.5, 0.43, "Complete/no pooling do not estimate b_j", cex = 0.85)
      return(invisible())
    }

    re <- ranef(mod)$group
    groups <- rownames(re)
    cols <- group_colors(groups)
    values <- as.matrix(re)
    xr <- range(c(0, values))
    pad <- max(diff(xr) * 0.08, 0.1)

    plot(
      c(xr[1] - pad, xr[2] + pad),
      c(0.5, length(groups) + 0.5),
      type = "n",
      yaxt = "n",
      xlab = "Deviation from population effect",
      ylab = "",
      main = ""
    )
    axis(2, at = seq_along(groups), labels = groups, las = 1, cex.axis = 0.8)
    abline(v = 0, lty = 2)

    pchs <- c(16, 1)
    for (j in seq_len(ncol(values))) {
      points(
        values[, j],
        seq_along(groups),
        pch = pchs[j],
        col = cols[groups],
        cex = 1.15,
        lwd = 1.5
      )
    }

    legend(
      "topright",
      legend = colnames(values),
      pch = pchs[seq_len(ncol(values))],
      bty = "n",
      cex = 0.8
    )
    mtext(expression(b[j] %~% N(0, Sigma)), side = 3, line = -1.2, adj = 0, cex = 0.8)
  })

  output$shrinkage_plot <- renderPlot({
    dat <- sim()$data
    mod <- fit()
    groups <- levels(dat$group)
    cols <- group_colors(groups)
    no_pool <- no_pool_coefficients(dat)
    current <- current_group_coefficients(dat, mod, input$model)
    population <- population_coefficients(dat, mod, input$model)

    xr <- range(c(no_pool[, "intercept"], current[, "intercept"], population[1]))
    yr <- range(c(no_pool[, "slope"], current[, "slope"], population[2]))
    xpad <- max(diff(xr) * 0.12, 0.2)
    ypad <- max(diff(yr) * 0.12, 0.15)

    plot(
      c(xr[1] - xpad, xr[2] + xpad),
      c(yr[1] - ypad, yr[2] + ypad),
      type = "n",
      xlab = "Group intercept",
      ylab = "Group slope",
      main = ""
    )

    segments(
      no_pool[, "intercept"],
      no_pool[, "slope"],
      current[, "intercept"],
      current[, "slope"],
      col = adjustcolor(cols[groups], alpha.f = 0.55),
      lwd = 1.5
    )
    points(
      no_pool[, "intercept"],
      no_pool[, "slope"],
      pch = 1,
      col = cols[groups],
      cex = 1.15,
      lwd = 1.5
    )
    points(
      current[, "intercept"],
      current[, "slope"],
      pch = 16,
      col = cols[groups],
      cex = 1.15
    )
    points(population[1], population[2], pch = 8, cex = 1.5, lwd = 2)

    text(
      current[, "intercept"],
      current[, "slope"],
      labels = groups,
      col = cols[groups],
      pos = 3,
      cex = 0.7
    )

    legend(
      "topright",
      legend = c("No-pooling estimate", "Selected model", "Population effect"),
      pch = c(1, 16, 8),
      bty = "n",
      cex = 0.78
    )
  })

  output$recovery_summary <- renderUI({
    x <- recovery()

    tags$div(
      tags$div(
        tags$strong("No pooling RMSE: "),
        sprintf("intercept %.2f · slope %.2f", x$no_pool_rmse["intercept"], x$no_pool_rmse["slope"])
      ),
      tags$div(
        tags$strong(paste0(model_labels[[input$model]], " RMSE: ")),
        sprintf("intercept %.2f · slope %.2f", x$selected_rmse["intercept"], x$selected_rmse["slope"])
      )
    )
  })

  output$recovery_table <- renderTable(
    {
      recovery()$table
    },
    digits = 2,
    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "xs",
    rownames = FALSE
  )
}

shinyApp(ui, server)
