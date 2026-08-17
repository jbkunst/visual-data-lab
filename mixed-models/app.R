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

  effects <- data.frame(
    group = factor(groups, levels = groups),
    b0 = b0,
    b1 = b1
  )

  dat <- do.call(
    rbind,
    lapply(seq_len(n_groups), function(i) {
      x <- sort(runif(n_per_group, -2, 2))
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
  x_grid <- seq(-2.1, 2.1, length.out = 100)

  do.call(
    rbind,
    lapply(groups, function(g) {
      effect <- sim$effects[sim$effects$group == g, ]

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
  x_grid <- seq(-2.1, 2.1, length.out = 100)
  grid <- expand.grid(x = x_grid, group = groups)
  grid$group <- factor(grid$group, levels = groups)
  grid$y <- predict(fit, newdata = grid)
  grid
}

no_pool_coefficients <- function(dat) {
  fits <- lapply(split(dat, dat$group), function(d) coef(lm(y ~ x, data = d)))
  out <- do.call(rbind, fits)
  colnames(out) <- c("intercept", "slope")
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
        choices = structure_labels,
        selected = "both"
      ),
      radioButtons(
        "model",
        input_label_vdl(
          "2. Fitted model",
          "Keeps the data fixed and changes how group structure is modeled."
        ),
        choices = model_labels,
        selected = "ri"
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
      tags$hr(),
      tags$small(tags$strong("Underlying model")),
      uiOutput("truth_formula"),
      tags$small(tags$strong("Fitted model")),
      uiOutput("fit_formula"),
      uiOutput("pooling_note"),
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
    layout_columns(
      col_widths = c(12, 6, 6, 4, 4, 4),
      gap = "0.75rem",
      card(
        card_header(uiOutput("main_title")),
        plotOutput("main_plot", width = "100%", height = "45vh")
      ),
      card(
        card_header("Residuals vs fitted"),
        plotOutput("residual_plot", width = "100%", height = "27vh")
      ),
      card(
        card_header("Normal Q-Q"),
        plotOutput("qq_plot", width = "100%", height = "27vh")
      ),
      card(
        card_header("Random effects"),
        plotOutput("random_effects_plot", width = "100%", height = "27vh")
      ),
      card(
        card_header("Pooling / shrinkage"),
        plotOutput("shrinkage_plot", width = "100%", height = "27vh")
      ),
      card(
        card_header("Variance components"),
        plotOutput("variance_plot", width = "100%", height = "27vh")
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

    tags$div(class = "small text-muted mt-2", tags$strong("Information sharing: "), text)
  })

  output$model_check <- renderUI({
    mod <- fit()
    residual_mean <- mean(residuals(mod))

    if (!inherits(mod, "merMod")) {
      return(
        tags$div(
          class = "small mt-2",
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
      class = "small mt-2",
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
    cols <- setNames(hcl.colors(length(groups), "Dark 3"), groups)

    plot(
      dat$x,
      dat$y,
      col = adjustcolor(cols[as.character(dat$group)], alpha.f = 0.65),
      pch = 16,
      xlab = "x",
      ylab = "y",
      main = ""
    )

    for (g in groups) {
      dtruth <- truth[truth$group == g, ]
      lines(dtruth$x, dtruth$y, col = adjustcolor(cols[g], alpha.f = 0.45), lty = 2, lwd = 2)
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
    mod <- fit()
    x <- fitted(mod)
    y <- residuals(mod)

    plot(
      x,
      y,
      pch = 16,
      col = adjustcolor(primary_color, alpha.f = 0.55),
      xlab = "Fitted",
      ylab = "Residual",
      main = ""
    )
    abline(h = 0, lty = 2)
    lines(lowess(x, y), lwd = 2)
  })

  output$qq_plot <- renderPlot({
    r <- residuals(fit())
    qqnorm(r, pch = 16, col = adjustcolor(primary_color, alpha.f = 0.55), main = "")
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
      points(values[, j], seq_along(groups), pch = pchs[j], cex = 1.15)
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
      col = "grey70"
    )
    points(no_pool[, "intercept"], no_pool[, "slope"], pch = 1, cex = 1.1)
    points(current[, "intercept"], current[, "slope"], pch = 16, cex = 1.1)
    points(population[1], population[2], pch = 8, cex = 1.5, lwd = 2)

    if (input$model %in% c("ri", "rs", "ris")) {
      text(
        current[, "intercept"],
        current[, "slope"],
        labels = groups,
        pos = 3,
        cex = 0.7
      )
    }

    legend(
      "topright",
      legend = c("No-pooling estimate", "Selected model", "Population effect"),
      pch = c(1, 16, 8),
      bty = "n",
      cex = 0.78
    )
  })

  output$variance_plot <- renderPlot({
    truth <- sim()$truth
    mod <- fit()

    true_sd <- c(
      intercept = truth$sigma_b0,
      slope = truth$sigma_b1,
      residual = truth$sigma_e
    )
    estimated_sd <- estimated_sds(mod)

    values <- c(true_sd, estimated_sd)
    ymax <- max(values, na.rm = TRUE) * 1.2
    if (!is.finite(ymax) || ymax == 0) ymax <- 1

    plot(
      c(0.7, 3.3),
      c(0, ymax),
      type = "n",
      xaxt = "n",
      xlab = "",
      ylab = "Standard deviation",
      main = ""
    )
    axis(1, at = 1:3, labels = c("Random intercept", "Random slope", "Residual"))

    for (i in seq_along(true_sd)) {
      if (!is.na(estimated_sd[i])) {
        segments(i, true_sd[i], i, estimated_sd[i], col = "grey70")
      }
    }

    points(1:3, true_sd, pch = 1, cex = 1.35, lwd = 2)
    keep <- !is.na(estimated_sd)
    points((1:3)[keep], estimated_sd[keep], pch = 16, cex = 1.15)

    legend(
      "topright",
      legend = c("True", "Estimated"),
      pch = c(1, 16),
      bty = "n",
      cex = 0.82
    )
  })
}

shinyApp(ui, server)
