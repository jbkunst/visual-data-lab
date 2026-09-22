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

empty_plot <- function(text) {
  plot.new()
  text(0.5, 0.5, text, cex = 0.95, col = "grey40")
}

make_group_sizes <- function(pattern, n_groups, n_per_group) {
  if (pattern == "balanced") {
    return(rep(as.integer(n_per_group), n_groups))
  }

  as.integer(round(exp(seq(log(4), log(60), length.out = n_groups))))
}

simulate_grouped_data <- function(
  structure,
  n_groups,
  n_per_group,
  group_size_pattern,
  seed
) {
  set.seed(seed)

  beta0 <- 2
  beta1 <- 1
  sigma_b0 <- if (structure %in% c("intercept", "both")) 1.5 else 0
  sigma_b1 <- if (structure %in% c("slope", "both")) 0.65 else 0
  sigma_e <- 0.8
  rho <- if (structure == "both") 0.35 else 0

  groups <- paste0("G", seq_len(n_groups))
  sizes <- make_group_sizes(group_size_pattern, n_groups, n_per_group)
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
      n_i <- sizes[i]

      x <- if (structure %in% c("intercept", "both")) {
        sort(x_centers[i] + runif(n_i, -0.85, 0.85))
      } else {
        sort(runif(n_i, -2, 2))
      }

      epsilon <- rnorm(n_i, 0, sigma_e)
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
    group_sizes = setNames(sizes, groups),
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
    none = NULL,
    pooled = lm(y ~ x, data = dat),
    separate = lm(y ~ x * group, data = dat),
    ri = lmer(y ~ x + (1 | group), data = dat, REML = TRUE),
    rs = lmer(y ~ x + (0 + x | group), data = dat, REML = TRUE),
    ris = lmer(y ~ x + (1 + x | group), data = dat, REML = TRUE)
  )
}

predict_selected_model <- function(fit, newdata) {
  if (inherits(fit, "merMod")) {
    predict(fit, newdata = newdata, allow.new.levels = TRUE)
  } else {
    predict(fit, newdata = newdata)
  }
}

split_grouped_data <- function(dat, seed, train_share = 0.7) {
  set.seed(seed + 10000L)

  train_rows <- unlist(
    lapply(split(seq_len(nrow(dat)), dat$group), function(index) {
      n_train <- floor(length(index) * train_share)
      n_train <- max(2L, min(length(index) - 1L, n_train))
      sample(index, n_train)
    }),
    use.names = FALSE
  )

  list(
    train = dat[train_rows, , drop = FALSE],
    test = dat[-train_rows, , drop = FALSE]
  )
}

compare_models <- function(dat, seed) {
  split <- split_grouped_data(dat, seed)
  models <- c("pooled", "separate", "ri", "rs", "ris")

  rows <- lapply(models, function(model) {
    mod <- tryCatch(
      suppressWarnings(fit_selected_model(split$train, model)),
      error = function(e) NULL
    )

    if (is.null(mod)) {
      return(
        data.frame(
          model = model,
          train_rmse = NA_real_,
          test_rmse = NA_real_,
          equal_group_test_rmse = NA_real_
        )
      )
    }

    pred_train <- tryCatch(
      predict_selected_model(mod, split$train),
      error = function(e) rep(NA_real_, nrow(split$train))
    )
    pred_test <- tryCatch(
      predict_selected_model(mod, split$test),
      error = function(e) rep(NA_real_, nrow(split$test))
    )

    train_error2 <- (split$train$y - pred_train)^2
    test_error2 <- (split$test$y - pred_test)^2
    group_mse <- tapply(test_error2, split$test$group, mean, na.rm = TRUE)

    data.frame(
      model = model,
      train_rmse = sqrt(mean(train_error2, na.rm = TRUE)),
      test_rmse = sqrt(mean(test_error2, na.rm = TRUE)),
      equal_group_test_rmse = sqrt(mean(group_mse, na.rm = TRUE))
    )
  })

  do.call(rbind, rows)
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
      grid$y <- predict_selected_model(fit, grid)
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
  none = "No model",
  pooled = "Global model",
  separate = "Group-specific models",
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
      .mixed-comparison { grid-column: 3; grid-row: 3; }

      .mixed-grid .card { min-width: 0; min-height: 0; }
      .mixed-grid .shiny-plot-output { height: 100% !important; min-height: 0; }
      .comparison-wrap { overflow: auto; padding: 0.25rem 0.4rem; font-size: 0.72rem; }
      .comparison-wrap table { white-space: nowrap; margin-bottom: 0; }
      .comparison-summary { padding: 0.3rem 0.5rem 0; font-size: 0.72rem; }

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
        .mixed-residuals, .mixed-qq, .mixed-random, .mixed-shrinkage, .mixed-comparison {
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
        selected = "none"
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
      radioButtons(
        "group_size_pattern",
        input_label_vdl(
          "Group sizes",
          "Balanced gives every group the same amount of data. Unbalanced mixes small and large groups."
        ),
        choices = c("Balanced" = "balanced", "Unbalanced" = "unbalanced"),
        selected = "unbalanced"
      ),
      conditionalPanel(
        condition = "input.group_size_pattern == 'balanced'",
        sliderInput(
          "n_per_group",
          tags$small("Observations per group"),
          min = 5,
          max = 40,
          value = 18,
          step = 1
        )
      ),
      uiOutput("group_size_note"),
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
        class = "mixed-comparison",
        card_header("Train / test RMSE"),
        tags$div(class = "comparison-summary", uiOutput("comparison_summary")),
        tags$div(class = "comparison-wrap", tableOutput("comparison_table"))
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
      group_size_pattern = input$group_size_pattern,
      seed = seed()
    )
  })

  fit <- reactive({
    fit_selected_model(sim()$data, input$model)
  })

  comparison <- reactive({
    req(input$model != "none")
    compare_models(sim()$data, seed())
  })

  output$group_size_note <- renderUI({
    sizes <- make_group_sizes(
      input$group_size_pattern,
      input$n_groups,
      input$n_per_group
    )
    groups <- paste0("G", seq_len(input$n_groups))

    tags$div(
      class = "small text-muted mb-2",
      paste0(
        "n by group: ",
        paste(paste0(groups, "=", sizes), collapse = " · ")
      )
    )
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
    if (input$model == "none") {
      return(tags$div(class = "text-muted", "No fitted model"))
    }

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
      none = "Explore the grouped data before fitting a model.",
      pooled = "Complete pooling · one global relationship is shared by every group.",
      separate = "No pooling · each group gets an independent OLS relationship.",
      "Partial pooling · group deviations are estimated jointly and shrink toward zero."
    )

    tags$div(class = "small text-muted mt-1", text)
  })

  output$model_check <- renderUI({
    if (input$model == "none") {
      return(
        tags$div(
          class = "small mt-2 mb-2 text-muted",
          "No model selected"
        )
      )
    }

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

    if (input$model == "none") {
      return(invisible())
    }

    mod <- fit()
    truth <- truth_lines(simulation)
    fitted <- fitted_lines(dat, mod)

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
    if (input$model == "none") {
      empty_plot("Select a fitted model")
      return(invisible())
    }

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
    if (input$model == "none") {
      empty_plot("Select a fitted model")
      return(invisible())
    }

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
    if (input$model == "none") {
      empty_plot("Select a fitted model")
      return(invisible())
    }

    mod <- fit()

    if (!inherits(mod, "merMod")) {
      empty_plot("This model has no random effects")
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
    if (input$model == "none") {
      empty_plot("Select a fitted model")
      return(invisible())
    }

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

  output$comparison_summary <- renderUI({
    if (input$model == "none") {
      return(tags$span(class = "text-muted", "Select a fitted model to compare generalization."))
    }

    tags$span(
      "70% train / 30% test within each group. Test weights rows; Equal-group gives every group the same weight."
    )
  })

  output$comparison_table <- renderTable(
    {
      if (input$model == "none") {
        return(NULL)
      }

      x <- comparison()
      score <- replace(x$equal_group_test_rmse, is.na(x$equal_group_test_rmse), Inf)
      best <- which.min(score)

      data.frame(
        Model = paste0(
          unname(model_labels[x$model]),
          ifelse(x$model == input$model, "  ← selected", "")
        ),
        Train = x$train_rmse,
        Test = x$test_rmse,
        `Equal-group test` = x$equal_group_test_rmse,
        Best = ifelse(seq_len(nrow(x)) == best, "✓", ""),
        check.names = FALSE
      )
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
