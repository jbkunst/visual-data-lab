# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(markdown)
library(rpart)

# data --------------------------------------------------------------------
artifact <- readRDS("data/shap-credit.rds")

predictors <- artifact$predictors
models <- artifact$models
model_labels <- vapply(models, `[[`, character(1), "label")

predict_model <- function(model, newdata) {
  prediction <- switch(
    model$type,
    logistic = stats::predict(model$fit, newdata = newdata, type = "response"),
    spline = stats::predict(model$fit, newdata = newdata, type = "response"),
    tree = stats::predict(model$fit, newdata = newdata),
    bagged = Reduce(
      `+`,
      lapply(model$fit, function(tree) stats::predict(tree, newdata = newdata))
    ) / length(model$fit)
  )

  pmin(pmax(as.numeric(prediction), 0), 1)
}

shap_one <- function(model, x, background, nsim = 24L, seed = 1L) {
  set.seed(seed)

  variables <- names(x)
  values <- setNames(numeric(length(variables)), variables)

  for (variable in variables) {
    before <- background[sample.int(nrow(background), nsim, replace = TRUE), , drop = FALSE]
    after <- before

    for (s in seq_len(nsim)) {
      permutation <- sample(variables)
      position <- match(variable, permutation)
      preceding <- if (position > 1L) permutation[seq_len(position - 1L)] else character()

      if (length(preceding)) {
        before[s, preceding] <- x[1, preceding, drop = FALSE]
        after[s, preceding] <- x[1, preceding, drop = FALSE]
      }

      after[s, variable] <- x[[variable]]
    }

    values[[variable]] <- mean(
      predict_model(model, after) - predict_model(model, before)
    )
  }

  baseline <- mean(predict_model(model, background))
  prediction <- predict_model(model, x)[[1]]
  residual <- prediction - baseline - sum(values)

  if (sum(abs(values)) > 0) {
    values <- values + residual * abs(values) / sum(abs(values))
  } else {
    values <- values + residual / length(values)
  }

  values
}

profile_labels <- c(
  seniority = "Seniority",
  time = "Loan term",
  age = "Age",
  expenses = "Expenses",
  income = "Income",
  assets = "Assets",
  debt = "Debt",
  amount = "Loan amount",
  price = "Price"
)

make_slider <- function(variable) {
  meta <- artifact$control_meta[[variable]]

  sliderInput(
    variable,
    profile_labels[[variable]],
    min = meta$min,
    max = meta$max,
    value = meta$value,
    step = meta$step,
    ticks = FALSE
  )
}

# theme -------------------------------------------------------------------
apptheme <- bs_theme()
sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(
  bslib::card,
  full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 12)
)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Client profile",
      actionButton("random_profile", "Random case", class = "btn-primary w-100"),
      tags$small("Selects one observation from the development sample."),
      lapply(predictors, make_slider),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
        )
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    div(
      class = "p-3 h-100",
      radioButtons(
        "model",
        "Model",
        choices = stats::setNames(names(models), model_labels),
        selected = "logistic",
        inline = TRUE
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("SHAP explanation for current profile"),
          uiOutput("prediction_summary"),
          plotOutput("shap_plot", height = "100%")
        ),
        div(
          class = "d-grid gap-3",
          card(
            card_header("Portfolio PD distribution"),
            plotOutput("pd_plot", height = "280px")
          ),
          card(
            card_header(uiOutput("dependence_title")),
            plotOutput("dependence_plot", height = "320px")
          )
        )
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  current_profile <- reactive({
    values <- lapply(predictors, function(variable) input[[variable]])
    names(values) <- predictors
    as.data.frame(values, check.names = FALSE)
  })

  active_variable <- reactiveVal("income")
  previous_profile <- reactiveVal(NULL)
  updating_profile <- reactiveVal(FALSE)

  observeEvent(current_profile(), {
    current <- unlist(current_profile()[1, predictors], use.names = TRUE)
    previous <- previous_profile()

    if (!is.null(previous) && !updating_profile()) {
      changed <- predictors[current[predictors] != previous[predictors]]

      if (length(changed) == 1L) {
        active_variable(changed[[1]])
      }
    }

    previous_profile(current)
  }, ignoreInit = FALSE)

  observeEvent(input$random_profile, {
    updating_profile(TRUE)
    row <- artifact$development[
      sample.int(nrow(artifact$development), 1L),
      predictors,
      drop = FALSE
    ]

    for (variable in predictors) {
      freezeReactiveValue(input, variable)
      updateSliderInput(session, variable, value = row[[variable]])
    }

    session$onFlushed(function() updating_profile(FALSE), once = TRUE)
  })

  current_model <- reactive(models[[input$model]])

  current_pd <- reactive({
    predict_model(current_model(), current_profile())[[1]]
  })

  shap_request <- reactive({
    list(model = input$model, profile = current_profile())
  }) |>
    debounce(250)

  current_shap <- reactive({
    request <- shap_request()

    shap_one(
      models[[request$model]],
      x = request$profile,
      background = artifact$background,
      nsim = 24L,
      seed = 2026L
    )
  })

  output$prediction_summary <- renderUI({
    baseline <- artifact$baseline[[input$model]]

    div(
      class = "d-flex gap-4 align-items-end px-2 pt-2",
      div(tags$small("Predicted PD"), tags$h2(scales::percent(current_pd(), accuracy = 0.1))),
      div(tags$small("Portfolio mean"), tags$h4(scales::percent(baseline, accuracy = 0.1))),
      div(tags$small("Active variable"), tags$h5(profile_labels[[active_variable()]]))
    )
  })

  output$shap_plot <- renderPlot({
    values <- current_shap()
    ord <- order(abs(values))
    values <- values[ord]
    lim <- max(abs(values), 0.01) * 1.2

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mar = c(4, 8, 1, 1))

    bp <- barplot(
      values,
      horiz = TRUE,
      names.arg = profile_labels[names(values)],
      las = 1,
      xlim = c(-lim, lim),
      border = NA,
      col = ifelse(values >= 0, "#d95f59", "#4c91d9"),
      xlab = "Contribution to predicted PD"
    )

    abline(v = 0, col = "gray50")
    text(
      x = values,
      y = bp,
      labels = sprintf("%+.1f pp", 100 * values),
      pos = ifelse(values >= 0, 4, 2),
      cex = 0.8,
      xpd = TRUE
    )
  })

  output$pd_plot <- renderPlot({
    pd <- artifact$pd[[input$model]]

    hist(
      pd,
      breaks = 28,
      col = "gray88",
      border = "white",
      main = NULL,
      xlab = "Predicted probability of default",
      ylab = "Clients",
      xlim = c(0, max(pd, current_pd()) * 1.05)
    )

    abline(v = current_pd(), col = "#d95f59", lwd = 3, lty = 2)
  })

  output$dependence_title <- renderUI({
    paste("SHAP dependence ·", profile_labels[[active_variable()]])
  })

  output$dependence_plot <- renderPlot({
    variable <- active_variable()
    data <- artifact$shap_data
    data <- data[data$model == input$model & data$variable == variable, , drop = FALSE]

    plot(
      data$value,
      data$shap,
      pch = 16,
      cex = 0.65,
      col = grDevices::adjustcolor("#3b82c4", alpha.f = 0.45),
      xlab = profile_labels[[variable]],
      ylab = "SHAP contribution to PD"
    )

    if (nrow(data) > 5) {
      lines(stats::lowess(data$value, data$shap, f = 0.6), lwd = 2, col = "gray40")
    }

    shap <- current_shap()
    points(
      current_profile()[[variable]],
      shap[[variable]],
      pch = 21,
      cex = 1.7,
      bg = "#d95f59",
      col = "white",
      lwd = 1.5
    )
  })
}

shinyApp(ui, server)
