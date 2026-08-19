# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(highcharter)
library(xgboost)
library(vdltheme)

# data --------------------------------------------------------------------
app_dir <- if (file.exists("credit-analysis.rds")) "." else "xgboost-training"
credit <- readRDS(file.path(app_dir, "credit-analysis.rds"))

train <- credit$train
validation <- credit$test
predictors <- credit$predictors

rm(credit)

dtrain <- xgb.DMatrix(
  as.matrix(train[, predictors, drop = FALSE]),
  label = train$status_bad,
  nthread = 1
)

dvalidation <- xgb.DMatrix(
  as.matrix(validation[, predictors, drop = FALSE]),
  label = validation$status_bad,
  nthread = 1
)

y_train <- train$status_bad
y_validation <- validation$status_bad

# helpers -----------------------------------------------------------------
options(highcharter.theme = highcharter_theme_vdl())

xy <- function(x, y) {
  list_parse2(data.frame(x, y))
}

step_line <- function(step) {
  list(list(
    value = step,
    color = "#adb5bd",
    width = 1,
    dashStyle = "ShortDash",
    zIndex = 4
  ))
}

chart_header_help <- function(title, help) {
  card_header(
    div(
      class = "d-flex align-items-center gap-1",
      style = "font-weight: 400;",
      span(title),
      tooltip(
        tags$span(
          "ⓘ",
          class = "text-muted",
          style = "font-weight: 400;",
          tabindex = "0"
        ),
        help,
        placement = "right",
        options = list(trigger = "hover focus click")
      )
    )
  )
}

line_chart <- function(y_title, step, series) {
  chart <- highchart() |>
    hc_chart(type = "line") |>
    hc_xAxis(
      title = list(text = "Trees"),
      min = 1,
      plotLines = step_line(step)
    ) |>
    hc_yAxis(title = list(text = y_title)) |>
    hc_tooltip(shared = TRUE, valueDecimals = 4) |>
    hc_plotOptions(series = list(
      animation = list(duration = 250),
      lineWidth = 2,
      marker = list(enabled = FALSE)
    ))

  for (series_i in series) {
    chart <- chart |>
      hc_add_series(
        id = series_i$id,
        name = series_i$name,
        data = xy(series_i$x, series_i$y)
      )
  }

  chart
}

hist_points <- function(x, bins = 18) {
  value_range <- range(x, finite = TRUE)
  padding <- max(diff(value_range) * 0.01, 1e-6)

  histogram <- hist(
    x,
    breaks = seq(
      value_range[1] - padding,
      value_range[2] + padding,
      length.out = bins + 1
    ),
    plot = FALSE
  )

  xy(histogram$mids, histogram$counts)
}

hist_chart <- function(id, x_title, data) {
  highchart() |>
    hc_chart(type = "column") |>
    hc_xAxis(title = list(text = x_title)) |>
    hc_yAxis(title = list(text = "Borrowers")) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(pointFormat = "Borrowers: <b>{point.y}</b>") |>
    hc_add_series(id = id, data = data)
}

cards <- list(
  c(
    "loss",
    "Loss",
    "Binary log-loss is minimized by training. Validation rising while train keeps falling is a classic overfitting signal."
  ),
  c(
    "auc",
    "AUC",
    "Ranking quality for bad vs good borrowers. Higher is better; the lines compare train and validation."
  ),
  c(
    "gap",
    "Overfitting gap",
    "Validation log-loss minus train log-loss. A growing positive gap means train is improving faster than validation."
  ),
  c(
    "learning_rate",
    "Learning rate",
    "How strongly each new tree can change the model. It is constant within a run in this version."
  ),
  c(
    "gradient_norm",
    "Gradient norm",
    "Size of the remaining log-loss gradient on train. Smaller values mean less error signal remains."
  ),
  c(
    "update_magnitude",
    "Update magnitude",
    "Average absolute validation PD change from the latest 10 trees. It should shrink near convergence."
  ),
  c(
    "gradient_distribution",
    "Gradient distribution",
    "Training gradients p − y at the selected step: the errors the next trees still need to correct."
  ),
  c(
    "leaf_embedding",
    "Leaf embedding",
    "Borrowers are close when they repeatedly land in the same leaves at the selected step."
  ),
  c(
    "tree_contributions",
    "Tree contributions",
    "Distribution of validation PD changes from the previous 10-tree snapshot to the selected step."
  )
)

curve_ids <- c(
  "loss",
  "auc",
  "gap",
  "learning_rate",
  "gradient_norm",
  "update_magnitude"
)

# ui ----------------------------------------------------------------------
apptheme <- theme_vdl()
sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(
  bslib::card,
  full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 0)
)

chart_cards <- lapply(cards, function(x) {
  card(
    chart_header_help(x[2], x[3]),
    highchartOutput(x[1], height = "100%")
  )
})

ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "XGBoost Training",
      sliderInput(
        "eta",
        input_label_vdl("Learning rate", "Contribution of each new tree."),
        0.01,
        0.30,
        0.05,
        0.01
      ),
      sliderInput(
        "max_depth",
        input_label_vdl("Tree depth", "Maximum complexity of each tree."),
        1,
        8,
        4,
        1
      ),
      sliderInput(
        "subsample",
        input_label_vdl(
          "Subsample",
          "Fraction of training borrowers used by each tree."
        ),
        0.5,
        1,
        0.8,
        0.1
      ),
      sliderInput(
        "trees",
        input_label_vdl("Trees", "Maximum number of boosting rounds."),
        50,
        300,
        200,
        10
      ),
      actionButton(
        "train_model",
        "Train model",
        class = "btn-primary w-100"
      ),
      hr(),
      sliderInput(
        "step",
        input_label_vdl(
          "Inspect step",
          "Move through training every 10 trees without retraining."
        ),
        20,
        200,
        100,
        10
      ),
      uiOutput("run_info"),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
        )
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    do.call(
      layout_columns,
      c(
        list(
          col_widths = rep(4, 9),
          row_heights = rep(1, 3),
          gap = "0.75rem"
        ),
        chart_cards
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  run <- eventReactive(
    input$train_model,
    {
      set.seed(2026)

      fit <- xgb.train(
        data = dtrain,
        nrounds = input$trees,
        evals = list(train = dtrain, validation = dvalidation),
        params = list(
          objective = "binary:logistic",
          eval_metric = c("logloss", "auc"),
          eta = input$eta,
          max_depth = input$max_depth,
          subsample = input$subsample,
          colsample_bytree = 0.8,
          nthread = 1
        ),
        verbose = 0
      )

      evaluation_log <- attr(fit, "evaluation_log", exact = TRUE)

      if (is.null(evaluation_log)) {
        evaluation_log <- fit$evaluation_log
      }

      if (is.null(evaluation_log)) {
        stop("XGBoost did not return an evaluation log.")
      }

      evaluation_log <- as.data.frame(evaluation_log)
      names(evaluation_log) <- gsub(
        "-",
        "_",
        names(evaluation_log),
        fixed = TRUE
      )
      evaluation_log$step <- seq_len(nrow(evaluation_log))

      steps <- seq(10L, input$trees, by = 10L)

      train_pred <- vapply(
        steps,
        function(k) {
          predict(
            fit,
            dtrain,
            iterationrange = c(1L, k)
          )
        },
        numeric(nrow(train))
      )

      validation_pred <- vapply(
        steps,
        function(k) {
          predict(
            fit,
            dvalidation,
            iterationrange = c(1L, k)
          )
        },
        numeric(nrow(validation))
      )

      gradients <- sweep(train_pred, 1, y_train, "-")

      update <- colMeans(abs(
        validation_pred[, -1, drop = FALSE] -
          validation_pred[, -ncol(validation_pred), drop = FALSE]
      ))

      list(
        fit = fit,
        log = evaluation_log,
        steps = steps,
        gradients = gradients,
        validation_pred = validation_pred,
        gradient_norm = sqrt(colMeans(gradients^2)),
        update = update,
        eta = input$eta,
        depth = input$max_depth,
        subsample = input$subsample,
        trees = input$trees
      )
    },
    ignoreNULL = FALSE
  )

  curves <- reactive({
    x <- run()

    series <- function(id, name, x_value, y_value) {
      list(
        id = id,
        name = name,
        x = x_value,
        y = y_value
      )
    }

    list(
      loss = list(
        "Log-loss",
        list(
          series(
            "train_loss",
            "Train",
            x$log$step,
            x$log$train_logloss
          ),
          series(
            "validation_loss",
            "Validation",
            x$log$step,
            x$log$validation_logloss
          )
        )
      ),
      auc = list(
        "AUC",
        list(
          series(
            "train_auc",
            "Train",
            x$log$step,
            x$log$train_auc
          ),
          series(
            "validation_auc",
            "Validation",
            x$log$step,
            x$log$validation_auc
          )
        )
      ),
      gap = list(
        "Log-loss gap",
        list(series(
          "gap",
          "Validation − train",
          x$log$step,
          x$log$validation_logloss - x$log$train_logloss
        ))
      ),
      learning_rate = list(
        "η",
        list(series(
          "eta",
          "Learning rate",
          x$log$step,
          rep(x$eta, nrow(x$log))
        ))
      ),
      gradient_norm = list(
        "Gradient norm",
        list(series(
          "gradient_norm",
          "Gradient norm",
          x$steps,
          x$gradient_norm
        ))
      ),
      update_magnitude = list(
        "Mean |Δ PD|",
        list(series(
          "update",
          "Latest 10 trees",
          x$steps[-1],
          x$update
        ))
      )
    )
  })

  snapshot_at <- function(step) {
    x <- run()
    index <- which.min(abs(x$steps - step))
    trees <- x$steps[index]

    leaf <- predict(
      x$fit,
      dvalidation,
      predleaf = TRUE,
      iterationrange = c(1L, trees)
    )

    if (is.null(dim(leaf))) {
      leaf <- matrix(leaf, ncol = 1)
    }

    similarity <- matrix(0, nrow(leaf), nrow(leaf))

    for (j in seq_len(ncol(leaf))) {
      similarity <- similarity + outer(leaf[, j], leaf[, j], `==`)
    }

    embedding <- stats::cmdscale(
      stats::as.dist(1 - similarity / ncol(leaf)),
      k = 2
    )

    list(
      gradient = hist_points(x$gradients[, index]),
      contribution = hist_points(
        x$validation_pred[, index] -
          x$validation_pred[, max(1L, index - 1L)]
      ),
      good = xy(
        embedding[y_validation == 0, 1],
        embedding[y_validation == 0, 2]
      ),
      bad = xy(
        embedding[y_validation == 1, 1],
        embedding[y_validation == 1, 2]
      )
    )
  }

  update_step_lines <- function(step) {
    for (id in curve_ids) {
      highchartProxy(id) |>
        hcpxy_update(
          xAxis = list(plotLines = step_line(step))
        )
    }
  }

  update_curve_series <- function(step) {
    curve_data <- curves()

    for (id in curve_ids) {
      specification <- curve_data[[id]]
      proxy <- highchartProxy(id)

      for (series_i in specification[[2]]) {
        proxy <- proxy |>
          hcpxy_update_series(
            id = series_i$id,
            data = xy(series_i$x, series_i$y)
          )
      }

      proxy |>
        hcpxy_update(
          xAxis = list(plotLines = step_line(step))
        )
    }
  }

  update_snapshot_series <- function(step) {
    snapshot <- snapshot_at(step)

    highchartProxy("gradient_distribution") |>
      hcpxy_update_series(
        id = "gradient_hist",
        data = snapshot$gradient
      )

    highchartProxy("leaf_embedding") |>
      hcpxy_update_series(
        id = "good",
        data = snapshot$good
      ) |>
      hcpxy_update_series(
        id = "bad",
        data = snapshot$bad
      )

    highchartProxy("tree_contributions") |>
      hcpxy_update_series(
        id = "contribution_hist",
        data = snapshot$contribution
      )
  }

  for (id in curve_ids) {
    local({
      chart_id <- id

      output[[chart_id]] <- renderHighchart({
        specification <- isolate(curves()[[chart_id]])
        selected_step <- isolate(input$step)

        chart <- line_chart(
          specification[[1]],
          selected_step,
          specification[[2]]
        )

        if (chart_id == "gap") {
          chart <- chart |>
            hc_yAxis(
              title = list(text = "Log-loss gap"),
              plotLines = list(list(
                value = 0,
                color = "#adb5bd",
                width = 1
              ))
            )
        }

        chart
      })
    })
  }

  output$gradient_distribution <- renderHighchart({
    snapshot <- isolate(snapshot_at(input$step))

    hist_chart(
      "gradient_hist",
      "Gradient (p − y)",
      snapshot$gradient
    )
  })

  output$tree_contributions <- renderHighchart({
    snapshot <- isolate(snapshot_at(input$step))

    hist_chart(
      "contribution_hist",
      "Δ probability of default",
      snapshot$contribution
    )
  })

  output$leaf_embedding <- renderHighchart({
    snapshot <- isolate(snapshot_at(input$step))

    highchart() |>
      hc_chart(type = "scatter") |>
      hc_xAxis(title = list(text = "Leaf space 1")) |>
      hc_yAxis(title = list(text = "Leaf space 2")) |>
      hc_tooltip(pointFormat = "{point.x:.2f}, {point.y:.2f}") |>
      hc_add_series(
        id = "good",
        name = "Good",
        data = snapshot$good
      ) |>
      hc_add_series(
        id = "bad",
        name = "Bad",
        data = snapshot$bad
      )
  })

  output$run_info <- renderUI({
    x <- run()
    best <- x$log$step[which.min(x$log$validation_logloss)]

    tags$small(
      class = "text-muted",
      sprintf(
        "η %.2f · depth %d · subsample %.1f · best loss at tree %d",
        x$eta,
        x$depth,
        x$subsample,
        best
      )
    )
  })

  observeEvent(
    input$train_model,
    {
      x <- run()
      selected_step <- min(max(20L, input$step), x$trees)

      updateSliderInput(
        session,
        "step",
        min = 20,
        max = x$trees,
        value = selected_step,
        step = 10
      )

      update_curve_series(selected_step)
      update_snapshot_series(selected_step)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$step,
    {
      req(run())

      update_step_lines(input$step)
      update_snapshot_series(input$step)
    },
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)
