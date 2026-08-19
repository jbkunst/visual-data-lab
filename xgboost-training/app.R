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
  label = train$status_bad, nthread = 1
)
dvalidation <- xgb.DMatrix(
  as.matrix(validation[, predictors, drop = FALSE]),
  label = validation$status_bad, nthread = 1
)
y_train <- train$status_bad
y_validation <- validation$status_bad
# helpers -----------------------------------------------------------------
options(highcharter.theme = highcharter_theme_vdl())
xy <- function(x, y) list_parse2(data.frame(x, y))
step_line <- function(step) list(list(
  value = step, color = "#adb5bd", width = 1,
  dashStyle = "ShortDash", zIndex = 4
))
chart_header_help <- function(title, help) {
  card_header(div(
    class = "d-flex align-items-center gap-1", span(title),
    tooltip(
      tags$span("ⓘ", class = "text-muted", tabindex = "0"),
      help, placement = "right",
      options = list(trigger = "hover focus click")
    )
  ))
}
line_chart <- function(y_title, step, series) {
  chart <- highchart() |>
    hc_chart(type = "line") |>
    hc_xAxis(title = list(text = "Trees"), min = 1, plotLines = step_line(step)) |>
    hc_yAxis(title = list(text = y_title)) |>
    hc_tooltip(shared = TRUE, valueDecimals = 4) |>
    hc_plotOptions(series = list(
      animation = list(duration = 250), lineWidth = 2,
      marker = list(enabled = FALSE)
    ))
  for (s in series) {
    chart <- chart |> hc_add_series(id = s$id, name = s$name, data = xy(s$x, s$y))
  }
  chart
}
hist_points <- function(x, bins = 18) {
  r <- range(x, finite = TRUE)
  pad <- max(diff(r) * 0.01, 1e-6)
  h <- hist(x, breaks = seq(r[1] - pad, r[2] + pad, length.out = bins + 1), plot = FALSE)
  xy(h$mids, h$counts)
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
  c("loss", "Loss", "Binary log-loss is minimized by training. Validation rising while train keeps falling is a classic overfitting signal."),
  c("auc", "AUC", "Ranking quality for bad vs good borrowers. Higher is better; the lines compare train and validation."),
  c("gap", "Overfitting gap", "Validation log-loss minus train log-loss. A growing positive gap means train is improving faster than validation."),
  c("learning_rate", "Learning rate", "How strongly each new tree can change the model. It is constant within a run in this version."),
  c("gradient_norm", "Gradient norm", "Size of the remaining log-loss gradient on train. Smaller values mean less error signal remains."),
  c("update_magnitude", "Update magnitude", "Average absolute validation PD change from the latest 10 trees. It should shrink near convergence."),
  c("gradient_distribution", "Gradient distribution", "Training gradients p − y at the selected step: the errors the next trees still need to correct."),
  c("leaf_embedding", "Leaf embedding", "Borrowers are close when they repeatedly land in the same leaves at the selected step."),
  c("tree_contributions", "Tree contributions", "Distribution of validation PD changes from the previous 10-tree snapshot to the selected step.")
)
# ui ----------------------------------------------------------------------
apptheme <- theme_vdl()
sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(
  bslib::card, full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 0)
)
chart_cards <- lapply(cards, \(x) card(
  chart_header_help(x[2], x[3]),
  highchartOutput(x[1], height = "100%")
))
ui <- page_fillable(
  theme = apptheme, padding = 0,
  layout_sidebar(
    fillable = TRUE, padding = "0.75rem",
    sidebar = sidebar(
      title = "XGBoost Training",
      sliderInput("eta", input_label_vdl("Learning rate", "Contribution of each new tree."), 0.01, 0.30, 0.05, 0.01),
      sliderInput("max_depth", input_label_vdl("Tree depth", "Maximum complexity of each tree."), 1, 8, 4, 1),
      sliderInput("subsample", input_label_vdl("Subsample", "Fraction of training borrowers used by each tree."), 0.5, 1, 0.8, 0.1),
      sliderInput("trees", input_label_vdl("Trees", "Maximum number of boosting rounds."), 50, 300, 200, 10),
      actionButton("train_model", "Train model", class = "btn-primary w-100"),
      hr(),
      sliderInput("step", input_label_vdl(
        "Inspect step", "Move through training every 10 trees without retraining."
      ), 20, 200, 100, 10),
      uiOutput("run_info"),
      accordion(
        open = FALSE,
        accordion_panel("How it works", tags$small(htmltools::includeMarkdown("readme.md")))
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    do.call(layout_columns, c(
      list(col_widths = rep(4, 9), row_heights = rep(1, 3), gap = "0.75rem"),
      chart_cards
    ))
  )
)
# server ------------------------------------------------------------------
server <- function(input, output, session) {
  run <- eventReactive(input$train_model, {
    set.seed(2026)
    fit <- xgb.train(
      data = dtrain, nrounds = input$trees,
      evals = list(train = dtrain, validation = dvalidation),
      params = list(
        objective = "binary:logistic", eval_metric = c("logloss", "auc"),
        eta = input$eta, max_depth = input$max_depth,
        subsample = input$subsample, colsample_bytree = 0.8, nthread = 1
      ),
      verbose = 0
    )
    log <- as.data.frame(fit$evaluation_log)
    names(log) <- gsub("-", "_", names(log), fixed = TRUE)
    steps <- seq(10L, input$trees, by = 10L)
    train_pred <- vapply(
      steps, \(k) predict(fit, dtrain, iterationrange = c(1L, k)),
      numeric(nrow(train))
    )
    validation_pred <- vapply(
      steps, \(k) predict(fit, dvalidation, iterationrange = c(1L, k)),
      numeric(nrow(validation))
    )
    gradients <- sweep(train_pred, 1, y_train, "-")
    update <- colMeans(abs(
      validation_pred[, -1, drop = FALSE] -
        validation_pred[, -ncol(validation_pred), drop = FALSE]
    ))
    list(
      fit = fit, log = log, steps = steps, gradients = gradients,
      validation_pred = validation_pred,
      gradient_norm = sqrt(colMeans(gradients^2)), update = update,
      eta = input$eta, depth = input$max_depth,
      subsample = input$subsample, trees = input$trees
    )
  }, ignoreNULL = FALSE)
  curves <- reactive({
    x <- run()
    s <- function(id, name, xval, yval) list(id = id, name = name, x = xval, y = yval)
    list(
      loss = list("Log-loss", list(
        s("train_loss", "Train", x$log$iter, x$log$train_logloss),
        s("validation_loss", "Validation", x$log$iter, x$log$validation_logloss)
      )),
      auc = list("AUC", list(
        s("train_auc", "Train", x$log$iter, x$log$train_auc),
        s("validation_auc", "Validation", x$log$iter, x$log$validation_auc)
      )),
      gap = list("Log-loss gap", list(s(
        "gap", "Validation − train", x$log$iter,
        x$log$validation_logloss - x$log$train_logloss
      ))),
      learning_rate = list("η", list(s(
        "eta", "Learning rate", x$log$iter, rep(x$eta, nrow(x$log))
      ))),
      gradient_norm = list("Gradient norm", list(s(
        "gradient_norm", "Gradient norm", x$steps, x$gradient_norm
      ))),
      update_magnitude = list("Mean |Δ PD|", list(s(
        "update", "Latest 10 trees", x$steps[-1], x$update
      )))
    )
  })
  for (id in c("loss", "auc", "gap", "learning_rate", "gradient_norm", "update_magnitude")) {
    local({
      chart_id <- id
      output[[chart_id]] <- renderHighchart({
        spec <- curves()[[chart_id]]
        chart <- line_chart(spec[[1]], input$step, spec[[2]])
        if (chart_id == "gap") {
          chart <- chart |> hc_yAxis(
            title = list(text = "Log-loss gap"),
            plotLines = list(list(value = 0, color = "#adb5bd", width = 1))
          )
        }
        chart
      })
    })
  }
  snapshot <- reactive({
    x <- run()
    i <- which.min(abs(x$steps - input$step))
    k <- x$steps[i]
    leaf <- predict(
      x$fit, dvalidation, predleaf = TRUE,
      iterationrange = c(1L, k)
    )
    if (is.null(dim(leaf))) leaf <- matrix(leaf, ncol = 1)
    similarity <- matrix(0, nrow(leaf), nrow(leaf))
    for (j in seq_len(ncol(leaf))) {
      similarity <- similarity + outer(leaf[, j], leaf[, j], `==`)
    }
    emb <- stats::cmdscale(stats::as.dist(1 - similarity / ncol(leaf)), k = 2)
    list(
      gradient = hist_points(x$gradients[, i]),
      contribution = hist_points(
        x$validation_pred[, i] - x$validation_pred[, max(1L, i - 1L)]
      ),
      good = xy(emb[y_validation == 0, 1], emb[y_validation == 0, 2]),
      bad = xy(emb[y_validation == 1, 1], emb[y_validation == 1, 2])
    )
  })
  output$run_info <- renderUI({
    x <- run()
    best <- x$log$iter[which.min(x$log$validation_logloss)]
    tags$small(class = "text-muted", sprintf(
      "η %.2f · depth %d · subsample %.1f · best loss at tree %d",
      x$eta, x$depth, x$subsample, best
    ))
  })
  output$gradient_distribution <- renderHighchart(
    hist_chart("gradient_hist", "Gradient (p − y)", snapshot()$gradient)
  )
  output$tree_contributions <- renderHighchart(
    hist_chart("contribution_hist", "Δ probability of default", snapshot()$contribution)
  )
  output$leaf_embedding <- renderHighchart({
    s <- snapshot()
    highchart() |>
      hc_chart(type = "scatter") |>
      hc_xAxis(title = list(text = "Leaf space 1")) |>
      hc_yAxis(title = list(text = "Leaf space 2")) |>
      hc_tooltip(pointFormat = "{point.x:.2f}, {point.y:.2f}") |>
      hc_add_series(id = "good", name = "Good", data = s$good) |>
      hc_add_series(id = "bad", name = "Bad", data = s$bad)
  })
  observeEvent(run(), {
    updateSliderInput(
      session, "step", min = 20, max = input$trees,
      value = min(max(20, input$step), input$trees), step = 10
    )
  })
  observeEvent(input$step, {
    req(run())
    for (id in c("loss", "auc", "gap", "learning_rate", "gradient_norm", "update_magnitude")) {
      highchartProxy(id) |>
        hcpxy_update(xAxis = list(plotLines = step_line(input$step)))
    }
    s <- snapshot()
    highchartProxy("gradient_distribution") |>
      hcpxy_update_series(id = "gradient_hist", data = s$gradient)
    highchartProxy("leaf_embedding") |>
      hcpxy_update_series(id = "good", data = s$good) |>
      hcpxy_update_series(id = "bad", data = s$bad)
    highchartProxy("tree_contributions") |>
      hcpxy_update_series(id = "contribution_hist", data = s$contribution)
  }, ignoreInit = TRUE)
}
shinyApp(ui, server)
