# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(vdltheme)

# theme -------------------------------------------------------------------
apptheme <- theme_vdl()
sidebar <- purrr::partial(bslib::sidebar, width = 290)
card <- purrr::partial(
  bslib::card,
  full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 0)
)
thematic::thematic_shiny(font = "auto")

# data --------------------------------------------------------------------
make_data <- function(scenario, n, seed) {
  set.seed(seed)
  data <- switch(
    scenario,
    diagonal = {
      x1 <- rnorm(n)
      x2 <- rnorm(n)
      score <- x1 + x2 + rnorm(n, sd = 0.25)
      data.frame(x1, x2, class = cut(
        score, c(-Inf, -0.75, 0.75, Inf), labels = c("A", "B", "C")
      ))
    },
    islands = {
      x1 <- runif(n, -3, 3)
      x2 <- rnorm(n)
      class <- ifelse(abs(x1) > 1.3, "A", ifelse(x2 > 0, "B", "C"))
      flip <- sample.int(n, ceiling(0.05 * n))
      class[flip] <- sample(c("A", "B", "C"), length(flip), replace = TRUE)
      data.frame(x1, x2, class = factor(class, levels = c("A", "B", "C")))
    },
    iris = data.frame(
      x1 = iris$Petal.Length, x2 = iris$Petal.Width, class = iris$Species
    )
  )
  train <- unlist(lapply(
    split(seq_len(nrow(data)), data$class),
    function(i) sample(i, max(1, floor(0.7 * length(i))))
  ))
  data$partition <- ifelse(seq_len(nrow(data)) %in% train, "train", "test")
  data$class <- droplevels(data$class)
  data
}

# models ------------------------------------------------------------------
fit_knn <- function(data, k = 15) {
  x <- as.matrix(data[c("x1", "x2")])
  center <- colMeans(x)
  spread <- apply(x, 2, sd)
  spread[!is.finite(spread) | spread == 0] <- 1
  k <- min(k, nrow(data) - 1L)
  if (k %% 2 == 0) k <- k - 1L
  list(
    train = sweep(sweep(x, 2, center), 2, spread, "/"),
    class = data$class,
    center = center,
    spread = spread,
    k = k
  )
}
predict_knn <- function(model, newdata) {
  test <- as.matrix(newdata[c("x1", "x2")])
  test <- sweep(sweep(test, 2, model$center), 2, model$spread, "/")
  class::knn(model$train, test, model$class, k = model$k)
}
fit_xgboost <- function(data) {
  levels <- levels(data$class)
  matrix <- as.matrix(data[c("x1", "x2")])
  model <- xgboost::xgb.train(
    params = list(
      objective = "multi:softprob", num_class = length(levels),
      max_depth = 4, eta = 0.15, nthread = 1
    ),
    data = xgboost::xgb.DMatrix(matrix, label = as.integer(data$class) - 1L),
    nrounds = 60, verbose = 0
  )
  list(model = model, levels = levels)
}
predict_xgboost <- function(model, newdata) {
  probability <- predict(model$model, as.matrix(newdata[c("x1", "x2")]))
  if (is.null(dim(probability))) {
    probability <- matrix(probability, ncol = length(model$levels), byrow = TRUE)
  }
  factor(model$levels[max.col(probability)], levels = model$levels)
}
models <- list(
  multinomial = list(
    label = "Multinomial", subtitle = "linear probabilities",
    fit = \(d) nnet::multinom(class ~ x1 + x2, d, trace = FALSE),
    predict = \(m, d) predict(m, d, type = "class")
  ),
  knn = list(
    label = "KNN", subtitle = "local neighbourhoods",
    fit = fit_knn,
    predict = predict_knn
  ),
  svm = list(
    label = "SVM", subtitle = "smooth radial boundary",
    fit = \(d) e1071::svm(
      class ~ x1 + x2, d, kernel = "radial", cost = 10, gamma = 0.5, scale = TRUE
    ),
    predict = \(m, d) predict(m, d)
  ),
  rpart = list(
    label = "rpart", subtitle = "rectangular tree",
    fit = \(d) rpart::rpart(
      class ~ x1 + x2, d, method = "class",
      control = rpart::rpart.control(cp = 0, maxdepth = 5, minsplit = 12)
    ),
    predict = \(m, d) predict(m, d, type = "class")
  ),
  pptree = list(
    label = "PPtree", subtitle = "oblique tree",
    fit = \(d) PPtreeViz::PPTreeclass(class ~ x1 + x2, d, PPmethod = "LDA"),
    predict = \(m, d) predict(m, d, Rule = 1)
  ),
  pptree_ext = list(
    label = "PPtreeExt", subtitle = "flexible oblique tree",
    fit = \(d) PPtreeExt::PPtreeExtclass(
      class ~ x1 + x2, d, PPmethod = "LDA",
      srule = TRUE, tot = nrow(d), tol = 0.2
    ),
    predict = \(m, d) predict(m, d)$predict.class
  ),
  random_forest = list(
    label = "Random forest", subtitle = "rectangular forest",
    fit = \(d) randomForest::randomForest(
      class ~ x1 + x2, d, ntree = 100, mtry = 2
    ),
    predict = \(m, d) predict(m, d)
  ),
  ppforest = list(
    label = "PPforest", subtitle = "oblique forest",
    fit = \(d) PPforest::PPforest(
      d, y = "class", std = "scale", size.tr = 1,
      m = 100, PPmethod = "LDA", size.p = 1, parallel = FALSE
    ),
    predict = \(m, d) predict(m, d, parallel = FALSE)[[3]]
  ),
  xgboost = list(
    label = "XGBoost", subtitle = "boosted rectangular trees",
    fit = fit_xgboost,
    predict = predict_xgboost
  )
)

# plots -------------------------------------------------------------------
decision_grid <- function(data, length = 80) {
  x_range <- range(data$x1)
  y_range <- range(data$x2)
  x_pad <- max(diff(x_range) * 0.06, 0.05)
  y_pad <- max(diff(y_range) * 0.06, 0.05)
  expand.grid(
    x1 = seq(x_range[1] - x_pad, x_range[2] + x_pad, length.out = length),
    x2 = seq(y_range[1] - y_pad, y_range[2] + y_pad, length.out = length)
  )
}
class_colors <- function(data) {
  colors <- c("#7E57C2", "#009E73", "#E69F00")
  setNames(colors[seq_len(nlevels(data$class))], levels(data$class))
}
draw_surface <- function(data, grid, prediction) {
  colors <- class_colors(data)
  classes <- names(colors)
  xs <- sort(unique(grid$x1))
  ys <- sort(unique(grid$x2))
  z <- matrix(match(as.character(prediction), classes), nrow = length(xs))
  par(mar = c(3.3, 3.3, 0.5, 0.5))
  image(
    xs, ys, z,
    col = grDevices::adjustcolor(colors, alpha.f = 0.22),
    breaks = seq(0.5, length(classes) + 0.5),
    xlab = "x1", ylab = "x2", asp = 1, useRaster = TRUE
  )
  points(
    data$x1, data$x2,
    pch = 16, cex = 0.68,
    col = colors[as.character(data$class)]
  )
  box(col = "#ced4da")
}

draw_comparison <- function(scores, fit_time, predict_time, predictions, labels, classes) {
  score <- do.call(rbind, scores)
  old <- par(mfrow = c(2, 2), mar = c(6, 5, 2.5, 0.8), cex = 0.85)
  on.exit(par(old))

  barplot(
    t(score[, c("train", "test")]), beside = TRUE, names.arg = labels,
    col = c("#7E57C2", "#009E73"), las = 2, ylim = c(0, 1),
    ylab = "Accuracy", main = "Train vs test"
  )
  legend("bottomleft", c("Train", "Test"), fill = c("#7E57C2", "#009E73"), bty = "n")

  barplot(
    1000 * rbind(fit = fit_time, predict = predict_time),
    beside = TRUE, names.arg = labels, col = c("#56B4E9", "#E69F00"), las = 2,
    ylab = "Milliseconds", main = "Execution time"
  )
  legend("topleft", c("Fit", "Predict grid"), fill = c("#56B4E9", "#E69F00"), bty = "n")

  recall <- score[, paste0("recall_", classes), drop = FALSE]
  image(
    seq_len(nrow(recall)), seq_len(ncol(recall)), recall,
    col = hcl.colors(10, "BluYl"), zlim = c(0, 1), axes = FALSE,
    xlab = "", ylab = "", main = "Test recall by class"
  )
  axis(1, seq_along(labels), labels, las = 2)
  axis(2, seq_along(classes), classes, las = 1)
  text(
    rep(seq_len(nrow(recall)), ncol(recall)),
    rep(seq_len(ncol(recall)), each = nrow(recall)),
    sprintf("%.0f", 100 * recall), cex = 0.7
  )

  agreement <- vapply(predictions, function(reference) {
    vapply(predictions, function(prediction) mean(prediction == reference), numeric(1))
  }, numeric(length(predictions)))
  image(
    seq_along(labels), seq_along(labels), agreement,
    col = hcl.colors(10, "BluYl"), zlim = c(0, 1), axes = FALSE,
    xlab = "", ylab = "", main = "Agreement on prediction grid"
  )
  axis(1, seq_along(labels), labels, las = 2)
  axis(2, seq_along(labels), labels, las = 1)
  text(
    rep(seq_along(labels), length(labels)),
    rep(seq_along(labels), each = length(labels)),
    sprintf("%.0f", 100 * agreement), cex = 0.55
  )
}

model_card <- function(id, model) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center gap-2 w-100",
        div(
          model$label,
          tags$small(class = "d-block fw-normal text-muted", model$subtitle)
        ),
        uiOutput(paste0(id, "_metrics"), inline = TRUE)
      )
    ),
    plotOutput(paste0(id, "_plot"), height = 230)
  )
}
cards <- Map(model_card, names(models), models)
model_grid <- do.call(layout_columns, c(cards, list(col_widths = 4, gap = "0.75rem")))

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = FALSE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Multiclass regions",
      selectInput(
        "scenario", tags$small("Dataset"),
        c(
          "Two islands" = "islands",
          "Diagonal bands" = "diagonal",
          "Iris petals" = "iris"
        )
      ),
      conditionalPanel(
        "input.scenario != 'iris'",
        sliderInput("n", tags$small("Observations"), 150, 600, 300, step = 50)
      ),
      actionButton("resample", "New sample", class = "btn-primary w-100"),
      actionButton("compare", "Compare results", class = "btn-outline-primary w-100"),
      uiOutput("class_legend"),
      tags$small("The background is the predicted class, not a probability surface."),
      accordion(open = FALSE, accordion_panel(
        "How it works", tags$small(htmltools::includeMarkdown("readme.md"))
      )),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    model_grid
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  seed <- reactiveVal(2026L)
  observeEvent(input$resample, seed(seed() + 1L))
  data <- reactive(make_data(input$scenario, input$n, seed()))
  fits <- reactive({
    train <- data()[data()$partition == "train", c("x1", "x2", "class")]
    lapply(models, function(model) {
      elapsed <- system.time(fit <- model$fit(train))[["elapsed"]]
      list(model = fit, time = elapsed)
    })
  })

  surfaces <- reactive({
    grid <- decision_grid(data())
    results <- Map(
      function(model, fit) {
        elapsed <- system.time({
          prediction <- as.character(model$predict(fit$model, grid))
        })[["elapsed"]]
        list(prediction = prediction, time = elapsed)
      },
      models, fits()
    )
    list(grid = grid, results = results)
  })

  scores <- reactive({
    d <- data()
    predictions <- Map(
      function(model, fit) as.character(model$predict(fit$model, d)),
      models, fits()
    )
    classes <- levels(d$class)
    lapply(predictions, function(prediction) {
      correct <- prediction == as.character(d$class)
      recall <- vapply(classes, function(class) {
        test_class <- d$partition == "test" & d$class == class
        mean(correct[test_class])
      }, numeric(1))
      c(
        train = mean(correct[d$partition == "train"]),
        test = mean(correct[d$partition == "test"]),
        setNames(recall, paste0("recall_", classes))
      )
    })
  })

  observeEvent(input$compare, showModal(modalDialog(
    title = "Compare models",
    plotOutput("comparison_plot", height = 720),
    size = "xl", easyClose = TRUE, footer = modalButton("Close")
  )))

  output$comparison_plot <- renderPlot({
    draw_comparison(
      scores(),
      vapply(fits(), \(x) x$time, numeric(1)),
      vapply(surfaces()$results, \(x) x$time, numeric(1)),
      lapply(surfaces()$results, \(x) x$prediction),
      vapply(models, \(x) x$label, character(1)),
      levels(data()$class)
    )
  }, res = 110)

  output$class_legend <- renderUI({
    colors <- class_colors(data())
    div(
      class = "d-flex flex-wrap gap-3",
      Map(function(label, color) {
        tags$span(
          class = "text-nowrap",
          tags$span(style = paste0("color:", color), "\u25CF"),
          label
        )
      }, names(colors), colors)
    )
  })
  invisible(lapply(names(models), function(id) {
    output[[paste0(id, "_plot")]] <- renderPlot({
      surface <- surfaces()
      draw_surface(data(), surface$grid, surface$results[[id]]$prediction)
    }, res = 110)
    output[[paste0(id, "_metrics")]] <- renderUI({
      score <- scores()[[id]]
      tags$small(
        class = "text-nowrap fw-normal",
        sprintf("train %.0f%% · test %.0f%%", 100 * score["train"], 100 * score["test"])
      )
    })
  }))
}
shinyApp(ui, server)
