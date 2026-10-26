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
class_palette <- c("#f59322", "#0877bd", "#cc79a7")
dataset_labels <- c(
  diagonal = "Diagonal bands",
  islands = "Two islands",
  checkerboard = "XOR checkerboard",
  ellipses = "Gaussian ellipses",
  rings = "Concentric rings",
  spirals = "Three spirals",
  iris = "Iris petals",
  penguins = "Palmer penguins"
)

make_ellipse <- function(n, center, axes, angle) {
  rotation <- matrix(
    c(cos(angle), -sin(angle), sin(angle), cos(angle)),
    nrow = 2, byrow = TRUE
  )
  points <- matrix(rnorm(2 * n), ncol = 2) %*% diag(axes) %*% t(rotation)
  sweep(points, 2, center, "+")
}

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
    checkerboard = {
      x1 <- runif(n, -2, 2)
      x2 <- runif(n, -2, 2)
      col <- cut(x1, c(-Inf, -2 / 3, 2 / 3, Inf), labels = FALSE)
      row <- cut(x2, c(-Inf, -2 / 3, 2 / 3, Inf), labels = FALSE)
      class <- LETTERS[(col + row - 2L) %% 3L + 1L]
      flip <- sample.int(n, ceiling(0.04 * n))
      class[flip] <- sample(LETTERS[1:3], length(flip), replace = TRUE)
      data.frame(x1, x2, class = factor(class, levels = LETTERS[1:3]))
    },
    rings = {
      class <- factor(rep(c("A", "B", "C"), length.out = n))
      radius <- c(A = 0.65, B = 1.35, C = 2.05)[class] +
        rnorm(n, sd = 0.12)
      angle <- runif(n, 0, 2 * pi)
      data.frame(
        x1 = radius * cos(angle), x2 = radius * sin(angle), class
      )
    },
    spirals = {
      class_id <- rep(0:2, length.out = n)
      progress <- runif(n, 0.08, 1)
      radius <- 0.25 + 2.4 * progress + rnorm(n, sd = 0.06)
      angle <- 3 * pi * progress + 2 * pi * class_id / 3 +
        rnorm(n, sd = 0.12)
      data.frame(
        x1 = radius * cos(angle),
        x2 = radius * sin(angle),
        class = factor(LETTERS[class_id + 1L], levels = LETTERS[1:3])
      )
    },
    ellipses = {
      class_id <- rep(1:3, length.out = n)
      centers <- list(c(-1.3, -0.6), c(1.3, -0.5), c(0, 1.2))
      axes <- list(c(0.75, 0.28), c(0.65, 0.35), c(0.8, 0.25))
      angles <- c(0.5, -0.5, 0)
      data <- do.call(rbind, lapply(1:3, function(i) {
        points <- make_ellipse(
          sum(class_id == i), centers[[i]], axes[[i]], angles[i]
        )
        data.frame(x1 = points[, 1], x2 = points[, 2], class = LETTERS[i])
      }))
      data$class <- factor(data$class, levels = LETTERS[1:3])
      data[sample(nrow(data)), ]
    },
    iris = data.frame(
      x1 = iris$Petal.Length, x2 = iris$Petal.Width, class = iris$Species
    ),
    penguins = {
      penguins <- stats::na.omit(
        palmerpenguins::penguins[
          c("bill_length_mm", "bill_depth_mm", "species")
        ]
      )
      data.frame(
        x1 = penguins$bill_length_mm,
        x2 = penguins$bill_depth_mm,
        class = droplevels(penguins$species)
      )
    }
  )
  axis_labels <- switch(
    scenario,
    iris = c("Petal length", "Petal width"),
    penguins = c("Bill length", "Bill depth"),
    c("x1", "x2")
  )
  train <- unlist(lapply(
    split(seq_len(nrow(data)), data$class),
    function(i) sample(i, max(1, floor(0.7 * length(i))))
  ))
  data$partition <- ifelse(seq_len(nrow(data)) %in% train, "train", "test")
  data$class <- droplevels(data$class)
  attr(data, "axis_labels") <- axis_labels
  data
}

# models ------------------------------------------------------------------
probability_prediction <- function(probability, classes) {
  probability <- as.matrix(probability)
  if (!is.null(colnames(probability))) {
    probability <- probability[, classes, drop = FALSE]
  }
  winner <- max.col(probability, ties.method = "first")
  list(
    class = classes[winner],
    confidence = probability[cbind(seq_len(nrow(probability)), winner)]
  )
}

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
  prediction <- class::knn(
    model$train, test, model$class, k = model$k, prob = TRUE
  )
  list(
    class = as.character(prediction),
    confidence = as.numeric(attr(prediction, "prob"))
  )
}

fit_neural_network <- function(data) {
  x <- as.matrix(data[c("x1", "x2")])
  center <- colMeans(x)
  spread <- apply(x, 2, sd)
  spread[!is.finite(spread) | spread == 0] <- 1
  scaled <- as.data.frame(sweep(sweep(x, 2, center), 2, spread, "/"))
  scaled$class <- data$class
  list(
    model = nnet::nnet(
      class ~ x1 + x2, scaled,
      size = 6, decay = 0.01, maxit = 500, trace = FALSE
    ),
    center = center,
    spread = spread,
    levels = levels(data$class)
  )
}

predict_neural_network <- function(model, newdata) {
  x <- as.matrix(newdata[c("x1", "x2")])
  x <- as.data.frame(sweep(sweep(x, 2, model$center), 2, model$spread, "/"))
  probability <- predict(model$model, x, type = "raw")
  colnames(probability) <- model$levels
  probability_prediction(probability, model$levels)
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
  probability_prediction(probability, model$levels)
}

predict_ppforest <- function(model, newdata) {
  prediction <- predict(model, newdata, parallel = FALSE)
  list(
    class = as.character(prediction[[3]]),
    confidence = colMeans(sweep(prediction[[1]], 2, prediction[[2]], "=="))
  )
}

models <- list(
  multinomial = list(
    label = "Multinomial",
    fit = \(d) nnet::multinom(class ~ x1 + x2, d, trace = FALSE),
    predict = \(m, d) probability_prediction(
      predict(m, d, type = "probs"), m$lev
    )
  ),
  knn = list(
    label = "KNN",
    fit = fit_knn,
    predict = predict_knn
  ),
  svm = list(
    label = "SVM",
    fit = \(d) e1071::svm(
      class ~ x1 + x2, d, kernel = "radial", cost = 10, gamma = 0.5,
      scale = TRUE, probability = TRUE
    ),
    predict = \(m, d) {
      prediction <- predict(m, d, probability = TRUE)
      probability <- attr(prediction, "probabilities")
      probability_prediction(probability, colnames(probability))
    }
  ),
  rpart = list(
    label = "rpart",
    fit = \(d) rpart::rpart(
      class ~ x1 + x2, d, method = "class",
      control = rpart::rpart.control(cp = 0, maxdepth = 5, minsplit = 12)
    ),
    predict = \(m, d) {
      probability <- predict(m, d, type = "prob")
      probability_prediction(probability, colnames(probability))
    }
  ),
  naive_bayes = list(
    label = "Naive Bayes",
    fit = \(d) e1071::naiveBayes(class ~ x1 + x2, d),
    predict = \(m, d) {
      probability <- predict(m, d, type = "raw")
      probability_prediction(probability, colnames(probability))
    }
  ),
  neural_network = list(
    label = "Neural network",
    fit = fit_neural_network,
    predict = predict_neural_network
  ),
  random_forest = list(
    label = "Random forest",
    fit = \(d) randomForest::randomForest(
      class ~ x1 + x2, d, ntree = 100, mtry = 2
    ),
    predict = \(m, d) {
      probability <- predict(m, d, type = "prob")
      probability_prediction(probability, colnames(probability))
    }
  ),
  ppforest = list(
    label = "PPforest",
    fit = \(d) PPforest::PPforest(
      d, y = "class", std = "scale", size.tr = 1,
      m = 50, PPmethod = "LDA", size.p = 1, parallel = FALSE
    ),
    predict = predict_ppforest
  ),
  xgboost = list(
    label = "XGBoost",
    fit = fit_xgboost,
    predict = predict_xgboost
  )
)

model_families <- c(
  multinomial = "linear",
  knn = "local",
  svm = "kernel",
  rpart = "tree",
  naive_bayes = "probabilistic",
  neural_network = "neural",
  random_forest = "ensemble",
  ppforest = "ensemble",
  xgboost = "ensemble"
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
  setNames(class_palette[seq_len(nlevels(data$class))], levels(data$class))
}

fade_colors <- function(colors, strength) {
  rgb <- grDevices::col2rgb(colors) / 255
  blended <- 1 - sweep(1 - rgb, 2, strength, "*")
  grDevices::rgb(blended[1, ], blended[2, ], blended[3, ])
}

draw_surface <- function(data, grid, prediction, view) {
  colors <- class_colors(data)
  classes <- names(colors)
  xs <- sort(unique(grid$x1))
  ys <- sort(unique(grid$x2))
  confidence <- prediction$confidence
  has_confidence <- all(is.finite(confidence))
  if (view == "confidence" && has_confidence) {
    chance <- 1 / length(classes)
    certainty <- pmax(0, pmin(1, (confidence - chance) / (1 - chance)))
    strength <- 0.06 + 0.7 * certainty
  } else {
    strength <- rep(0.22, nrow(grid))
  }
  cell_colors <- fade_colors(colors[prediction$class], strength)
  z <- matrix(seq_along(cell_colors), nrow = length(xs))
  par(mar = rep(0, 4))
  image(
    xs, ys, z,
    col = cell_colors,
    breaks = seq(0.5, length(cell_colors) + 0.5),
    axes = FALSE, xlab = "", ylab = "", useRaster = TRUE,
    frame.plot = FALSE
  )
  if (view == "confidence" && has_confidence) {
    contour(
      xs, ys, matrix(confidence, nrow = length(xs)),
      levels = c(0.5, 0.7, 0.9), add = TRUE, drawlabels = TRUE,
      col = grDevices::adjustcolor("#343a40", alpha.f = 0.5),
      lwd = 0.7, labcex = 0.55
    )
  }
  if (view == "confidence" && !has_confidence) {
    legend(
      "topright", "class only", bty = "n", text.col = "#6c757d", cex = 0.7
    )
  }
  points(
    data$x1, data$x2,
    pch = 21, cex = 0.62, lwd = 0.45,
    bg = colors[as.character(data$class)], col = "white"
  )
}

macro_f1 <- function(truth, prediction, classes) {
  mean(vapply(classes, function(class) {
    true_positive <- sum(truth == class & prediction == class)
    precision <- true_positive / max(1, sum(prediction == class))
    recall <- true_positive / max(1, sum(truth == class))
    if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
  }, numeric(1)))
}

draw_comparison <- function(scores, fit_time, predict_time, predictions, labels, classes) {
  score <- do.call(rbind, scores)
  old <- par(mfrow = c(2, 2), mar = c(6, 5, 2.5, 0.8), cex = 0.85)
  on.exit(par(old))

  model_colors <- hcl.colors(length(labels), "Dark 3")
  time_colors <- unname(class_palette[1:2])
  value_colors <- colorRampPalette(c("#f8f9fa", "#9ecae1", class_palette[2]))(12)
  xlim <- pmax(0, pmin(1, extendrange(score[, c("train", "test")], f = 0.12)))
  ylim <- pmax(0, pmin(1, extendrange(score[, c("train_f1", "test_f1")], f = 0.12)))
  plot(
    score[, "train"], score[, "train_f1"], type = "n", xlim = xlim, ylim = ylim,
    xlab = "Accuracy", ylab = "Macro-F1", main = "Train → test performance"
  )
  abline(0, 1, col = "#ced4da", lty = 3)
  segments(
    score[, "train"], score[, "train_f1"], score[, "test"], score[, "test_f1"],
    col = adjustcolor(model_colors, 0.55)
  )
  points(
    score[, "train"], score[, "train_f1"],
    pch = 1, lwd = 1.4, col = model_colors, cex = 1.2
  )
  points(
    score[, "test"], score[, "test_f1"],
    pch = 16, col = model_colors, cex = 1.2
  )
  label_y <- seq(
    ylim[1] + 0.05 * diff(ylim),
    ylim[2] - 0.05 * diff(ylim),
    length.out = nrow(score)
  )[rank(score[, "test_f1"], ties.method = "first")]
  segments(
    score[, "test"], score[, "test_f1"], score[, "test"], label_y,
    col = adjustcolor(model_colors, 0.45), lty = 3
  )
  text(
    score[, "test"], label_y, labels,
    pos = ifelse(score[, "test"] > mean(xlim), 2, 4),
    col = model_colors, cex = 0.65
  )
  legend(
    "bottomleft", c("Train", "Test"), pch = c(1, 16),
    col = "#343a40", pt.lwd = 1.4, bty = "n"
  )

  barplot(
    1000 * rbind(fit = fit_time, predict = predict_time),
    beside = TRUE, names.arg = labels, col = time_colors, las = 2,
    ylab = "Milliseconds", main = "Execution time"
  )
  legend("topleft", c("Fit", "Predict grid"), fill = time_colors, bty = "n")

  recall <- score[, paste0("recall_", classes), drop = FALSE]
  image(
    seq_len(nrow(recall)), seq_len(ncol(recall)), recall,
    col = value_colors, zlim = c(0, 1), axes = FALSE,
    xlab = "", ylab = "", main = "Test recall by class"
  )
  axis(1, seq_along(labels), labels, las = 2)
  axis(2, seq_along(classes), classes, las = 1)
  text(
    rep(seq_len(nrow(recall)), ncol(recall)),
    rep(seq_len(ncol(recall)), each = nrow(recall)),
    sprintf("%.0f", 100 * recall),
    col = ifelse(recall > 0.65, "white", "#343a40"), cex = 0.7
  )

  agreement <- vapply(predictions, function(reference) {
    vapply(predictions, function(prediction) mean(prediction == reference), numeric(1))
  }, numeric(length(predictions)))
  image(
    seq_along(labels), seq_along(labels), agreement,
    col = value_colors, zlim = c(0, 1), axes = FALSE,
    xlab = "", ylab = "", main = "Agreement on prediction grid"
  )
  axis(1, seq_along(labels), labels, las = 2)
  axis(2, seq_along(labels), labels, las = 1)
  text(
    rep(seq_along(labels), length(labels)),
    rep(seq_along(labels), each = length(labels)),
    sprintf("%.0f", 100 * agreement),
    col = ifelse(agreement > 0.65, "white", "#343a40"), cex = 0.55
  )
}

model_card <- function(id, model) {
  card(
    card_header(div(
      class = "d-flex justify-content-between align-items-center gap-2 w-100",
      tags$span(class = "fs-6 fw-semibold", model$label),
      uiOutput(
        paste0(id, "_order_value"), inline = TRUE,
        class = "ms-auto text-end"
      )
    )),
    plotOutput(paste0(id, "_plot"), height = "100%")
  )
}
cards <- Map(model_card, names(models), models)
model_grid <- function(order = names(models)) {
  do.call(layout_columns, c(unname(cards[order]), list(
    col_widths = 4,
    row_heights = rep("minmax(320px, 1fr)", ceiling(length(order) / 3)),
    gap = "0.5rem",
    height = "100%",
    fill = TRUE,
    fillable = TRUE
  )))
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  tags$head(tags$style(HTML("\
    .modal-dialog.modal-xl {\
      width: calc(100vw - 2rem);\
      max-width: calc(100vw - 2rem);\
      margin: 1rem auto;\
    }\
    .modal-dialog.modal-xl .modal-content {\
      height: calc(100vh - 2rem);\
    }\
    .modal-dialog.modal-xl .modal-body {\
      display: flex;\
      min-height: 0;\
      padding: .75rem 1rem;\
    }\
    .modal-dialog.modal-xl #comparison_plot {\
      flex: 1 1 auto;\
      height: 100% !important;\
      min-height: 0;\
    }\
  "))),
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Multiclass Decision Regions",
      selectInput(
        "scenario", tags$small("Dataset"),
        setNames(names(dataset_labels), dataset_labels)
      ),
      conditionalPanel(
        "input.scenario != 'iris' && input.scenario != 'penguins'",
        sliderInput("n", tags$small("Observations"), 100, 1000, 300, step = 100)
      ),
      radioButtons(
        "surface_view", tags$small("Prediction display"),
        c("Class + confidence" = "confidence", "Predicted class" = "class")
      ),
      selectInput(
        "card_order", tags$small("Order cards"),
        c(
          "Test accuracy (desc)" = "test_accuracy_desc",
          "Test accuracy (asc)" = "test_accuracy_asc",
          "Test macro-F1 (desc)" = "test_f1_desc",
          "Test macro-F1 (asc)" = "test_f1_asc",
          "Time (asc)" = "time_asc",
          "Time (desc)" = "time_desc",
          "Model family" = "family"
        )
      ),
      actionButton("resample", "New sample", class = "btn-primary w-100"),
      actionButton("compare", "Compare results", class = "btn-outline-primary w-100"),
      uiOutput("class_legend"),
      uiOutput("axis_labels"),
      uiOutput("surface_note"),
      accordion(open = FALSE, accordion_panel(
        "How it works", tags$small(htmltools::includeMarkdown("readme.md"))
      )),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    uiOutput("model_grid", fill = TRUE)
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  seed <- reactiveVal(2026L)
  observeEvent(input$resample, seed(seed() + 1L))
  observe({
    label <- if (input$scenario %in% c("iris", "penguins")) {
      "New split"
    } else {
      "New sample"
    }
    updateActionButton(session, "resample", label = label)
  })
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
          prediction <- model$predict(fit$model, grid)
        })[["elapsed"]]
        list(prediction = prediction, time = elapsed)
      },
      models, fits()
    )
    list(grid = grid, results = results)
  })

  scores <- reactive({
    d <- data()
    truth <- as.character(d$class)
    train <- d$partition == "train"
    test <- d$partition == "test"
    newdata <- d[c("x1", "x2")]
    predictions <- Map(
      function(model, fit) model$predict(fit$model, newdata)$class,
      models, fits()
    )
    classes <- levels(d$class)
    lapply(predictions, function(prediction) {
      correct <- prediction == truth
      recall <- vapply(classes, function(class) {
        test_class <- test & truth == class
        mean(correct[test_class])
      }, numeric(1))
      c(
        train = mean(correct[train]),
        test = mean(correct[test]),
        train_f1 = macro_f1(truth[train], prediction[train], classes),
        test_f1 = macro_f1(truth[test], prediction[test], classes),
        setNames(recall, paste0("recall_", classes))
      )
    })
  })

  order_values <- reactive({
    ids <- names(models)
    criterion <- sub("_(asc|desc)$", "", input$card_order)
    switch(
      criterion,
      family = setNames(match(model_families, unique(model_families)), ids),
      test_accuracy = vapply(scores(), \(x) x[["test"]], numeric(1)),
      test_f1 = vapply(scores(), \(x) x[["test_f1"]], numeric(1)),
      time = vapply(fits(), \(x) x$time, numeric(1)) +
          vapply(surfaces()$results, \(x) x$time, numeric(1))
    )
  })

  card_order <- reactive({
    ids <- names(models)
    value <- order_values()
    descending <- grepl("_desc$", input$card_order)
    index <- if (descending) {
      order(-value, seq_along(value))
    } else {
      order(value, seq_along(value))
    }
    ids[index]
  })

  order_label <- reactive({
    criterion <- sub("_(asc|desc)$", "", input$card_order)
    if (identical(criterion, "family")) {
      return(model_families)
    }
    value <- order_values()
    label <- sprintf("%.3f", value)
    setNames(label, names(value))
  })

  output$model_grid <- renderUI(model_grid(card_order()))

  observeEvent(input$compare, showModal(modalDialog(
    title = "Model comparison",
    plotOutput("comparison_plot", height = "100%"),
    size = "xl", easyClose = TRUE, footer = modalButton("Close")
  )))

  output$comparison_plot <- renderPlot({
    draw_comparison(
      scores(),
      vapply(fits(), \(x) x$time, numeric(1)),
      vapply(surfaces()$results, \(x) x$time, numeric(1)),
      lapply(surfaces()$results, \(x) x$prediction$class),
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
  output$axis_labels <- renderUI({
    labels <- attr(data(), "axis_labels")
    tags$small(sprintf("Axes: %s × %s", labels[1], labels[2]))
  })
  output$surface_note <- renderUI({
    if (input$surface_view == "class") {
      tags$small("The background shows the class selected by each model.")
    } else {
      tags$small(
        "Fainter color means a lower winning score. Contours mark 50%, 70%, and 90%."
      )
    }
  })
  invisible(lapply(names(models), function(id) {
    output[[paste0(id, "_order_value")]] <- renderUI({
      tags$span(class = "fw-normal text-muted", order_label()[[id]])
    })
    output[[paste0(id, "_plot")]] <- renderPlot({
      surface <- surfaces()
      draw_surface(
        data(), surface$grid, surface$results[[id]]$prediction,
        input$surface_view
      )
    }, res = 110)
  }))
}
shinyApp(ui, server)
