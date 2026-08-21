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
      x1 = iris$Petal.Length,
      x2 = iris$Petal.Width,
      class = iris$Species
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

# model helpers ------------------------------------------------------------
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

models <- list(
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
  knn = list(
    label = "KNN", subtitle = "local neighbourhoods",
    fit = fit_knn,
    predict = predict_knn
  )
)

# plot helpers -------------------------------------------------------------
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
    plotOutput(paste0(id, "_plot"), height = "100%")
  )
}

cards <- Map(model_card, names(models), models)
model_grid <- do.call(
  layout_columns,
  c(cards, list(col_widths = 4, gap = "0.75rem"))
)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
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
      uiOutput("class_legend"),
      tags$small("The background is the predicted class, not a probability surface."),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
        )
      ),
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
    lapply(models, function(model) model$fit(train))
  })

  surfaces <- reactive({
    grid <- decision_grid(data())
    predictions <- Map(
      function(model, fit) as.character(model$predict(fit, grid)),
      models, fits()
    )
    list(grid = grid, predictions = predictions)
  })

  scores <- reactive({
    d <- data()
    predictions <- Map(
      function(model, fit) as.character(model$predict(fit, d)),
      models, fits()
    )

    lapply(predictions, function(prediction) {
      correct <- prediction == as.character(d$class)
      c(
        train = mean(correct[d$partition == "train"]),
        test = mean(correct[d$partition == "test"])
      )
    })
  })

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
      draw_surface(data(), surface$grid, surface$predictions[[id]])
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
