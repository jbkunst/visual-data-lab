# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(rpart)
library(vdltheme)

# theme and data -----------------------------------------------------------
apptheme <- theme_vdl()
sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(
  bslib::card,
  full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 0)
)
thematic::thematic_shiny(font = "auto")

app_dir <- if (file.exists("sensor_readings_2.data")) "." else "projection-pursuit-trees"
robot <- read.csv(
  file.path(app_dir, "sensor_readings_2.data"),
  header = FALSE,
  col.names = c("x1", "x2", "class")
)
robot$class <- factor(c(
  "Move-Forward" = "Forward",
  "Sharp-Right-Turn" = "Sharp right",
  "Slight-Left-Turn" = "Slight left",
  "Slight-Right-Turn" = "Slight right"
)[robot$class])

# helpers -----------------------------------------------------------------
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
      data.frame(x1, x2, class = factor(class))
    },
    robot[sample.int(nrow(robot), min(n, nrow(robot))), ]
  )

  train <- unlist(lapply(
    split(seq_len(nrow(data)), data$class),
    function(i) sample(i, max(1, floor(0.7 * length(i))))
  ))
  data$partition <- ifelse(seq_len(nrow(data)) %in% train, "train", "test")
  data$class <- droplevels(data$class)
  data
}

timed_fit <- function(code) {
  start <- proc.time()[[3]]
  model <- force(code)
  list(model = model, seconds = proc.time()[[3]] - start)
}

predict_class <- function(fit, method, newdata) {
  switch(
    method,
    rpart = as.character(predict(fit, newdata, type = "class")),
    pptree = as.character(predict(fit, newdata, Rule = 1)),
    ext = as.character(predict(fit, newdata)$predict.class)
  )
}

pp_depths <- function(tree) {
  depths <- rep(NA_integer_, nrow(tree))
  visit <- function(id, depth) {
    row <- match(id, tree[, 1])
    depths[[row]] <<- depth
    if (tree[row, 4] != 0) {
      visit(tree[row, 2], depth + 1L)
      visit(tree[row, 3], depth + 1L)
    }
  }
  visit(1, 0L)
  depths
}

pp_cuts <- function(fit, method) {
  tree <- fit$Tree.Struct
  rows <- which(tree[, 4] != 0)
  coef_id <- tree[rows, 4]
  cutoff <- if (method == "pptree") {
    fit$splitCutoff.node[coef_id, 1]
  } else {
    as.numeric(fit$splitCutoff.node)[coef_id]
  }
  cuts <- data.frame(
    step = coef_id,
    node = tree[rows, 1],
    depth = pp_depths(tree)[rows],
    a = fit$projbest.node[coef_id, 1],
    b = fit$projbest.node[coef_id, 2],
    cutoff = cutoff
  )
  cuts[order(cuts$step), ]
}

decision_grid <- function(data, length = 90) {
  x_range <- range(data$x1)
  y_range <- range(data$x2)
  x_pad <- max(diff(x_range) * 0.06, 0.05)
  y_pad <- max(diff(y_range) * 0.06, 0.05)
  expand.grid(
    x1 = seq(x_range[1] - x_pad, x_range[2] + x_pad, length.out = length),
    x2 = seq(y_range[1] - y_pad, y_range[2] + y_pad, length.out = length)
  )
}

draw_cut <- function(cut, color, width = 1) {
  if (abs(cut$b) > 1e-9) {
    abline(a = cut$cutoff / cut$b, b = -cut$a / cut$b, col = color, lwd = width)
  } else {
    abline(v = cut$cutoff / cut$a, col = color, lwd = width)
  }
}

draw_surface <- function(data, grid, prediction, title, cuts = NULL, step = Inf) {
  classes <- levels(data$class)
  palette <- setNames(grDevices::hcl.colors(length(classes), "Dark 3"), classes)
  xs <- sort(unique(grid$x1))
  ys <- sort(unique(grid$x2))
  z <- matrix(match(prediction, classes), nrow = length(xs))

  image(
    xs, ys, z,
    col = grDevices::adjustcolor(palette, alpha.f = 0.18),
    breaks = seq(0.5, length(classes) + 0.5),
    xlab = "x1", ylab = "x2", main = title, asp = 1
  )

  if (!is.null(cuts)) {
    visible <- cuts[cuts$step <= step, , drop = FALSE]
    if (nrow(visible)) {
      for (i in seq_len(nrow(visible))) draw_cut(visible[i, ], "#6c757d", 1)
      draw_cut(visible[nrow(visible), ], "#111315", 2.5)
    }
  }

  train <- data$partition == "train"
  points(data$x1[train], data$x2[train], pch = 21, cex = 0.75,
    bg = palette[as.character(data$class[train])], col = "white")
  points(data$x1[!train], data$x2[!train], pch = 1, cex = 0.85,
    col = palette[as.character(data$class[!train])], lwd = 1.3)
  legend("topright", legend = classes, pch = 21, pt.bg = palette,
    col = "white", bty = "n", cex = 0.75)
  box(col = "#ced4da")
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Projection Pursuit Trees",
      selectInput(
        "scenario", tags$small("Dataset"),
        c("Two islands" = "islands", "Diagonal bands" = "diagonal",
          "Robot navigation · UCI" = "robot")
      ),
      sliderInput("n", tags$small("Observations"), 150, 900, 450, step = 50),
      actionButton("resample", "New sample", class = "btn-primary w-100"),
      sliderInput("depth", tags$small("rpart · maximum depth"), 1, 8, 4),
      sliderInput(
        "tol", input_label_vdl(
          "PPtreeExt · entropy tolerance",
          "Lower values allow more splits; higher values stop earlier."
        ),
        0.05, 0.8, 0.2, step = 0.05
      ),
      selectInput(
        "focus", tags$small("Reveal cuts for"),
        c("PPtree" = "pptree", "PPtreeExt" = "ext")
      ),
      sliderInput(
        "step", tags$small("Successive cuts"), 0, 1, 1,
        animate = animationOptions(interval = 900, loop = FALSE)
      ),
      tags$small(textOutput("cut_note")),
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
      col_widths = c(6, 6, 6, 6),
      gap = "0.75rem",
      card(card_header("rpart · rectangular"), plotOutput("rpart_plot", height = "100%")),
      card(card_header("PPtree · oblique"), plotOutput("pptree_plot", height = "100%")),
      card(card_header("PPtreeExt · multiple oblique regions"), plotOutput("ext_plot", height = "100%")),
      card(
        card_header("Performance"),
        card_body(
          tableOutput("metrics"),
          tags$small("Filled points are train; outlined points are test. Timing is one fit, not a benchmark.")
        )
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  seed <- reactiveVal(2026L)
  observeEvent(input$resample, seed(seed() + 1L))

  data <- reactive(make_data(input$scenario, input$n, seed()))

  fits <- reactive({
    train <- data()[data()$partition == "train", c("x1", "x2", "class")]
    list(
      rpart = timed_fit(rpart(
        class ~ x1 + x2, train, method = "class",
        control = rpart.control(cp = 0, maxdepth = input$depth, minsplit = 12)
      )),
      pptree = timed_fit(PPtreeViz::PPTreeclass(class ~ x1 + x2, train, PPmethod = "LDA")),
      ext = timed_fit(PPtreeExt::PPtreeExtclass(
        class ~ x1 + x2, train, PPmethod = "LDA",
        srule = TRUE, tot = nrow(train), tol = input$tol
      ))
    )
  })

  cuts <- reactive(lapply(c("pptree", "ext"), function(x) {
    pp_cuts(fits()[[x]]$model, x)
  }) |> setNames(c("pptree", "ext")))

  observeEvent(list(fits(), input$focus), {
    total <- nrow(cuts()[[input$focus]])
    updateSliderInput(session, "step", max = max(1, total), value = total)
  })

  surfaces <- reactive({
    grid <- decision_grid(data())
    predictions <- lapply(names(fits()), function(method) {
      predict_class(fits()[[method]]$model, method, grid)
    }) |> setNames(names(fits()))
    list(grid = grid, predictions = predictions)
  })

  plot_method <- function(method, label) {
    surface <- surfaces()
    model_cuts <- if (method == "rpart") NULL else cuts()[[method]]
    shown <- if (identical(method, input$focus)) input$step else Inf
    draw_surface(
      data(), surface$grid, surface$predictions[[method]], label,
      model_cuts, shown
    )
  }

  output$rpart_plot <- renderPlot(plot_method("rpart", "Axis-aligned cuts"), res = 110)
  output$pptree_plot <- renderPlot(plot_method("pptree", "At most G - 1 cuts"), res = 110)
  output$ext_plot <- renderPlot(plot_method("ext", "A class may occupy several leaves"), res = 110)

  output$cut_note <- renderText({
    selected <- cuts()[[input$focus]]
    if (!nrow(selected) || input$step == 0) return("Start before the first split.")
    cut <- selected[min(input$step, nrow(selected)), ]
    sprintf(
      "Cut %d · depth %d: %.2f x1 %+.2f x2 = %.2f",
      cut$step, cut$depth, cut$a, cut$b, cut$cutoff
    )
  })

  output$metrics <- renderTable({
    d <- data()
    methods <- names(fits())
    labels <- c(rpart = "rpart", pptree = "PPtree", ext = "PPtreeExt")
    score <- function(method, partition) {
      rows <- d$partition == partition
      newdata <- d[rows, c("x1", "x2")]
      mean(predict_class(fits()[[method]]$model, method, newdata) == d$class[rows])
    }
    depths <- c(
      rpart = max(floor(log2(as.integer(row.names(fits()$rpart$model$frame))))),
      pptree = max(pp_depths(fits()$pptree$model$Tree.Struct)),
      ext = max(pp_depths(fits()$ext$model$Tree.Struct))
    )
    splits <- c(
      rpart = sum(fits()$rpart$model$frame$var != "<leaf>"),
      pptree = nrow(cuts()$pptree),
      ext = nrow(cuts()$ext)
    )
    data.frame(
      Model = labels[methods],
      Train = vapply(methods, score, numeric(1), "train"),
      Test = vapply(methods, score, numeric(1), "test"),
      Seconds = vapply(fits(), `[[`, numeric(1), "seconds"),
      Splits = splits[methods],
      Depth = depths[methods],
      check.names = FALSE
    )
  }, digits = 3, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui, server)
