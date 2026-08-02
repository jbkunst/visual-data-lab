# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(markdown)

# theme -------------------------------------------------------------------
apptheme <- bs_theme()
sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))
primary <- unname(bs_get_variables(apptheme, "primary"))

# app options -------------------------------------------------------------
N_POINTS <- 256
MAX_COMPONENTS <- 60

resample_path <- function(x, y, n = N_POINTS) {
  x <- c(x, x[1])
  y <- c(y, y[1])
  distance <- c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))
  target <- seq(0, max(distance), length.out = n + 1)[-(n + 1)]
  data.frame(
    x = approx(distance, x, target, ties = "ordered")$y,
    y = approx(distance, y, target, ties = "ordered")$y
  )
}

shape_path <- function(shape, n = N_POINTS) {
  path <- switch(
    shape,
    circle = {
      angle <- seq(0, 2 * pi, length.out = 1001)[-1001]
      resample_path(cos(angle), sin(angle), n)
    },
    square = resample_path(c(-1, 1, 1, -1), c(-1, -1, 1, 1), n),
    star = {
      angle <- pi / 2 + seq(0, 2 * pi, length.out = 11)[-11]
      radius <- rep(c(1, 0.42), 5)
      resample_path(radius * cos(angle), radius * sin(angle), n)
    },
    heart = {
      angle <- seq(0, 2 * pi, length.out = 1001)[-1001]
      resample_path(
        16 * sin(angle)^3,
        13 * cos(angle) - 5 * cos(2 * angle) -
          2 * cos(3 * angle) - cos(4 * angle),
        n
      )
    }
  )

  path$x <- path$x - mean(path$x)
  path$y <- path$y - mean(path$y)
  scale <- max(sqrt(path$x^2 + path$y^2))
  transform(path, x = x / scale, y = y / scale)
}

fourier_components <- function(path) {
  z <- complex(real = path$x, imaginary = path$y)
  coefficient <- fft(z) / length(z)
  index <- seq_along(coefficient) - 1
  data.frame(
    frequency = ifelse(index <= length(z) / 2, index, index - length(z)),
    amplitude = Mod(coefficient),
    phase = Arg(coefficient),
    real = Re(coefficient),
    imaginary = Im(coefficient),
    energy = Mod(coefficient)^2
  )
}

reconstruct_path <- function(components, n = N_POINTS) {
  time <- 2 * pi * (0:(n - 1)) / n
  coefficient <- complex(real = components$real, imaginary = components$imaginary)
  z <- exp(1i * outer(time, components$frequency)) %*% coefficient
  data.frame(x = Re(z), y = Im(z))
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  tags$head(
    tags$script(src = "epicycles.js"),
    tags$style(HTML(
      ".epicycle-stage {height: 100%; min-height: 480px; background: var(--bs-body-bg);}
       #epicycle-canvas {width: 100%; height: 100%; display: block;}"
    ))
  ),
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Fourier Epicycles",
      withMathJax(),
      selectInput(
        "shape",
        tags$small("Shape"),
        c("Heart" = "heart", "Star" = "star", "Square" = "square", "Circle" = "circle")
      ),
      sliderInput("components", tags$small("Number of circles"), 1, MAX_COMPONENTS, 15, 1),
      radioButtons(
        "order",
        tags$small("Circle order"),
        c("Amplitude" = "amplitude", "Frequency" = "frequency"),
        "amplitude",
        inline = TRUE
      ),
      sliderInput("speed", tags$small("Animation speed"), 0.25, 2, 1, 0.25),
      checkboxInput("playing", tags$small("Play animation"), TRUE),
      actionButton("restart", "Restart", width = "100%"),
      tags$hr(),
      checkboxInput("show_circles", tags$small("Show circles"), TRUE),
      checkboxInput("show_vectors", tags$small("Show vectors"), TRUE),
      checkboxInput("show_original", tags$small("Show original path"), TRUE),
      checkboxInput("show_trace", tags$small("Show reconstructed trace"), TRUE),
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
      col_widths = c(8, 4),
      card(
        card_header("Rotating Fourier vectors"),
        card_body(
          class = "p-0",
          tags$div(
            class = "epicycle-stage",
            tags$canvas(id = "epicycle-canvas", `aria-label` = "Animated Fourier epicycles")
          )
        )
      ),
      card(
        card_header(uiOutput("reconstruction_header")),
        card_body(plotOutput("reconstruction", height = "100%"))
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  path <- reactive(shape_path(input$shape))
  components <- reactive(fourier_components(path()))

  selected <- reactive({
    x <- components()[order(-components()$amplitude), ]
    x <- head(x, input$components)
    if (input$order == "frequency") x <- x[order(abs(x$frequency), x$frequency), ]
    x
  })

  reconstruction <- reactive(reconstruct_path(selected(), nrow(path())))

  quality <- reactive({
    ordered <- components()[order(-components()$amplitude), ]
    total <- sum(ordered$energy)
    list(
      recovered = 100 * sum(selected()$energy) / total,
      next_gain = 100 * ordered$energy[input$components + 1] / total
    )
  })

  send_data <- function() {
    x <- selected()
    original <- path()
    session$sendCustomMessage(
      "epicycles-data",
      list(
        components = lapply(seq_len(nrow(x)), \(i) list(
          frequency = x$frequency[i],
          amplitude = x$amplitude[i],
          phase = x$phase[i]
        )),
        path = lapply(seq_len(nrow(original)), \(i) list(
          x = original$x[i],
          y = original$y[i]
        ))
      )
    )
  }

  send_options <- function() {
    session$sendCustomMessage(
      "epicycles-options",
      list(
        speed = input$speed,
        playing = input$playing,
        showCircles = input$show_circles,
        showVectors = input$show_vectors,
        showOriginal = input$show_original,
        showTrace = input$show_trace
      )
    )
  }

  session$onFlushed(function() {
    send_data()
    send_options()
  }, once = TRUE)

  observeEvent(
    list(input$shape, input$components, input$order),
    send_data(),
    ignoreInit = TRUE
  )

  observeEvent(
    list(
      input$speed,
      input$playing,
      input$show_circles,
      input$show_vectors,
      input$show_original,
      input$show_trace
    ),
    send_options(),
    ignoreInit = TRUE
  )

  observeEvent(input$restart, {
    session$sendCustomMessage("epicycles-command", list(command = "restart"))
  })

  output$reconstruction_header <- renderUI({
    q <- quality()
    tags$div(
      class = "d-flex justify-content-between w-100",
      tags$span("Reconstruction"),
      tags$small(
        class = "text-muted",
        sprintf(
          "%d circles · %.1f%% recovered · next +%.2f pp",
          input$components,
          q$recovered,
          q$next_gain
        )
      )
    )
  })

  output$reconstruction <- renderPlot({
    original <- path()
    estimate <- reconstruction()
    par(mar = c(1, 1, 1, 1), bg = "transparent")
    plot(
      original$x,
      original$y,
      type = "l",
      asp = 1,
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = c(-1.1, 1.1),
      ylim = c(-1.1, 1.1),
      col = "grey75",
      lwd = 3
    )
    lines(estimate$x, estimate$y, col = primary, lwd = 2)
    legend(
      "bottom",
      c("Original", "Reconstructed"),
      col = c("grey75", primary),
      lwd = c(3, 2),
      bty = "n",
      horiz = TRUE,
      cex = 0.8
    )
  }, res = 110)
}

shinyApp(ui, server)
