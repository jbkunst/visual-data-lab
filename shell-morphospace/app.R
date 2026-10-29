# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(plotly)
library(vdltheme)

# theme -------------------------------------------------------------------
apptheme <- theme_vdl()
shell_color <- "#49A89D"

sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

# shell geometry ----------------------------------------------------------
# Circular aperture in the radial/vertical plane, swept along Raup's spiral.
# The final aperture has radius 1; four turns show the same growth interval.
generate_shell <- function(w, d, t) {
  theta <- seq(-8 * pi, 0, length.out = 361)
  phi <- seq(0, 2 * pi, length.out = 73)
  radius <- w^(theta / (2 * pi))
  offset <- (1 + d) / (1 - d)
  radial <- outer(radius, offset + cos(phi))

  list(
    x = radial * cos(theta),
    y = radial * sin(theta),
    z = outer(radius, 2 * t / (1 - d) + sin(phi))
  )
}

presets <- list(
  Nautilus = c(w = 3, d = 0.05, t = 0),
  Snail = c(w = 2.5, d = 0.1, t = 0.7),
  Turritella = c(w = 1.5, d = 0.05, t = 1.8),
  Horn = c(w = 8, d = 0.65, t = 2.5),
  `Clam-like` = c(w = 80, d = 0, t = 0),
  `Open coil` = c(w = 1.5, d = 0.65, t = 0.15),
  `Ammonite-like` = c(w = 2, d = 0.3, t = 0),
  `Limpet-like` = c(w = 100, d = 0, t = 0.1),
  `Ram horn-like` = c(w = 5, d = 0.5, t = 0.2),
  `Tusk / beak-like` = c(w = 100, d = 0.7, t = 1.5)
)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Shell morphospace",
      selectInput("preset", "Start with a shape", choices = names(presets)),
      sliderInput(
        "w",
        input_label_vdl("Expansion \\(W\\)", "Aperture size multiplies by W each turn. Higher values create a rapidly flaring shell."),
        min = 1.5, max = 100, value = 3, step = 0.5, ticks = FALSE
      ),
      sliderInput(
        "d",
        input_label_vdl("Distance from axis \\(D\\)", "Moves the aperture away from the coiling axis. Higher values open a gap between the whorls."),
        min = 0, max = 0.85, value = 0.05, step = 0.05, ticks = FALSE
      ),
      sliderInput(
        "t",
        input_label_vdl("Translation \\(T\\)", "Moves the growing aperture along the vertical axis. Zero gives a flat coil; higher values create a spire."),
        min = 0, max = 3, value = 0, step = 0.05, ticks = FALSE
      ),
      tags$small(class = "text-muted", "Drag to rotate. Scroll to zoom. Projections show the surface as a wireframe."),
      accordion(
        open = FALSE,
        accordion_panel("How it works", tags$small(htmltools::includeMarkdown("readme.md"))),
        accordion_panel(
          "Parametric surface",
          withMathJax(tags$small(
            tags$p("A circular opening grows as it follows a spiral."),
            "\\[r = W^{\\theta/(2\\pi)},\\quad A = \\frac{1+D}{1-D}\\]",
            "\\[\\begin{aligned}x &= r(A+\\cos\\varphi)\\cos\\theta\\\\y &= r(A+\\cos\\varphi)\\sin\\theta\\\\z &= r\\left(\\frac{2T}{1-D}+\\sin\\varphi\\right)\\end{aligned}\\]",
            tags$p("The angle \\(\\theta\\) follows four spiral turns, from \\(-8\\pi\\) to \\(0\\). The angle \\(\\varphi\\) runs around the opening, from \\(0\\) to \\(2\\pi\\).")
          ))
        ),
        accordion_panel("Inspiration and resources", tags$small(htmltools::includeMarkdown("resources.md")))
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    layout_columns(
      col_widths = c(12, 4, 4, 4),
      row_heights = c(3, 2),
      gap = "0.75rem",
      card(card_header("3D form"), plotlyOutput("shell3d", height = "100%")),
      card(card_header("X-Y projection"), plotOutput("xy", height = "100%")),
      card(card_header("X-Z projection"), plotOutput("xz", height = "100%")),
      card(card_header("Y-Z projection"), plotOutput("yz", height = "100%"))
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  observeEvent(input$preset, {
    values <- presets[[input$preset]]
    updateSliderInput(session, "w", value = values[["w"]])
    updateSliderInput(session, "d", value = values[["d"]])
    updateSliderInput(session, "t", value = values[["t"]])
  }, ignoreInit = TRUE)

  shell <- reactive({
    generate_shell(input$w, input$d, input$t)
  })

  output$shell3d <- renderPlotly({
    s <- isolate(shell())
    # Explicit triangles keep the coiled surface's connectivity in 3D.
    vertices <- matrix(seq_along(s$x) - 1L, nrow = nrow(s$x))
    a <- as.vector(vertices[-nrow(vertices), -ncol(vertices)])
    b <- as.vector(vertices[-1, -ncol(vertices)])
    c <- as.vector(vertices[-nrow(vertices), -1])
    d <- as.vector(vertices[-1, -1])
    plot_ly(
      x = as.vector(s$x), y = as.vector(s$y), z = as.vector(s$z), type = "mesh3d",
      i = c(a, b), j = c(b, d), k = c(c, c),
      color = I(shell_color),
      showscale = FALSE,
      hoverinfo = "skip",
      flatshading = FALSE,
      lighting = list(ambient = 0.25, diffuse = 0.95, specular = 0.45, roughness = 0.45),
      lightposition = list(x = -100, y = 200, z = 150)
    ) |>
      layout(
        margin = list(l = 0, r = 0, t = 0, b = 0),
        uirevision = "shell-camera",
        scene = list(
          uirevision = "shell-camera",
          aspectmode = "data",
          xaxis = list(title = "X", zeroline = FALSE),
          yaxis = list(title = "Y", zeroline = FALSE),
          zaxis = list(title = "Z", zeroline = FALSE),
          camera = list(eye = list(x = 1, y = 2.5, z = 0.6))
        )
      ) |>
      config(displaylogo = FALSE)
  })

  observeEvent(shell(), {
    s <- shell()
    plotlyProxy("shell3d", session) |>
      plotlyProxyInvoke(
        "restyle",
        list(x = list(as.vector(s$x)), y = list(as.vector(s$y)), z = list(as.vector(s$z))),
        list(0)
      )
  }, ignoreInit = TRUE)

  projection <- function(horizontal, vertical) {
    s <- shell()
    x <- s[[horizontal]]
    y <- s[[vertical]]
    # All projections use the same units and span, preserving proportions.
    span <- max(vapply(s[c("x", "y", "z")], function(a) diff(range(a)), numeric(1))) * 1.1
    xlim <- mean(range(x)) + c(-0.5, 0.5) * span
    ylim <- mean(range(y)) + c(-0.5, 0.5) * span
    par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.7, 0.5, 0), tcl = -0.2)
    plot(NA, xlim = xlim, ylim = ylim, asp = 1,
         xlab = toupper(horizontal), ylab = toupper(vertical), bty = "n")
    for (j in seq(1, ncol(x) - 1, by = 4)) {
      lines(x[, j], y[, j], col = grDevices::adjustcolor(shell_color, alpha.f = 0.55), lwd = 0.7)
    }
    for (i in seq(1, nrow(x), by = 9)) {
      lines(x[i, ], y[i, ], col = grDevices::adjustcolor(shell_color, alpha.f = 0.65), lwd = 0.7)
    }
    lines(x[nrow(x), ], y[nrow(y), ], col = "#23645D", lwd = 2)
  }

  output$xy <- renderPlot(projection("x", "y"))
  output$xz <- renderPlot(projection("x", "z"))
  output$yz <- renderPlot(projection("y", "z"))
}

shinyApp(ui, server)
