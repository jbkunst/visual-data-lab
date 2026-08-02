# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggsketch)
library(klassets)

# theme -------------------------------------------------------------------
apptheme <- bs_theme(
  bg = "#efece4",
  fg = "#25313b",
  primary = "#d46a4c"
)

options(
  ggsketch.seed = 42L,
  ggsketch.base_family = "auto"
)

ink <- "#25313b"
accent <- "#d46a4c"
blue <- "#466c8b"
green <- "#70845c"
purple <- "#86648b"
paper <- "#fbf7ed"

# data --------------------------------------------------------------------
set.seed(123)

original <- klassets::sim_quasianscombe_set_1(
  n = 120,
  beta1 = 1.25,
  error_dist = purrr::partial(rnorm, sd = 0.65)
)

nonlinear <- klassets::sim_quasianscombe_set_2(
  original,
  fun = function(x) 1.8 * x^2,
  residual_factor = 0.15
)

outliers <- klassets::sim_quasianscombe_set_3(
  original,
  prop = 0.08,
  residual_factor = 0.10
)

clusters <- klassets::sim_quasianscombe_set_4(
  original,
  rescale_to = c(0.05, 0.18),
  prop = 0.35
)

heteroskedastic <- klassets::sim_quasianscombe_set_5(
  original,
  fun = function(x) x^1.6,
  residual_factor = 2
)

simpson <- klassets::sim_quasianscombe_set_6(
  original,
  groups = 3,
  b1_factor = -0.7,
  residual_factor = 0.20
)

set.seed(99)
clusters$group <- factor(
  stats::kmeans(scale(clusters[c("x", "y")]), centers = 2)$cluster,
  labels = c("Group A", "Group B")
)

simpson$group <- factor(
  sort(rep(seq_len(3), length.out = nrow(simpson))),
  labels = c("Group A", "Group B", "Group C")
)

outlier_model <- lm(y ~ x, data = outliers)
outliers$influential <- factor(
  rank(-stats::cooks.distance(outlier_model)) <= 4,
  levels = c(FALSE, TRUE),
  labels = c("Other observations", "Influential observations")
)

scene_stats <- function(data) {
  model <- lm(y ~ x, data = data)

  list(
    n = nrow(data),
    mean_x = mean(data$x),
    mean_y = mean(data$y),
    intercept = unname(coef(model)[1]),
    slope = unname(coef(model)[2]),
    correlation = cor(data$x, data$y),
    r_squared = summary(model)$r.squared
  )
}

scenes <- list(
  list(
    id = "linear",
    short = "Expected",
    title = "The relationship we expected",
    question = "What shape do these summaries suggest?",
    setup = "A positive slope and a stable average relationship usually make us imagine a simple cloud around a straight line.",
    reveal_title = "Sometimes the summary works well",
    explanation = "The points form one continuous cloud and the fitted line is a useful description of the relationship.",
    takeaway = "This comfortable picture is only one possibility behind a fitted line.",
    plot_note = "One cloud, one useful summary",
    data = original
  ),
  list(
    id = "nonlinear",
    short = "Curved",
    title = "A straight line can hide a curve",
    question = "Would you expect a curved relationship?",
    setup = "The fitted intercept and slope remain almost unchanged, so the numerical result still sounds linear.",
    reveal_title = "The line misses the shape",
    explanation = "The observations follow a strong curved pattern. The straight line compresses that structure into one average direction.",
    takeaway = "A linear summary can miss a strong nonlinear relationship.",
    plot_note = "The blue curve follows what the line misses",
    data = nonlinear
  ),
  list(
    id = "outliers",
    short = "Outliers",
    title = "A few points can hold up the result",
    question = "How many observations create the slope?",
    setup = "The regression equation looks reassuringly similar to the first chapter, but it does not show how evenly the evidence is distributed.",
    reveal_title = "The result depends on a handful of points",
    explanation = "Most observations show a weaker relationship. A few influential values pull the fitted line toward the original slope.",
    takeaway = "A coefficient can look stable while being structurally fragile.",
    plot_note = "These points have unusual influence",
    data = outliers
  ),
  list(
    id = "clusters",
    short = "Clusters",
    title = "One line can mix different populations",
    question = "Are these observations one population?",
    setup = "Means and regression coefficients do not reveal whether the observations form one cloud or several separated groups.",
    reveal_title = "The global line runs between groups",
    explanation = "The fitted relationship partly reflects the distance between two clusters. It may not describe the relationship followed inside either cluster.",
    takeaway = "Look for subpopulations before interpreting a global trend.",
    plot_note = "The groups create part of the global trend",
    data = clusters
  ),
  list(
    id = "heteroskedastic",
    short = "Unequal error",
    title = "The average can stay stable while uncertainty changes",
    question = "Is the line equally reliable everywhere?",
    setup = "The fitted line describes the conditional average, but its equation says nothing about how observations spread around it.",
    reveal_title = "The error grows across the range",
    explanation = "The cloud opens like a fan. Predictions at larger values of x are less precise even though the average line is nearly unchanged.",
    takeaway = "A model is not only its fitted mean; the residual pattern matters too.",
    plot_note = "Same average direction, different uncertainty",
    data = heteroskedastic
  ),
  list(
    id = "simpson",
    short = "Simpson",
    title = "A hidden variable can reverse the story",
    question = "Does the global slope describe each group?",
    setup = "With every point treated as one population, the fitted line suggests a positive relationship.",
    reveal_title = "Within each group, the direction changes",
    explanation = "Colour reveals a third variable. The groups occupy different regions, and their internal relationships disagree with the aggregated trend.",
    takeaway = "Aggregation can create a result that no individual group follows.",
    plot_note = "Global trend versus within-group trends",
    data = simpson
  )
)

scenes <- lapply(scenes, function(scene) {
  scene$stats <- scene_stats(scene$data)
  scene
})

# helpers -----------------------------------------------------------------
fmt <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

metric_card <- function(label, value, note = NULL, wide = FALSE) {
  div(
    class = paste("metric-card", if (wide) "metric-wide"),
    span(class = "metric-label", label),
    strong(class = "metric-value", value),
    if (!is.null(note)) span(class = "metric-note", note)
  )
}

plot_scene <- function(scene, revealed) {
  data <- scene$data
  model <- lm(y ~ x, data = data)
  xr <- range(data$x)
  yr <- range(data$y)

  p <- ggplot(data, aes(x, y)) +
    ggsketch::geom_sketch_abline(
      intercept = unname(coef(model)[1]),
      slope = unname(coef(model)[2]),
      colour = accent,
      linewidth = 1.4,
      roughness = 0.45,
      seed = 10L
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.06, 0.06))) +
    scale_y_continuous(expand = expansion(mult = c(0.10, 0.10))) +
    labs(x = "x", y = "y") +
    ggsketch::theme_sketch(
      base_size = 15,
      base_family = "auto",
      rough_frame = TRUE,
      roughness = 0.35,
      paper = "notebook",
      seed = 4L
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = margin(18, 22, 16, 18),
      axis.title = element_text(size = 15),
      panel.grid.minor = element_blank()
    )

  if (!revealed) {
    return(
      p +
        annotate(
          "label",
          x = mean(xr),
          y = mean(yr),
          label = "What do you imagine\nthe data look like?",
          family = "sans",
          size = 6,
          lineheight = 0.95,
          colour = ink,
          fill = paper,
          linewidth = 0
        )
    )
  }

  if (scene$id == "linear") {
    p <- p +
      ggsketch::geom_sketch_point(
        colour = ink,
        alpha = 0.70,
        size = 2.6,
        roughness = 0.35,
        seed = 21L
      )
  }

  if (scene$id == "nonlinear") {
    p <- p +
      ggsketch::geom_sketch_point(
        colour = ink,
        alpha = 0.65,
        size = 2.5,
        roughness = 0.35,
        seed = 22L
      ) +
      ggsketch::geom_sketch_smooth(
        method = "loess",
        formula = y ~ x,
        se = FALSE,
        colour = blue,
        linewidth = 1.25,
        roughness = 0.45,
        seed = 23L
      )
  }

  if (scene$id == "outliers") {
    p <- p +
      ggsketch::geom_sketch_point(
        aes(colour = influential, size = influential),
        alpha = 0.82,
        roughness = 0.35,
        seed = 24L
      ) +
      scale_colour_manual(
        values = c(
          "Other observations" = ink,
          "Influential observations" = blue
        )
      ) +
      scale_size_manual(
        values = c(
          "Other observations" = 2.2,
          "Influential observations" = 4.2
        )
      )
  }

  if (scene$id == "clusters") {
    p <- p +
      ggsketch::geom_sketch_point(
        aes(colour = group),
        alpha = 0.78,
        size = 2.7,
        roughness = 0.35,
        seed = 25L
      ) +
      ggsketch::geom_sketch_smooth(
        aes(colour = group),
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        linewidth = 1,
        roughness = 0.40,
        seed = 26L
      ) +
      scale_colour_manual(values = c("Group A" = blue, "Group B" = green))
  }

  if (scene$id == "heteroskedastic") {
    data$prediction <- predict(model)

    p <- p +
      geom_segment(
        data = data,
        aes(x = x, xend = x, y = prediction, yend = y),
        inherit.aes = FALSE,
        colour = blue,
        alpha = 0.16,
        linewidth = 0.45
      ) +
      ggsketch::geom_sketch_point(
        colour = ink,
        alpha = 0.62,
        size = 2.4,
        roughness = 0.35,
        seed = 27L
      )
  }

  if (scene$id == "simpson") {
    p <- p +
      ggsketch::geom_sketch_point(
        aes(colour = group),
        alpha = 0.82,
        size = 2.7,
        roughness = 0.35,
        seed = 28L
      ) +
      ggsketch::geom_sketch_smooth(
        aes(colour = group),
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        linewidth = 1.05,
        roughness = 0.40,
        seed = 29L
      ) +
      scale_colour_manual(
        values = c(
          "Group A" = blue,
          "Group B" = green,
          "Group C" = purple
        )
      )
  }

  p +
    annotate(
      "label",
      x = xr[1] + 0.03 * diff(xr),
      y = yr[2] - 0.04 * diff(yr),
      label = scene$plot_note,
      hjust = 0,
      vjust = 1,
      family = "sans",
      size = 4.2,
      colour = ink,
      fill = paper,
      linewidth = 0
    )
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "preconnect",
      href = "https://fonts.gstatic.com",
      crossorigin = "anonymous"
    ),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Caveat:wght@500;600;700&family=IBM+Plex+Sans:wght@400;500;600&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("\n      :root {\n        --paper: #fbf7ed;\n        --canvas: #efece4;\n        --ink: #25313b;\n        --muted: #66717a;\n        --accent: #d46a4c;\n        --blue: #466c8b;\n        --rule: rgba(70, 108, 139, 0.20);\n      }\n\n      html, body, .container-fluid { height: 100%; }\n\n      body {\n        background: var(--canvas);\n        color: var(--ink);\n        font-family: 'IBM Plex Sans', sans-serif;\n      }\n\n      .story-shell {\n        width: min(1500px, 100%);\n        margin: 0 auto;\n        padding: 24px 30px 20px;\n      }\n\n      .story-header {\n        display: flex;\n        align-items: end;\n        justify-content: space-between;\n        gap: 24px;\n        margin-bottom: 18px;\n      }\n\n      .story-kicker, .chapter-kicker {\n        margin: 0 0 2px;\n        color: var(--accent);\n        font-size: 0.74rem;\n        font-weight: 600;\n        letter-spacing: 0.14em;\n        text-transform: uppercase;\n      }\n\n      .story-title {\n        margin: -8px 0 -2px;\n        font-family: 'Caveat', cursive;\n        font-size: clamp(3rem, 5vw, 5.2rem);\n        font-weight: 700;\n        line-height: 0.95;\n      }\n\n      .story-subtitle {\n        margin: 8px 0 0;\n        color: var(--muted);\n        font-size: 0.98rem;\n      }\n\n      .progress-strip {\n        display: flex;\n        align-items: flex-start;\n        justify-content: flex-end;\n        gap: 8px;\n        min-width: 430px;\n      }\n\n      .progress-step {\n        position: relative;\n        width: 62px;\n        color: #8a918f;\n        text-align: center;\n        font-size: 0.65rem;\n      }\n\n      .progress-step::before {\n        content: '';\n        position: absolute;\n        top: 13px;\n        left: -12px;\n        width: 24px;\n        border-top: 2px solid #cbc7bd;\n      }\n\n      .progress-step:first-child::before { display: none; }\n\n      .progress-step span {\n        display: grid;\n        place-items: center;\n        width: 28px;\n        height: 28px;\n        margin: 0 auto 4px;\n        border: 2px solid #b9b5ab;\n        border-radius: 46% 54% 48% 52%;\n        background: var(--paper);\n        font-family: 'Caveat', cursive;\n        font-size: 1rem;\n        font-weight: 700;\n      }\n\n      .progress-step.active, .progress-step.done { color: var(--ink); }\n      .progress-step.active span { border-color: var(--accent); color: var(--accent); transform: rotate(-3deg); }\n      .progress-step.done span { border-color: var(--blue); background: rgba(70, 108, 139, 0.08); }\n\n      .story-grid {\n        display: grid;\n        grid-template-columns: minmax(0, 1.72fr) minmax(330px, 0.78fr);\n        gap: 22px;\n        min-height: 640px;\n      }\n\n      .paper-panel {\n        position: relative;\n        overflow: hidden;\n        border: 1px solid #d8d1c2;\n        border-radius: 4px 10px 5px 8px;\n        background: var(--paper);\n        box-shadow: 8px 9px 0 rgba(37, 49, 59, 0.07);\n      }\n\n      .plot-panel { min-height: 640px; }\n      .plot-panel .shiny-plot-output { height: 640px !important; }\n\n      .narrative-panel {\n        display: flex;\n        flex-direction: column;\n        padding: 26px 28px 22px;\n        background-image: repeating-linear-gradient(\n          to bottom,\n          transparent 0,\n          transparent 31px,\n          var(--rule) 32px\n        );\n      }\n\n      .chapter-number {\n        display: inline-block;\n        margin-bottom: 4px;\n        font-family: 'Caveat', cursive;\n        color: var(--blue);\n        font-size: 1.35rem;\n        transform: rotate(-2deg);\n      }\n\n      .chapter-title {\n        margin: 0 0 12px;\n        font-family: 'Caveat', cursive;\n        font-size: clamp(2rem, 3vw, 3rem);\n        font-weight: 700;\n        line-height: 0.95;\n      }\n\n      .chapter-question {\n        margin: 4px 0 10px;\n        font-size: 1.05rem;\n        font-weight: 600;\n        line-height: 1.35;\n      }\n\n      .chapter-copy {\n        margin-bottom: 4px;\n        color: #4f5a62;\n        line-height: 1.55;\n      }\n\n      .metric-grid {\n        display: grid;\n        grid-template-columns: 1fr 1fr;\n        gap: 9px;\n        margin: 14px 0;\n      }\n\n      .metric-card {\n        min-height: 72px;\n        padding: 9px 11px;\n        border: 1px solid rgba(37, 49, 59, 0.18);\n        border-radius: 5px 9px 6px 8px;\n        background: rgba(251, 247, 237, 0.90);\n      }\n\n      .metric-wide { grid-column: 1 / -1; }\n      .metric-label, .metric-note { display: block; }\n      .metric-label { color: var(--muted); font-size: 0.66rem; text-transform: uppercase; letter-spacing: 0.08em; }\n      .metric-value { display: block; margin-top: 2px; font-family: 'Caveat', cursive; font-size: 1.52rem; line-height: 1; }\n      .metric-note { margin-top: 4px; color: var(--muted); font-size: 0.65rem; }\n\n      .reveal-box {\n        margin-top: 4px;\n        padding: 12px 14px 10px;\n        border-left: 4px solid var(--accent);\n        background: rgba(212, 106, 76, 0.07);\n      }\n\n      .reveal-box h3 {\n        margin: 0 0 4px;\n        font-family: 'Caveat', cursive;\n        font-size: 1.65rem;\n      }\n\n      .reveal-box p { margin: 0; line-height: 1.45; }\n\n      .takeaway {\n        margin: 13px 0 0;\n        padding: 6px 0 2px;\n        border-bottom: 5px solid rgba(242, 194, 76, 0.38);\n        font-family: 'Caveat', cursive;\n        font-size: 1.32rem;\n        font-weight: 600;\n        line-height: 1.15;\n        transform: rotate(-0.6deg);\n      }\n\n      .story-nav {\n        display: grid;\n        grid-template-columns: 1fr auto 1fr;\n        align-items: center;\n        gap: 12px;\n        margin-top: auto;\n        padding-top: 16px;\n      }\n\n      .story-nav .btn {\n        border-width: 2px;\n        border-radius: 7px 11px 6px 10px;\n        font-weight: 600;\n      }\n\n      .story-nav .btn-primary {\n        padding: 9px 20px;\n        border-color: var(--accent);\n        background: var(--accent);\n        box-shadow: 3px 4px 0 rgba(37, 49, 59, 0.14);\n        font-family: 'Caveat', cursive;\n        font-size: 1.28rem;\n      }\n\n      .story-nav .btn-outline-secondary { border-color: #9b9a94; color: var(--ink); }\n      .story-nav .next-wrap { text-align: right; }\n      .story-nav .btn:disabled { opacity: 0.34; }\n\n      .story-footer {\n        margin-top: 15px;\n        color: var(--muted);\n        font-size: 0.74rem;\n        text-align: center;\n      }\n\n      @media (max-width: 980px) {\n        .story-shell { padding: 18px 15px; }\n        .story-header { align-items: flex-start; flex-direction: column; }\n        .progress-strip { justify-content: flex-start; min-width: 0; width: 100%; overflow-x: auto; padding-bottom: 4px; }\n        .story-grid { grid-template-columns: 1fr; }\n        .plot-panel, .plot-panel .shiny-plot-output { min-height: 480px; height: 480px !important; }\n      }\n    "))
  ),
  div(
    class = "story-shell",
    div(
      class = "story-header",
      div(
        p(class = "story-kicker", "A visual statistics story"),
        h1(class = "story-title", "Draw the Data"),
        p(
          class = "story-subtitle",
          "The same fitted line can hide very different stories."
        )
      ),
      uiOutput("progress")
    ),
    div(
      class = "story-grid",
      div(class = "paper-panel plot-panel", plotOutput("scene_plot")),
      uiOutput("narrative")
    ),
    div(
      class = "story-footer",
      "Built as a guided Quasi-Anscombe experiment with Shiny, ggplot2, ggsketch, and klassets."
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  chapter <- reactiveVal(1L)
  revealed <- reactiveVal(FALSE)

  current_scene <- reactive({
    scenes[[chapter()]]
  })

  output$progress <- renderUI({
    div(
      class = "progress-strip",
      lapply(seq_along(scenes), function(i) {
        state <- if (i < chapter()) {
          "done"
        } else if (i == chapter()) {
          "active"
        } else {
          ""
        }

        div(
          class = paste("progress-step", state),
          span(i),
          scenes[[i]]$short
        )
      })
    )
  })

  output$scene_plot <- renderPlot({
    plot_scene(current_scene(), revealed())
  }, res = 110)

  output$narrative <- renderUI({
    scene <- current_scene()
    stats <- scene$stats

    div(
      class = "paper-panel narrative-panel",
      div(
        span(
          class = "chapter-number",
          sprintf("Chapter %s of %s", chapter(), length(scenes))
        ),
        p(class = "chapter-kicker", "Read the summary first"),
        h2(class = "chapter-title", scene$title),
        p(class = "chapter-question", scene$question),
        p(class = "chapter-copy", scene$setup),
        div(
          class = "metric-grid",
          metric_card("Observations", stats$n),
          metric_card("Mean x", fmt(stats$mean_x)),
          metric_card("Mean y", fmt(stats$mean_y)),
          metric_card("Correlation", fmt(stats$correlation)),
          metric_card(
            "Fitted line",
            sprintf("y = %s + %sx", fmt(stats$intercept), fmt(stats$slope)),
            "The headline result",
            wide = TRUE
          )
        ),
        if (revealed()) {
          tagList(
            div(
              class = "reveal-box",
              h3(scene$reveal_title),
              p(scene$explanation)
            ),
            p(class = "takeaway", scene$takeaway)
          )
        } else {
          p(
            class = "chapter-copy",
            strong("Pause before revealing the points. "),
            "Try to picture the dataset described by these numbers."
          )
        }
      ),
      div(
        class = "story-nav",
        actionButton(
          "previous",
          "Previous",
          class = "btn-outline-secondary",
          disabled = chapter() == 1L
        ),
        actionButton(
          "reveal",
          if (revealed()) "Hide the data" else "Draw the data",
          class = "btn-primary"
        ),
        div(
          class = "next-wrap",
          actionButton(
            "next",
            if (chapter() == length(scenes)) "Start over" else "Next",
            class = "btn-outline-secondary",
            disabled = !revealed()
          )
        )
      )
    )
  })

  observeEvent(input$reveal, {
    revealed(!revealed())
  }, ignoreInit = TRUE)

  observeEvent(input$previous, {
    chapter(max(1L, chapter() - 1L))
    revealed(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$next, {
    if (!revealed()) return()

    if (chapter() == length(scenes)) {
      chapter(1L)
    } else {
      chapter(chapter() + 1L)
    }

    revealed(FALSE)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
