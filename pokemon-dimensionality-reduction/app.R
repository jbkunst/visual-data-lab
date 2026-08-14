# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(highcharter)
library(markdown)
library(Rtsne)
library(uwot)

# data --------------------------------------------------------------------
app_dir <- if (file.exists("prepare_data.R")) "." else "pokemon-dimensionality-reduction"
source(file.path(app_dir, "prepare_data.R"), local = TRUE)

data_file <- file.path(app_dir, "pokemon-data.rds")
pokemon_bundle <- if (file.exists(data_file)) {
  readRDS(data_file)
} else {
  prepare_pokemon_data(cache_file = file.path(tempdir(), "visual-data-lab-pokemon.rds"))
}

pokemon <- pokemon_bundle$data

similarity_presets <- list(
  balanced = c(continuous = 1, binary = 1, egg_groups = 1, species_traits = 1),
  battle = c(continuous = 2, binary = 0.5, egg_groups = 0.25, species_traits = 0.25),
  ecology = c(continuous = 0.4, binary = 0.5, egg_groups = 2, species_traits = 2)
)

recipe_labels <- c(
  balanced = "Balanced profile", battle = "Battle & morphology",
  ecology = "Breeding & ecology", custom = "Custom weights"
)

# pokemon_feature_matrix() prepares the mixed data for every similarity recipe:
# - continuous variables are standardized;
# - binary variables stay 0/1;
# - nominal variables are one-hot encoded;
# - semantic blocks are normalized so dummy-heavy groups do not dominate.

# theme -------------------------------------------------------------------
apptheme <- bs_theme(
  version = 5,
  bg = "#f5f7fb",
  fg = "#172033",
  primary = "#2a75bb",
  secondary = "#ffcb05",
  "tooltip-bg" = "#ffffff",
  "tooltip-color" = "#163a63",
  "tooltip-opacity" = 1
)

sidebar <- purrr::partial(bslib::sidebar, width = 305)
card <- purrr::partial(bslib::card, full_screen = TRUE)

# helpers -----------------------------------------------------------------
source(file.path(app_dir, "app_helpers.R"), local = TRUE)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  title = "Pokémon Dimensionality Reduction",
  theme = apptheme,
  padding = 0,
  gap = 0,
  tags$head(htmltools::includeCSS("styles.css")),
  div(
    class = "pokemon-topbar",
    div(
      class = "pokemon-brand",
      span(class = "pokeball-mark"),
      div(
        div(class = "pokemon-brand-title", "Pokémon Lab"),
        div(class = "pokemon-brand-subtitle", "Dimensionality Reduction")
      )
    ),
    div(
      class = "pokemon-topbar-actions",
      div(class = "pokemon-topbar-caption", "Gotta project 'em all!"),
      actionButton(
        "similarity_info", label = NULL,
        icon = bsicons::bs_icon(
          "gear", title = "Similarity details", a11y = "sem"
        ),
        class = "pokemon-info-button",
        title = "How Pokémon similarity is calculated"
      )
    )
  ),
  layout_sidebar(
    fill = TRUE,
    sidebar = sidebar(
      selectInput(
        "method",
        vdltheme::input_label_vdl(
          "Method",
          "Chooses the algorithm used to project the selected Pokémon into two dimensions."
        ),
        choices = c("PCA" = "pca", "t-SNE" = "tsne", "UMAP" = "umap"),
        selected = "tsne"
      ),
      selectInput(
        "recipe",
        vdltheme::input_label_vdl(
          "What makes Pokémon similar?",
          "Changes the relative influence of stats, breeding, and species traits. Type is only a visual overlay."
        ),
        choices = stats::setNames(names(recipe_labels), recipe_labels),
        selected = "balanced"
      ),
      sliderInput(
        "generations",
        vdltheme::input_label_vdl(
          "Generations",
          "Limits which generations enter the feature matrix and projection."
        ),
        min = 1, max = 9, value = c(1, 9), step = 1,
        ticks = TRUE, sep = ""
      ),
      conditionalPanel(
        "input.recipe === 'custom'",
        sliderInput("weight_continuous", "Stats & morphology", 0, 3, 1, 0.25),
        sliderInput("weight_binary", "Special flags", 0, 3, 1, 0.25),
        sliderInput("weight_egg_groups", "Egg groups", 0, 3, 1, 0.25),
        sliderInput("weight_species_traits", "Species traits", 0, 3, 1, 0.25)
      ),
      conditionalPanel(
        "input.method === 'tsne'",
        sliderInput(
          "perplexity",
          vdltheme::input_label_vdl(
            "Perplexity",
            "Controls the effective neighborhood size used by t-SNE."
          ),
          min = 5,
          max = 100,
          value = 40,
          step = 5
        ),
        sliderInput(
          "iterations",
          vdltheme::input_label_vdl(
            "Iterations",
            "Sets how long t-SNE refines the arrangement."
          ),
          min = 250,
          max = 2000,
          value = 750,
          step = 250
        )
      ),
      conditionalPanel(
        "input.method === 'umap'",
        sliderInput(
          "n_neighbors",
          vdltheme::input_label_vdl(
            "Neighbors",
            "Controls how much local versus broader structure UMAP preserves."
          ),
          min = 2,
          max = 100,
          value = 30,
          step = 1
        ),
        sliderInput(
          "min_dist",
          vdltheme::input_label_vdl(
            "Minimum distance",
            "Controls how tightly UMAP may pack nearby Pokémon."
          ),
          min = 0,
          max = 0.95,
          value = 0.15,
          step = 0.05
        )
      ),
      numericInput(
        "seed",
        vdltheme::input_label_vdl(
          "Seed",
          "Makes stochastic t-SNE and UMAP results reproducible."
        ),
        value = 13242, min = 1, step = 1
      ),
      actionButton(
        "run",
        "Re-run projection",
        class = "btn btn-primary pokemon-run-button"
      ),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
        ),
        accordion_panel(
          "Visual settings",
          sliderInput(
            "sprite_size",
            vdltheme::input_label_vdl(
              "Pokémon size",
              "Changes only the displayed sprite size; coordinates stay fixed."
            ),
            min = 18, max = 40, value = 28, step = 1, ticks = FALSE
          )
        )
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    card(
      class = "pokemon-card",
      card_header(
        div(
          class = "pokemon-chart-header",
          div(
            div(class = "pokemon-chart-kicker", "Projection map"),
            div(class = "pokemon-chart-title", textOutput("chart_title", inline = TRUE))
          ),
          div(class = "pokemon-chart-meta", textOutput("chart_meta", inline = TRUE))
        )
      ),
      card_body(
        class = "pokemon-chart-body",
        highchartOutput("embedding", height = "calc(100vh - 154px)")
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  observeEvent(input$similarity_info, {
    showModal(modalDialog(
      withMathJax(div(
        class = "pokemon-similarity-modal",
        tags$p(
          "Each row represents one Pokémon. Its numeric, binary, and categorical",
          "features are prepared before PCA, t-SNE, or UMAP sees them."
        ),
        tags$ul(
          tags$li("Continuous features are median-imputed and standardized as z-scores."),
          tags$li("Binary features remain 0/1, so rare flags are not artificially amplified."),
          tags$li("Egg groups and species traits are one-hot encoded."),
          tags$li("Pokémon types are excluded, then reused only to color or spotlight the resulting map."),
          tags$li("Related columns remain in semantic blocks so wide blocks do not dominate by size alone.")
        ),
        tags$h5("Block weighting"),
        tags$p("For each block ", tags$em("b"), ", the app applies:"),
        HTML("\\[X_b^* = X_b \\, \\frac{w_b}{\\sqrt{p_b}}\\]"),
        tags$p(
          HTML("Here \\(w_b\\) is the selected weight and \\(p_b\\) is the number of columns. "),
          "Dividing by ", HTML("\\(\\sqrt{p_b}\\)"),
          " prevents a block from gaining importance merely because one-hot encoding created many columns."
        ),
        tags$h5("Effect on similarity"),
        tags$p("The squared Euclidean distance between Pokémon ", tags$em("i"), " and ", tags$em("j"), " becomes:"),
        HTML(paste0(
          "\\[d^2(i,j) = \\sum_b \\frac{w_b^2}{p_b}",
          "\\left\\|x_{ib}-x_{jb}\\right\\|^2\\]"
        )),
        tags$p(
          "Larger weights make differences in that block matter more.",
          "Since distance squares the scaled differences, a weight of 2 contributes",
          "four times as much to squared distance, holding everything else constant."
        ),
        tags$h5("Weights used by each profile"),
        tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm pokemon-weight-table",
            tags$thead(tags$tr(
              tags$th("Profile"), tags$th("Stats"), tags$th("Flags"),
              tags$th("Egg groups"), tags$th("Species traits")
            )),
            tags$tbody(
              tags$tr(tags$th("Balanced"), tags$td("1"), tags$td("1"), tags$td("1"), tags$td("1")),
              tags$tr(tags$th("Battle & morphology"), tags$td("2"), tags$td("0.5"), tags$td("0.25"), tags$td("0.25")),
              tags$tr(tags$th("Breeding & ecology"), tags$td("0.4"), tags$td("0.5"), tags$td("2"), tags$td("2"))
            )
          )
        ),
        tags$p(
          class = "small text-muted",
          "Custom weights use the four sliders directly. A weight of 0 removes that block from the distance."
        ),
        tags$div(
          class = "pokemon-modal-note",
          tags$strong("Important: "),
          "these weights are not algorithm arguments. The app first creates the weighted feature matrix,",
          "then passes that matrix to PCA, t-SNE, or UMAP."
        )
      )),
      title = tagList(
        bsicons::bs_icon("gear", size = "0.9em", a11y = "none"),
        "How Pokémon similarity is calculated"
      ),
      size = "l", easyClose = TRUE,
      footer = modalButton("Got it")
    ))
  })

  feature_weights <- reactive({
    if (!identical(input$recipe, "custom")) {
      return(similarity_presets[[input$recipe]])
    }

    weights <- c(
      continuous = input$weight_continuous,
      binary = input$weight_binary,
      egg_groups = input$weight_egg_groups,
      species_traits = input$weight_species_traits
    )
    validate(need(sum(weights) > 0, "Give at least one feature block a positive weight."))
    weights
  })

  projection <- eventReactive(
    list(input$run, input$method, input$recipe, input$generations),
    {
      withProgress(message = paste("Running", method_label(input$method)), value = 0.2, {
        weights <- feature_weights()
        selected <- pokemon |>
          filter(
            generation_id >= input$generations[[1L]],
            generation_id <= input$generations[[2L]]
          )
        validate(need(nrow(selected) >= 10L, "Select at least 10 Pokémon."))
        x <- pokemon_feature_matrix(selected, block_weights = weights)
        xy <- run_projection(
          method = input$method, x = x,
          perplexity = input$perplexity, iterations = input$iterations,
          n_neighbors = input$n_neighbors, min_dist = input$min_dist,
          seed = input$seed
        )
        incProgress(0.8)
        list(
          data = selected, x = x, xy = xy,
          method = input$method, recipe = input$recipe,
          generations = input$generations,
          perplexity = input$perplexity, iterations = input$iterations,
          n_neighbors = input$n_neighbors, min_dist = input$min_dist,
          weights = weights
        )
      })
    },
    ignoreNULL = FALSE
  )

  observeEvent(input$selected_pokemon_id, {
    result <- projection()
    selected_index <- match(as.integer(input$selected_pokemon_id), result$data$id)
    req(!is.na(selected_index))
    showModal(pokemon_profile_modal(result, selected_index))
  }, ignoreInit = TRUE)

  output$chart_title <- renderText({
    paste(method_label(projection()$method), "Pokémon map")
  })

  output$chart_meta <- renderText({
    result <- projection()
    recipe <- unname(recipe_labels[[result$recipe]])
    context <- paste(
      recipe,
      paste(format(nrow(result$data), big.mark = ","), "Pokémon"),
      paste(ncol(result$x), "features"),
      "type as overlay",
      sep = " · "
    )

    if (identical(result$method, "pca")) {
      return(paste(context, "linear baseline", sep = " · "))
    }
    if (identical(result$method, "tsne")) {
      return(paste0(
        context,
        " · perplexity ", result$perplexity,
        " · ", result$iterations, " iterations"
      ))
    }
    paste0(
      context,
      " · ", result$n_neighbors, " neighbors · min dist ", result$min_dist
    )
  })

  output$embedding <- renderHighchart({
    xy <- projection()$xy
    selected <- projection()$data
    pokemon_series <- make_type_series(selected, xy, input$sprite_size)

    highchart() |>
      hc_chart(
        type = "scatter",
        zoomType = "xy",
        panning = list(enabled = TRUE, type = "xy"), panKey = "shift",
        backgroundColor = "transparent", animation = FALSE,
        spacing = list(18, 18, 18, 18),
        events = list(load = legend_type_focus_js)
      ) |>
      hc_title(text = NULL) |>
      hc_xAxis(visible = FALSE) |>
      hc_yAxis(visible = FALSE) |>
      hc_add_series_list(pokemon_series) |>
      hc_legend(
        enabled = TRUE, align = "center", verticalAlign = "top",
        layout = "horizontal", symbolRadius = 5,
        itemDistance = 12, margin = 12,
        itemStyle = list(fontWeight = "normal"),
        itemHoverStyle = list(fontWeight = "normal")
      ) |>
      hc_tooltip(
        useHTML = TRUE, outside = TRUE, borderWidth = 0,
        borderRadius = 14, shadow = TRUE, padding = 0,
        headerFormat = "", pointFormat = tooltip_html
      ) |>
      hc_plotOptions(series = list(
        animation = FALSE, cursor = "pointer",
        point = list(events = list(click = JS(
          "function () { Shiny.setInputValue('selected_pokemon_id', this.options.pokemon_id, {priority: 'event'}); }"
        ))),
        states = list(
          inactive = list(opacity = 1),
          hover = list(
            halo = list(size = 34, opacity = 0.24), brightness = 0.08
          )
        )
      )) |>
      hc_credits(enabled = FALSE)
  })
}

shinyApp(ui, server)
