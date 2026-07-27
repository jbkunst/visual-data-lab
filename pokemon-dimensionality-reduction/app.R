# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(highcharter)
library(Rtsne)
library(uwot)

# data --------------------------------------------------------------------
source("prepare_data.R", local = TRUE)

pokemon_bundle <- prepare_pokemon_data(
  cache_file = file.path(tempdir(), "visual-data-lab-pokemon.rds")
)

pokemon <- pokemon_bundle$data
pokemon_x <- pokemon_bundle$x

# pokemon_x is already prepared for mixed data:
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
  secondary = "#ffcb05"
)

sidebar <- purrr::partial(bslib::sidebar, width = 305)
card <- purrr::partial(bslib::card, full_screen = TRUE)

# helpers -----------------------------------------------------------------
method_label <- function(method) {
  switch(
    method,
    pca = "PCA",
    tsne = "t-SNE",
    umap = "UMAP",
    method
  )
}

run_projection <- function(
  method,
  x,
  perplexity,
  iterations,
  n_neighbors,
  min_dist,
  seed
) {
  set.seed(as.integer(seed))

  if (identical(method, "pca")) {
    # PCA still centers the final mixed feature matrix. We do not scale again:
    # doing so would undo the deliberate 0/1 treatment and block weighting.
    fit <- stats::prcomp(x, center = TRUE, scale. = FALSE)
    return(fit$x[, 1:2, drop = FALSE])
  }

  if (identical(method, "tsne")) {
    max_perplexity <- max(2, floor((nrow(x) - 1) / 3) - 1)
    perplexity <- min(as.numeric(perplexity), max_perplexity)

    fit <- Rtsne::Rtsne(
      x,
      dims = 2,
      perplexity = perplexity,
      max_iter = as.integer(iterations),
      check_duplicates = FALSE,
      pca = TRUE,
      verbose = FALSE
    )

    return(fit$Y)
  }

  # UMAP and t-SNE both consume the same deliberately prepared Euclidean
  # feature space, which makes their projections comparable at the input level.
  uwot::umap(
    x,
    n_components = 2,
    n_neighbors = min(as.integer(n_neighbors), nrow(x) - 1L),
    min_dist = as.numeric(min_dist),
    metric = "euclidean",
    n_threads = 1,
    verbose = FALSE
  )
}

make_points <- function(data, xy) {
  data |>
    mutate(
      x = xy[, 1],
      y = xy[, 2],
      pokemon_label = stringr::str_to_title(stringr::str_replace_all(pokemon, "-", " ")),
      type_1_label = stringr::str_to_title(type_1),
      type_2_label = if_else(type_2 == "none", "—", stringr::str_to_title(type_2)),
      generation_label = stringr::str_replace(generation, "GENERATION-", "Gen "),
      height_m = round(height / 10, 1),
      weight_kg = round(weight / 10, 1),
      growth_rate_label = stringr::str_to_title(stringr::str_replace_all(growth_rate, "-", " ")),
      habitat_label = if_else(
        habitat == "unknown",
        "Unknown",
        stringr::str_to_title(stringr::str_replace_all(habitat, "-", " "))
      ),
      special_status = case_when(
        is_mythical == 1 ~ "Mythical",
        is_legendary == 1 ~ "Legendary",
        is_baby == 1 ~ "Baby",
        TRUE ~ "—"
      )
    ) |>
    select(
      x, y, pokemon_label, type_1_label, type_2_label,
      generation_label, type_color, height_m, weight_kg,
      hp, attack, defense, special_attack, special_defense, speed,
      capture_rate, base_happiness, hatch_counter,
      growth_rate_label, habitat_label, special_status,
      sprite_url, artwork_url
    ) |>
    purrr::pmap(function(
      x, y, pokemon_label, type_1_label, type_2_label,
      generation_label, type_color, height_m, weight_kg,
      hp, attack, defense, special_attack, special_defense, speed,
      capture_rate, base_happiness, hatch_counter,
      growth_rate_label, habitat_label, special_status,
      sprite_url, artwork_url
    ) {
      list(
        x = x,
        y = y,
        name = pokemon_label,
        pokemon = pokemon_label,
        type_1 = type_1_label,
        type_2 = type_2_label,
        generation = generation_label,
        type_color = type_color,
        height_m = height_m,
        weight_kg = weight_kg,
        hp = hp,
        attack = attack,
        defense = defense,
        special_attack = special_attack,
        special_defense = special_defense,
        speed = speed,
        capture_rate = capture_rate,
        base_happiness = base_happiness,
        hatch_counter = hatch_counter,
        growth_rate = growth_rate_label,
        habitat = habitat_label,
        special_status = special_status,
        artwork_url = artwork_url,
        color = type_color,
        marker = list(
          symbol = sprintf("url(%s)", sprite_url),
          width = 26,
          height = 26
        )
      )
    })
}

tooltip_html <- paste0(
  '<div class="pokemon-tooltip">',
  '<div class="pokemon-tooltip-top">',
  '<img src="{point.artwork_url}" class="pokemon-artwork">',
  '<div class="pokemon-tooltip-title">',
  '<div class="pokemon-name">{point.pokemon}</div>',
  '<div class="pokemon-generation">{point.generation}</div>',
  '<div class="pokemon-types">',
  '<span class="pokemon-type" style="background:{point.type_color}">{point.type_1}</span>',
  '<span class="pokemon-type-secondary">{point.type_2}</span>',
  '</div>',
  '<div class="pokemon-size">{point.height_m} m · {point.weight_kg} kg</div>',
  '</div></div>',
  '<table class="pokemon-stats">',
  '<tr><td>HP</td><td><b>{point.hp}</b></td><td>Attack</td><td><b>{point.attack}</b></td></tr>',
  '<tr><td>Defense</td><td><b>{point.defense}</b></td><td>Speed</td><td><b>{point.speed}</b></td></tr>',
  '<tr><td>Sp. Atk</td><td><b>{point.special_attack}</b></td><td>Sp. Def</td><td><b>{point.special_defense}</b></td></tr>',
  '<tr><td>Capture</td><td><b>{point.capture_rate}</b></td><td>Happiness</td><td><b>{point.base_happiness}</b></td></tr>',
  '<tr><td>Growth</td><td><b>{point.growth_rate}</b></td><td>Habitat</td><td><b>{point.habitat}</b></td></tr>',
  '<tr><td>Status</td><td><b>{point.special_status}</b></td><td>Hatch</td><td><b>{point.hatch_counter}</b></td></tr>',
  '</table>',
  '</div>'
)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  title = "Pokémon Dimensionality Reduction",
  theme = apptheme,
  padding = 0,
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
    div(class = "pokemon-topbar-caption", "Gotta project 'em all!")
  ),
  layout_sidebar(
    fill = TRUE,
    sidebar = sidebar(
      title = "Projection",
      selectInput(
        "method",
        "Method",
        choices = c("PCA" = "pca", "t-SNE" = "tsne", "UMAP" = "umap"),
        selected = "tsne"
      ),
      conditionalPanel(
        "input.method === 'tsne'",
        sliderInput(
          "perplexity",
          "Perplexity",
          min = 5,
          max = 100,
          value = 40,
          step = 5
        ),
        sliderInput(
          "iterations",
          "Iterations",
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
          "Neighbors",
          min = 2,
          max = 100,
          value = 30,
          step = 1
        ),
        sliderInput(
          "min_dist",
          "Minimum distance",
          min = 0,
          max = 0.95,
          value = 0.15,
          step = 0.05
        )
      ),
      numericInput("seed", "Seed", value = 13242, min = 1, step = 1),
      actionButton(
        "run",
        "Run projection",
        class = "btn btn-primary pokemon-run-button"
      ),
      div(
        class = "pokemon-sidebar-note",
        strong(format(nrow(pokemon), big.mark = ",")),
        " Pokémon · stats + capture + species traits"
      ),
      accordion(
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(htmltools::includeMarkdown("readme.md"))
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
  projection <- eventReactive(
    input$run,
    {
      withProgress(message = paste("Running", method_label(input$method)), value = 0.2, {
        xy <- run_projection(
          method = input$method,
          x = pokemon_x,
          perplexity = input$perplexity,
          iterations = input$iterations,
          n_neighbors = input$n_neighbors,
          min_dist = input$min_dist,
          seed = input$seed
        )
        incProgress(0.8)
        xy
      })
    },
    ignoreNULL = FALSE
  )

  output$chart_title <- renderText({
    paste(method_label(input$method), "Pokémon map")
  })

  output$chart_meta <- renderText({
    if (identical(input$method, "pca")) {
      return("Linear baseline")
    }

    if (identical(input$method, "tsne")) {
      return(paste0("Perplexity ", input$perplexity, " · ", input$iterations, " iterations"))
    }

    paste0("", input$n_neighbors, " neighbors · min dist ", input$min_dist)
  })

  output$embedding <- renderHighchart({
    xy <- projection()
    points <- make_points(pokemon, xy)

    highchart() |>
      hc_chart(
        type = "scatter",
        zooming = list(type = "xy"),
        backgroundColor = "transparent",
        animation = FALSE,
        spacing = list(18, 18, 18, 18)
      ) |>
      hc_title(text = NULL) |>
      hc_xAxis(
        title = list(text = NULL),
        labels = list(enabled = FALSE),
        tickLength = 0,
        gridLineWidth = 0,
        lineWidth = 0
      ) |>
      hc_yAxis(
        title = list(text = NULL),
        labels = list(enabled = FALSE),
        tickLength = 0,
        gridLineWidth = 0,
        lineWidth = 0
      ) |>
      hc_add_series(
        data = points,
        name = "Pokémon",
        turboThreshold = 0,
        showInLegend = FALSE
      ) |>
      hc_tooltip(
        useHTML = TRUE,
        outside = TRUE,
        borderWidth = 0,
        borderRadius = 14,
        shadow = TRUE,
        padding = 0,
        headerFormat = "",
        pointFormat = tooltip_html
      ) |>
      hc_plotOptions(
        series = list(
          animation = FALSE,
          cursor = "pointer",
          states = list(
            hover = list(
              halo = list(size = 34, opacity = 0.24),
              brightness = 0.08
            )
          )
        )
      ) |>
      hc_credits(enabled = FALSE)
  })
}

shinyApp(ui, server)
