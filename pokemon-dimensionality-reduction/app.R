# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(highcharter)
library(Rtsne)

umap_available <- requireNamespace("uwot", quietly = TRUE)

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
  balanced = c(continuous = 1, binary = 1, types = 1, egg_groups = 1, species_traits = 1),
  battle = c(continuous = 2, binary = 0.5, types = 0.5, egg_groups = 0.25, species_traits = 0.25),
  types = c(continuous = 0.4, binary = 0.4, types = 2, egg_groups = 0.4, species_traits = 0.4),
  ecology = c(continuous = 0.4, binary = 0.5, types = 0.5, egg_groups = 2, species_traits = 2)
)

recipe_labels <- c(
  balanced = "Balanced profile", battle = "Battle & morphology",
  types = "Pokémon types", ecology = "Breeding & ecology",
  custom = "Custom weights"
)

method_choices <- c("PCA" = "pca", "t-SNE" = "tsne")
if (umap_available) method_choices <- c(method_choices, "UMAP" = "umap")

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

  clean_coordinates <- function(x) {
    x <- unname(as.matrix(x))
    storage.mode(x) <- "double"
    x
  }

  if (identical(method, "pca")) {
    # PCA still centers the final mixed feature matrix. We do not scale again:
    # doing so would undo the deliberate 0/1 treatment and block weighting.
    fit <- stats::prcomp(x, center = TRUE, scale. = FALSE)
    return(clean_coordinates(fit$x[, 1:2, drop = FALSE]))
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

    return(clean_coordinates(fit$Y))
  }

  # UMAP and t-SNE both consume the same deliberately prepared Euclidean
  # feature space, which makes their projections comparable at the input level.
  if (!umap_available) stop("UMAP is not available in this runtime.", call. = FALSE)
  coordinates <- uwot::umap(
    x,
    n_components = 2,
    n_neighbors = min(as.integer(n_neighbors), nrow(x) - 1L),
    min_dist = as.numeric(min_dist),
    metric = "euclidean",
    n_threads = 1,
    verbose = FALSE
  )
  clean_coordinates(coordinates)
}

make_points <- function(data, xy, highlight = "all", sprite_size = 28) {
  spotlight_size <- sprite_size + 7
  muted_size <- max(8, round(sprite_size * 0.3))

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
      ),
      highlighted = highlight == "all" | type_1 == highlight
    ) |>
    select(
      id, x, y, pokemon_label, type_1_label, type_2_label,
      generation_label, type_color, height_m, weight_kg,
      hp, attack, defense, special_attack, special_defense, speed,
      capture_rate, base_happiness, hatch_counter,
      growth_rate_label, habitat_label, special_status,
      sprite_url, artwork_url, highlighted
    ) |>
    purrr::pmap(function(
      id, x, y, pokemon_label, type_1_label, type_2_label,
      generation_label, type_color, height_m, weight_kg,
      hp, attack, defense, special_attack, special_defense, speed,
      capture_rate, base_happiness, hatch_counter,
      growth_rate_label, habitat_label, special_status,
      sprite_url, artwork_url, highlighted
    ) {
      list(
        pokemon_id = as.integer(id),
        x = as.numeric(x),
        y = as.numeric(y),
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
        opacity = if (highlighted) 1 else 0.12,
        marker = list(
          symbol = sprintf("url(%s)", sprite_url),
          width = if (highlight == "all") sprite_size else if (highlighted) spotlight_size else muted_size,
          height = if (highlight == "all") sprite_size else if (highlighted) spotlight_size else muted_size
        )
      )
    })
}

make_halos <- function(data, xy, highlight = "all", sprite_size = 28) {
  highlighted <- highlight == "all" | data$type_1 == highlight
  purrr::pmap(
    list(xy[, 1], xy[, 2], data$type_color, highlighted),
    function(x, y, color, highlighted) {
      list(
        x = as.numeric(x), y = as.numeric(y),
        color = if (highlight != "all" && highlighted) color else "rgba(0,0,0,0)",
        marker = list(symbol = "circle", radius = (sprite_size + 12) / 2)
      )
    }
  )
}

pokemon_label <- function(x) {
  stringr::str_to_title(stringr::str_replace_all(x, "-", " "))
}

type_badge <- function(type) {
  if (is.na(type) || identical(type, "none")) return(NULL)
  color <- pokemon_type_colors[[type]]
  tags$span(
    class = "pokemon-profile-type",
    style = sprintf("background:%s", color),
    pokemon_label(type)
  )
}

profile_fact <- function(label, value) {
  div(
    class = "pokemon-profile-fact",
    tags$span(class = "pokemon-profile-fact-label", label),
    tags$strong(value)
  )
}

metric_distribution <- function(values, selected, label, accent, digits = 0, suffix = "") {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  selected <- as.numeric(selected)
  n_bars <- min(length(values), 100L)
  shown <- if (length(values) > 100L) {
    as.numeric(stats::quantile(
      values, probs = (seq_len(n_bars) - 0.5) / n_bars,
      names = FALSE, type = 8
    ))
  } else {
    sort(values)
  }

  limits <- range(c(values, selected), finite = TRUE)
  scaled <- if (diff(limits) == 0) rep(0.5, length(shown)) else {
    (shown - limits[[1L]]) / diff(limits)
  }
  selected_scaled <- if (diff(limits) == 0) 0.5 else {
    (selected - limits[[1L]]) / diff(limits)
  }
  percentile <- 100 * (
    sum(values < selected) + 0.5 * sum(values == selected)
  ) / length(values)
  selected_index <- 1 + percentile / 100 * (n_bars - 1)

  width <- 300
  baseline <- 68
  plot_height <- 48
  step <- width / n_bars
  bar_width <- max(0.8, step * 0.72)
  bars <- lapply(seq_along(shown), function(i) {
    height <- 4 + plot_height * scaled[[i]]
    tags$rect(
      x = round((i - 1) * step + (step - bar_width) / 2, 2),
      y = round(baseline - height, 2), width = round(bar_width, 2),
      height = round(height, 2), rx = 0.7, fill = "#b7c0cb"
    )
  })
  selected_height <- 4 + plot_height * selected_scaled
  marker_width <- max(3, bar_width + 1)

  div(
    class = "pokemon-metric",
    div(
      class = "pokemon-metric-header",
      tags$span(label),
      tags$strong(
        paste0(format(round(selected, digits), trim = TRUE), suffix),
        tags$small(sprintf("P%02d", round(percentile)))
      )
    ),
    tags$svg(
      class = "pokemon-metric-chart", viewBox = "0 0 300 78",
      preserveAspectRatio = "none", role = "img",
      `aria-label` = sprintf("%s: %s, percentile %.0f", label, selected, percentile),
      tags$line(x1 = 0, y1 = baseline, x2 = width, y2 = baseline, stroke = "#d7dde5"),
      bars,
      tags$rect(
        x = round((selected_index - 1) * step + (step - marker_width) / 2, 2),
        y = round(baseline - selected_height, 2), width = round(marker_width, 2),
        height = round(selected_height, 2), rx = 1,
        fill = accent, stroke = "#ffcb05", `stroke-width` = 1.5
      ),
      tags$text(x = 1, y = 76, class = "pokemon-metric-limit", format(round(limits[[1L]], digits), trim = TRUE)),
      tags$text(x = 299, y = 76, `text-anchor` = "end", class = "pokemon-metric-limit", format(round(limits[[2L]], digits), trim = TRUE))
    )
  )
}

nearest_pokemon <- function(data, x, selected_index, n = 5L) {
  difference <- sweep(x, 2, x[selected_index, ], FUN = "-")
  distance <- sqrt(rowSums(difference^2))
  candidates <- order(distance)
  candidates <- candidates[candidates != selected_index][seq_len(min(n, nrow(data) - 1L))]
  data[candidates, , drop = FALSE] |>
    mutate(distance = distance[candidates])
}

neighbor_card <- function(data) {
  div(
    class = "pokemon-neighbor",
    tags$img(src = data$sprite_url, alt = pokemon_label(data$pokemon)),
    div(
      tags$strong(pokemon_label(data$pokemon)),
      div(class = "pokemon-neighbor-types", type_badge(data$type_1), type_badge(data$type_2)),
      tags$small(sprintf("distance %.2f", data$distance))
    )
  )
}

fullscreen_modal <- function(...) {
  dialog <- modalDialog(..., title = NULL, footer = NULL, easyClose = TRUE, fade = TRUE)
  query <- htmltools::tagQuery(dialog)
  query$addClass("pokemon-profile-shell")
  query$find(".modal-dialog")$addClass("modal-fullscreen")$allTags()
}

pokemon_profile_modal <- function(result, selected_index) {
  data <- result$data
  selected <- data[selected_index, , drop = FALSE]
  accent <- selected$type_color[[1L]]
  name <- pokemon_label(selected$pokemon[[1L]])
  generation <- stringr::str_replace(selected$generation[[1L]], "GENERATION-", "Gen ")
  status <- dplyr::case_when(
    selected$is_mythical[[1L]] == 1 ~ "Mythical",
    selected$is_legendary[[1L]] == 1 ~ "Legendary",
    selected$is_baby[[1L]] == 1 ~ "Baby",
    TRUE ~ "Standard"
  )
  metrics <- list(
    list(label = "HP", variable = "hp", digits = 0, suffix = "", transform = identity),
    list(label = "Attack", variable = "attack", digits = 0, suffix = "", transform = identity),
    list(label = "Defense", variable = "defense", digits = 0, suffix = "", transform = identity),
    list(label = "Special attack", variable = "special_attack", digits = 0, suffix = "", transform = identity),
    list(label = "Special defense", variable = "special_defense", digits = 0, suffix = "", transform = identity),
    list(label = "Speed", variable = "speed", digits = 0, suffix = "", transform = identity),
    list(label = "Height", variable = "height", digits = 1, suffix = " m", transform = function(x) x / 10),
    list(label = "Weight", variable = "weight", digits = 1, suffix = " kg", transform = function(x) x / 10),
    list(label = "Capture rate", variable = "capture_rate", digits = 0, suffix = "", transform = identity),
    list(label = "Base happiness", variable = "base_happiness", digits = 0, suffix = "", transform = identity),
    list(label = "Hatch counter", variable = "hatch_counter", digits = 0, suffix = "", transform = identity),
    list(label = "Base experience", variable = "base_experience", digits = 0, suffix = "", transform = identity)
  )
  metric_charts <- lapply(metrics, function(metric) {
    values <- metric$transform(data[[metric$variable]])
    metric_distribution(
      values, metric$transform(selected[[metric$variable]][[1L]]),
      metric$label, accent, metric$digits, metric$suffix
    )
  })
  neighbors <- nearest_pokemon(data, result$x, selected_index)

  fullscreen_modal(div(
    class = "pokemon-profile-view",
    style = sprintf(
      "--profile-accent:%s;--profile-artwork:url('%s')",
      accent, selected$artwork_url[[1L]]
    ),
    div(
      class = "pokemon-profile-hero",
      htmltools::tagAppendAttributes(
        modalButton(
          label = NULL,
          icon = bsicons::bs_icon("x-lg", title = "Close profile", a11y = "sem")
        ),
        class = "pokemon-profile-close", title = "Close"
      ),
      div(
        class = "pokemon-profile-hero-content",
        tags$img(class = "pokemon-profile-artwork", src = selected$artwork_url[[1L]], alt = name),
        div(
          class = "pokemon-profile-identity",
          tags$small(sprintf("#%04d · %s", selected$id[[1L]], generation)),
          tags$h2(name),
          div(class = "pokemon-profile-types", type_badge(selected$type_1[[1L]]), type_badge(selected$type_2[[1L]])),
          tags$p("A detailed view of this Pokémon within the current similarity profile.")
        )
      )
    ),
    div(
      class = "pokemon-profile-main",
      tags$aside(
        class = "pokemon-profile-sidebar",
        tags$h4("Pokémon profile"),
        profile_fact("Height", paste0(round(selected$height[[1L]] / 10, 1), " m")),
        profile_fact("Weight", paste0(round(selected$weight[[1L]] / 10, 1), " kg")),
        profile_fact("Growth rate", pokemon_label(selected$growth_rate[[1L]])),
        profile_fact("Habitat", pokemon_label(selected$habitat[[1L]])),
        profile_fact("Body shape", pokemon_label(selected$body_shape[[1L]])),
        profile_fact("Body color", pokemon_label(selected$body_color[[1L]])),
        profile_fact("Egg groups", paste(
          pokemon_label(selected$egg_group_1[[1L]]),
          pokemon_label(selected$egg_group_2[[1L]]), sep = " · "
        )),
        profile_fact("Status", status),
        div(
          class = "pokemon-profile-context",
          tags$small("Current similarity profile"),
          tags$strong(unname(recipe_labels[[result$recipe]])),
          tags$span(sprintf("%s Pokémon · %s", format(nrow(data), big.mark = ","), method_label(result$method)))
        )
      ),
      tags$section(
        class = "pokemon-profile-distributions",
        div(
          class = "pokemon-profile-section-heading",
          div(tags$small("RELATIVE POSITION"), tags$h3("How this Pokémon compares")),
          tags$p("Grey bars summarize the selected population; the colored bar marks this Pokémon.")
        ),
        div(class = "pokemon-metric-grid", metric_charts)
      )
    ),
    tags$section(
      class = "pokemon-neighbors-section",
      div(
        class = "pokemon-profile-section-heading",
        div(tags$small("WEIGHTED FEATURE SPACE"), tags$h3("Nearest Pokémon")),
        tags$p("Closest profiles before PCA, t-SNE, or UMAP reduces them to two dimensions.")
      ),
      div(
        class = "pokemon-neighbor-grid",
        lapply(seq_len(nrow(neighbors)), function(i) neighbor_card(neighbors[i, , drop = FALSE]))
      )
    )
  ))
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
        choices = method_choices,
        selected = "tsne"
      ),
      selectInput(
        "recipe",
        vdltheme::input_label_vdl(
          "What makes Pokémon similar?",
          "Changes the relative influence of stats, types, breeding, and species traits."
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
        sliderInput("weight_types", "Types", 0, 3, 1, 0.25),
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
      selectInput(
        "highlight",
        vdltheme::input_label_vdl(
          "Spotlight primary type",
          "Emphasizes one type without recalculating or moving the projection."
        ),
        choices = c(
          "All types" = "all",
          stats::setNames(names(pokemon_type_colors), stringr::str_to_title(names(pokemon_type_colors)))
        ),
        selected = "all"
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
          tags$li("Types, egg groups, and species traits are one-hot encoded."),
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
              tags$th("Types"), tags$th("Egg groups"), tags$th("Species traits")
            )),
            tags$tbody(
              tags$tr(tags$th("Balanced"), tags$td("1"), tags$td("1"), tags$td("1"), tags$td("1"), tags$td("1")),
              tags$tr(tags$th("Battle & morphology"), tags$td("2"), tags$td("0.5"), tags$td("0.5"), tags$td("0.25"), tags$td("0.25")),
              tags$tr(tags$th("Pokémon types"), tags$td("0.4"), tags$td("0.4"), tags$td("2"), tags$td("0.4"), tags$td("0.4")),
              tags$tr(tags$th("Breeding & ecology"), tags$td("0.4"), tags$td("0.5"), tags$td("0.5"), tags$td("2"), tags$td("2"))
            )
          )
        ),
        tags$p(
          class = "small text-muted",
          "Custom weights use the five sliders directly. A weight of 0 removes that block from the distance."
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
      types = input$weight_types,
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

    if (identical(result$method, "pca")) {
      return(paste(
        recipe, paste(format(nrow(result$data), big.mark = ","), "Pokémon"),
        "linear baseline", sep = " · "
      ))
    }
    if (identical(result$method, "tsne")) {
      return(paste0(
        recipe, " · ", format(nrow(result$data), big.mark = ","), " Pokémon",
        " · perplexity ", result$perplexity,
        " · ", result$iterations, " iterations"
      ))
    }
    paste0(
      recipe, " · ", format(nrow(result$data), big.mark = ","), " Pokémon",
      " · ", result$n_neighbors, " neighbors · min dist ", result$min_dist
    )
  })

  output$embedding <- renderHighchart({
    xy <- projection()$xy
    selected <- projection()$data
    highlight <- input$highlight
    points <- make_points(selected, xy, highlight, input$sprite_size)
    halos <- make_halos(selected, xy, highlight, input$sprite_size)

    highchart() |>
      hc_chart(
        type = "scatter",
        zoomType = "xy",
        panning = list(enabled = TRUE, type = "xy"), panKey = "shift",
        backgroundColor = "transparent", animation = FALSE,
        spacing = list(18, 18, 18, 18)
      ) |>
      hc_title(text = NULL) |>
      hc_xAxis(visible = FALSE) |>
      hc_yAxis(visible = FALSE) |>
      hc_add_series(
        id = "type_halos", data = halos, name = "Primary type",
        turboThreshold = 0, showInLegend = FALSE,
        enableMouseTracking = FALSE, zIndex = 1
      ) |>
      hc_add_series(
        id = "pokemon", data = points, name = "Pokémon",
        turboThreshold = 0, showInLegend = FALSE, zIndex = 2,
        point = list(events = list(click = JS(
          "function () { Shiny.setInputValue('selected_pokemon_id', this.options.pokemon_id, {priority: 'event'}); }"
        )))
      ) |>
      hc_tooltip(
        useHTML = TRUE, outside = TRUE, borderWidth = 0,
        borderRadius = 14, shadow = TRUE, padding = 0,
        headerFormat = "", pointFormat = tooltip_html
      ) |>
      hc_plotOptions(series = list(
        animation = FALSE, cursor = "pointer",
        states = list(hover = list(
          halo = list(size = 34, opacity = 0.24), brightness = 0.08
        ))
      )) |>
      hc_credits(enabled = FALSE)
  })
}

shinyApp(ui, server)
