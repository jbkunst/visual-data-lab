# Projection and profile helpers -------------------------------------------
#
# These functions are sourced by app.R after data and visual constants exist.
# They remain app-specific: the goal is to keep the reactive flow readable,
# not to create a reusable framework around unrelated apps.
# Used in chart metadata and the fullscreen profile. Keeping the display-name
# mapping here avoids leaking UI labels into projection calculations.
method_label <- function(method) {
  switch(
    method,
    pca = "PCA",
    tsne = "t-SNE",
    umap = "UMAP",
    method
  )
}

# Used only by projection() in server. Every method receives the same weighted
# feature matrix, so changes between PCA, t-SNE, and UMAP reflect the reduction
# method rather than different preprocessing. clean_coordinates() deliberately
# removes attributes that previously made PCA and UMAP serialize incorrectly.
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

# Used by output$embedding. It converts one row per Pokémon into explicit
# Highcharts point options, including the stable ID sent back to Shiny on click.
# Sprite sizes change presentation only; they never alter projected coordinates.
make_points <- function(data, xy, sprite_size = 28) {
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
      id, x, y, pokemon_label, type_1_label, type_2_label,
      generation_label, type_color, height_m, weight_kg,
      hp, attack, defense, special_attack, special_defense, speed,
      capture_rate, base_happiness, hatch_counter,
      growth_rate_label, habitat_label, special_status,
      sprite_url, artwork_url
    ) |>
    purrr::pmap(function(
      id, x, y, pokemon_label, type_1_label, type_2_label,
      generation_label, type_color, height_m, weight_kg,
      hp, attack, defense, special_attack, special_defense, speed,
      capture_rate, base_happiness, hatch_counter,
      growth_rate_label, habitat_label, special_status,
      sprite_url, artwork_url
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
        marker = list(
          symbol = sprintf("url(%s)", sprite_url),
          width = sprite_size, height = sprite_size
        )
      )
    })
}

# Used below the sprites in each primary-type series. Halos provide a subtle
# color field while remaining non-interactive, so sprites own hover and click.
make_halos <- function(data, xy, sprite_size = 28) {
  halo_colors <- vapply(data$type_color, function(color) {
    rgb <- grDevices::col2rgb(color)
    sprintf("rgba(%d,%d,%d,0.18)", rgb[1], rgb[2], rgb[3])
  }, character(1))

  purrr::pmap(
    list(xy[, 1], xy[, 2], halo_colors),
    function(x, y, color) {
      list(
        x = as.numeric(x), y = as.numeric(y),
        color = color,
        marker = list(symbol = "circle", radius = (sprite_size + 12) / 2)
      )
    }
  )
}

# Used once when Highcharts loads. Highcharts exposes legend click but no native
# legend-hover callback, so this binds mouse entry/exit to each rendered legend
# item. Point hover remains untouched for tooltips; only the legend changes type
# focus, and its existing click-to-toggle behavior is preserved.
legend_type_focus_js <- htmlwidgets::JS(
  "function () {
    var chart = this;

    function setFocus(typeKey) {
      chart.series.forEach(function (series) {
        var custom = series.options.custom || {};
        var sameType = custom.typeKey === typeKey;
        var opacity = custom.isHalo ? (sameType ? 1 : 0) : (sameType ? 1 : 0.12);
        var group = series.markerGroup || series.group;
        if (group) {
          group.attr({ opacity: opacity });
          if (sameType) group.toFront();
        }
      });
    }

    function resetFocus() {
      chart.series.forEach(function (series) {
        var custom = series.options.custom || {};
        var group = series.markerGroup || series.group;
        if (group) group.attr({ opacity: custom.isHalo ? 0 : 1 });
      });
    }

    chart.series.forEach(function (series) {
      if (!series.options.showInLegend) return;
      var legendGroup = series.legendItem && series.legendItem.group
        ? series.legendItem.group
        : series.legendGroup;
      var element = legendGroup && legendGroup.element;
      if (!element || element.__pokemonTypeFocusBound) return;

      element.__pokemonTypeFocusBound = true;
      element.addEventListener('mouseenter', function () {
        setFocus(series.options.custom.typeKey);
      });
      element.addEventListener('mouseleave', resetFocus);
    });
  }"
)

# Used by output$embedding to create one visible legend item per primary type.
# Coordinates stay fixed: legend interaction only changes SVG group opacity.
make_type_series <- function(data, xy, sprite_size = 28) {
  points_by_type <- split(make_points(data, xy, sprite_size), data$type_1)
  halos_by_type <- split(make_halos(data, xy, sprite_size), data$type_1)

  sort(names(points_by_type)) |>
    purrr::map(function(type_key) {
      type_name <- pokemon_label(type_key)
      sprite_id <- paste0("type-", type_key)

      list(
        list(
          data = halos_by_type[[type_key]],
          name = paste(type_name, "halo"), linkedTo = sprite_id,
          turboThreshold = 0, showInLegend = FALSE,
          enableMouseTracking = FALSE,
          opacity = 0,
          states = list(
            inactive = list(opacity = 0),
            hover = list(opacity = 0)
          ),
          custom = list(typeKey = type_key, isHalo = TRUE), zIndex = 1
        ),
        list(
          data = points_by_type[[type_key]],
          id = sprite_id, name = type_name,
          color = unname(pokemon_type_colors[[type_key]]),
          marker = list(symbol = "circle", radius = 5),
          turboThreshold = 0, showInLegend = TRUE,
          custom = list(typeKey = type_key, isHalo = FALSE),
          zIndex = 2
        )
      )
    }) |>
    purrr::list_flatten()
}

# Used throughout the tooltip and fullscreen profile to turn API identifiers
# such as "medium-slow" into compact human-readable labels.
pokemon_label <- function(x) {
  stringr::str_to_title(stringr::str_replace_all(x, "-", " "))
}

# Used in the profile hero and neighbor cards. Colors come from the same type
# palette as the map so a type keeps one visual meaning across every view.
type_badge <- function(type) {
  if (is.na(type) || identical(type, "none")) return(NULL)
  color <- pokemon_type_colors[[type]]
  tags$span(
    class = "pokemon-profile-type",
    style = sprintf("background:%s", color),
    pokemon_label(type)
  )
}

# Used in the left column of the fullscreen profile. This small helper keeps
# repeated fact rows structurally and accessibly consistent.
profile_fact <- function(label, value) {
  div(
    class = "pokemon-profile-fact",
    tags$span(class = "pokemon-profile-fact-label", label),
    tags$strong(value)
  )
}

# Used for each numeric panel in pokemon_profile_modal(). Populations above 100
# observations are represented by 100 quantiles instead of one bar per Pokémon.
# The manual SVG is intentional: it is much lighter than creating twelve
# Highcharts widgets and makes the selected value and percentile deterministic.
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

# Used by pokemon_profile_modal(). Neighbors are measured in the current weighted
# feature matrix before dimensionality reduction; using screen coordinates would
# make t-SNE or UMAP distortions look like genuine similarity.
nearest_pokemon <- function(data, x, selected_index, n = 5L) {
  difference <- sweep(x, 2, x[selected_index, ], FUN = "-")
  distance <- sqrt(rowSums(difference^2))
  candidates <- order(distance)
  candidates <- candidates[candidates != selected_index][seq_len(min(n, nrow(data) - 1L))]
  data[candidates, , drop = FALSE] |>
    mutate(distance = distance[candidates])
}

# Used to render each nearest-neighbor result at the bottom of the profile.
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

# Used only for Pokémon profiles. modalDialog() has no direct fullscreen argument
# in this API, so tagQuery adds Bootstrap's modal-fullscreen class while retaining
# Shiny's normal modal lifecycle, Escape handling, and removeModal behavior.
fullscreen_modal <- function(...) {
  dialog <- modalDialog(..., title = NULL, footer = NULL, easyClose = TRUE, fade = TRUE)
  query <- htmltools::tagQuery(dialog)
  query$addClass("pokemon-profile-shell")
  query$find(".modal-dialog")$addClass("modal-fullscreen")$allTags()
}

# Called by observeEvent(input$selected_pokemon_id). It assembles the hero,
# descriptive facts, relative-position panels, and feature-space neighbors from
# the same projection result that was visible when the user clicked.
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
  metric_charts <- purrr::map(metrics, function(metric) {
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
        purrr::map(seq_len(nrow(neighbors)), function(i) {
          neighbor_card(neighbors[i, , drop = FALSE])
        })
      )
    )
  ))
}

# Used by hc_tooltip(pointFormat = ...). Highcharts interpolates point fields in
# the browser, avoiding a Shiny round trip for ordinary hover interactions.
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
