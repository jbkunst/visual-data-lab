# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(igraph)
library(sigmajs)
library(vdltheme)

# theme options -----------------------------------------------------------
thematic::thematic_shiny(font = "auto")

apptheme <- theme_vdl()

sidebar <- purrr::partial(bslib::sidebar, width = 300)
card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

# app options -------------------------------------------------------------
N_MAX <- 1000
FORCE_DURATION <- 5000

NETWORK_HELP <- c(
  path = "A chain. Interior nodes are bridges, so removing one can split the network.",
  ring = "A closed chain. Every node has the same degree and there is no central hub.",
  star = "One hub connects all other nodes. The hub is highly central and structurally critical.",
  random = "Edges appear independently. Changing expected degree moves the graph from sparse to dense.",
  small_world = "Mostly local links plus a few shortcuts. It combines clustering with short paths.",
  scale_free = "New nodes prefer already connected nodes, producing a few hubs and many low-degree nodes.",
  communities = "Groups are planted before generating edges. This lets us compare known and detected communities."
)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# helpers -----------------------------------------------------------------
include_md <- function(path) {
  if (file.exists(path)) {
    htmltools::includeMarkdown(path)
  } else {
    NULL
  }
}

balanced_sizes <- function(n, groups) {
  sizes <- rep(n %/% groups, groups)
  remainder <- n %% groups

  if (remainder > 0) {
    sizes[seq_len(remainder)] <- sizes[seq_len(remainder)] + 1
  }

  sizes
}

adjusted_rand_index <- function(actual, estimated) {
  tab <- table(actual, estimated)
  total_pairs <- choose(sum(tab), 2)

  if (total_pairs == 0) {
    return(1)
  }

  index <- sum(choose(tab, 2))
  row_pairs <- sum(choose(rowSums(tab), 2))
  column_pairs <- sum(choose(colSums(tab), 2))
  expected <- row_pairs * column_pairs / total_pairs
  maximum <- 0.5 * (row_pairs + column_pairs)
  denominator <- maximum - expected

  if (abs(denominator) < .Machine$double.eps) {
    return(as.numeric(index == expected))
  }

  (index - expected) / denominator
}

network_reference <- function(type, n, parameters) {
  if (type == "path") {
    return(list(
      mean_degree = 2 * (n - 1) / n,
      clustering = 0,
      note = "Exact values for a path"
    ))
  }

  if (type == "ring") {
    return(list(
      mean_degree = 2,
      clustering = 0,
      note = "Exact values for a simple ring"
    ))
  }

  if (type == "star") {
    return(list(
      mean_degree = 2 * (n - 1) / n,
      clustering = 0,
      note = "Exact values for a star"
    ))
  }

  if (type == "random") {
    probability <- min(1, parameters$mean_degree / (n - 1))

    return(list(
      mean_degree = (n - 1) * probability,
      clustering = probability,
      note = "Expected values under the random graph model"
    ))
  }

  if (type == "small_world") {
    degree <- 2 * parameters$neighbours
    lattice_clustering <- if (degree <= 2) {
      0
    } else {
      3 * (degree - 2) / (4 * (degree - 1))
    }

    return(list(
      mean_degree = degree,
      clustering = lattice_clustering * (1 - parameters$rewiring)^3,
      note = "Degree is exact; clustering is a simple approximation"
    ))
  }

  if (type == "scale_free") {
    return(list(
      mean_degree = 2 * parameters$attachments,
      clustering = NA_real_,
      note = "Mean degree approaches twice the attachment parameter"
    ))
  }

  list(
    mean_degree = parameters$mean_degree,
    clustering = NA_real_,
    note = "Mean degree is the generation target"
  )
}

generate_network <- function(
  type,
  n,
  seed,
  mean_degree = 6,
  neighbours = 2,
  rewiring = 0.1,
  attachments = 2,
  groups = 4,
  community_strength = 0.85
) {
  set.seed(seed)

  true_group <- NULL
  neighbours <- min(neighbours, floor((n - 1) / 2))
  attachments <- min(attachments, n - 1)
  groups <- min(groups, n)

  parameters <- list(
    mean_degree = mean_degree,
    neighbours = neighbours,
    rewiring = rewiring,
    attachments = attachments,
    groups = groups,
    community_strength = community_strength
  )

  graph <- switch(
    type,
    path = make_ring(n, directed = FALSE, circular = FALSE),
    ring = make_ring(n, directed = FALSE, circular = TRUE),
    star = make_star(n, mode = "undirected", center = 1),
    random = {
      probability <- min(1, mean_degree / (n - 1))
      sample_gnp(n, p = probability, directed = FALSE, loops = FALSE)
    },
    small_world = {
      sample_smallworld(
        dim = 1,
        size = n,
        nei = neighbours,
        p = rewiring,
        loops = FALSE,
        multiple = FALSE
      )
    },
    scale_free = {
      sample_pa(
        n,
        power = 1,
        m = attachments,
        directed = FALSE
      )
    },
    communities = {
      sizes <- balanced_sizes(n, groups)
      average_size <- n / groups

      probability_in <- min(
        1,
        mean_degree * community_strength / max(average_size - 1, 1)
      )
      probability_out <- min(
        1,
        mean_degree * (1 - community_strength) / max(n - average_size, 1)
      )

      preferences <- matrix(probability_out, nrow = groups, ncol = groups)
      diag(preferences) <- probability_in
      true_group <- rep(seq_len(groups), times = sizes)

      sample_sbm(
        n,
        pref.matrix = preferences,
        block.sizes = sizes,
        directed = FALSE,
        loops = FALSE
      )
    }
  )

  graph <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)

  list(
    type = type,
    graph = graph,
    true_group = true_group,
    reference = network_reference(type, n, parameters)
  )
}

community_palette <- function(groups) {
  colors <- grDevices::hcl.colors(length(groups), palette = "Dark 3")
  stats::setNames(colors, groups)
}

analyse_network <- function(generated) {
  graph <- generated$graph
  n <- vcount(graph)
  degrees <- degree(graph)
  components_info <- components(graph)
  giant_component <- which.max(components_info$csize)
  giant_vertices <- which(components_info$membership == giant_component)
  giant_graph <- induced_subgraph(graph, giant_vertices)

  if (ecount(graph) == 0) {
    estimated_group <- seq_len(n)
    estimated_modularity <- 0
  } else {
    detected <- cluster_louvain(graph)
    estimated_group <- membership(detected)
    estimated_modularity <- modularity(detected)
  }

  mean_path <- if (vcount(giant_graph) > 1 && ecount(giant_graph) > 0) {
    mean_distance(giant_graph, directed = FALSE)
  } else {
    0
  }

  network_diameter <- if (vcount(giant_graph) > 1 && ecount(giant_graph) > 0) {
    diameter(giant_graph, directed = FALSE, weights = NA)
  } else {
    0
  }

  clustering <- transitivity(
    graph,
    type = "globalundirected",
    isolates = "zero"
  )

  local_clustering <- transitivity(
    graph,
    type = "localundirected",
    isolates = "zero"
  )

  betweenness_values <- betweenness(
    graph,
    directed = FALSE,
    normalized = TRUE
  )

  groups <- sort(unique(estimated_group))
  colors <- community_palette(groups)
  maximum_degree <- max(c(degrees, 1))

  nodes <- tibble(
    id = as.character(seq_len(n)),
    label = paste("Node", seq_len(n)),
    degree = as.numeric(degrees),
    betweenness = as.numeric(betweenness_values),
    clustering = as.numeric(local_clustering),
    estimated_group = as.integer(estimated_group),
    true_group = if (is.null(generated$true_group)) NA_integer_ else generated$true_group,
    size = 3 + 9 * sqrt(degree / maximum_degree),
    color = unname(colors[as.character(estimated_group)])
  )

  edges <- as_data_frame(graph, what = "edges") |>
    transmute(
      id = paste0("e", row_number()),
      source = as.character(from),
      target = as.character(to),
      color = "#D5DAE2",
      size = 0.5
    )

  recovery <- NULL

  if (!is.null(generated$true_group)) {
    recovery <- list(
      true_groups = length(unique(generated$true_group)),
      estimated_groups = length(unique(estimated_group)),
      adjusted_rand = adjusted_rand_index(
        generated$true_group,
        estimated_group
      ),
      true_modularity = if (ecount(graph) > 0) {
        modularity(graph, membership = generated$true_group)
      } else {
        0
      },
      estimated_modularity = estimated_modularity
    )
  }

  list(
    type = generated$type,
    graph = graph,
    nodes = nodes,
    edges = edges,
    reference = generated$reference,
    recovery = recovery,
    metrics = list(
      nodes = n,
      edges = ecount(graph),
      density = edge_density(graph, loops = FALSE),
      mean_degree = mean(degrees),
      components = components_info$no,
      giant_share = max(components_info$csize) / n,
      mean_path = mean_path,
      diameter = network_diameter,
      clustering = clustering,
      communities = length(unique(estimated_group)),
      modularity = estimated_modularity
    )
  )
}

extract_node_id <- function(event) {
  if (is.null(event)) {
    return(NULL)
  }

  if (is.atomic(event) && length(event) == 1) {
    return(as.character(event))
  }

  if (!is.null(event$id)) {
    return(as.character(event$id))
  }

  if (!is.null(event$node$id)) {
    return(as.character(event$node$id))
  }

  if (!is.null(event$data$node$id)) {
    return(as.character(event$data$node$id))
  }

  NULL
}

metric_row <- function(label, value) {
  div(
    class = "d-flex justify-content-between gap-3 py-1 border-bottom",
    tags$span(class = "text-muted", label),
    tags$strong(value)
  )
}

format_number <- function(x, digits = 2) {
  if (is.na(x)) {
    return("—")
  }

  format(round(x, digits), trim = TRUE, big.mark = ",", nsmall = digits)
}

format_percent <- function(x, digits = 1) {
  if (is.na(x)) {
    return("—")
  }

  paste0(round(100 * x, digits), "%")
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "Network Structures",

      selectInput(
        "structure",
        tags$small("Network structure"),
        choices = c(
          "Path" = "path",
          "Ring" = "ring",
          "Star" = "star",
          "Random graph" = "random",
          "Small-world" = "small_world",
          "Scale-free" = "scale_free",
          "Planted communities" = "communities"
        ),
        selected = "small_world"
      ),

      uiOutput("network_help"),

      radioButtons(
        "n",
        tags$small("Number of nodes"),
        choices = c(10, 50, 100, 500, N_MAX),
        selected = 100,
        inline = TRUE
      ),

      uiOutput("structure_parameters"),

      numericInput(
        "seed",
        tags$small("Random seed"),
        value = 123,
        min = 1,
        step = 1
      ),

      div(
        class = "d-flex flex-wrap gap-2",
        actionButton("generate", "Generate", class = "btn-primary btn-sm"),
        actionButton("start_layout", "Run layout", class = "btn-outline-primary btn-sm"),
        actionButton("stop_layout", "Stop", class = "btn-outline-secondary btn-sm")
      ),

      accordion(
        multiple = FALSE,
        open = FALSE,
        accordion_panel(
          "How it works",
          tags$small(include_md("readme.md"))
        )
      ),
      
      tags$small(include_md("credits.md"))
    ),

    layout_columns(
      col_widths = c(8, 4, 4, 4, 4),
      row_heights = c(3, 2),
      gap = "0.75rem",

      card(
        card_header(uiOutput("network_title")),
        sigmajsOutput("network", height = "100%")
      ),

      card(
        card_header("Selected node"),
        card_body(uiOutput("selected_node"))
      ),

      card(
        card_header("Network size"),
        card_body(uiOutput("size_metrics"))
      ),

      card(
        card_header("Connectivity"),
        card_body(uiOutput("connectivity_metrics"))
      ),

      card(
        card_header("Structure and recovery"),
        card_body(uiOutput("structure_metrics"))
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  output$network_help <- renderUI({
    tags$small(
      class = "text-muted",
      NETWORK_HELP[[input$structure]]
    )
  })

  output$structure_parameters <- renderUI({
    if (input$structure == "random") {
      return(
        sliderInput(
          "mean_degree",
          tags$small("Expected average degree"),
          min = 1,
          max = 50,
          value = 6,
          step = 1
        )
      )
    }

    if (input$structure == "small_world") {
      return(
        tagList(
          sliderInput(
            "neighbours",
            tags$small("Neighbours on each side"),
            min = 1,
            max = 15,
            value = 2,
            step = 1
          ),
          sliderInput(
            "rewiring",
            tags$small("Rewiring probability"),
            min = 0,
            max = 1,
            value = 0.1,
            step = 0.05
          )
        )
      )
    }

    if (input$structure == "scale_free") {
      return(
        sliderInput(
          "attachments",
          tags$small("Links created by each new node"),
          min = 1,
          max = 10,
          value = 2,
          step = 1
        )
      )
    }

    if (input$structure == "communities") {
      return(
        tagList(
          sliderInput(
            "groups",
            tags$small("Planted communities"),
            min = 2,
            max = 8,
            value = 4,
            step = 1
          ),
          sliderInput(
            "mean_degree",
            tags$small("Target average degree"),
            min = 2,
            max = 40,
            value = 10,
            step = 1
          ),
          sliderInput(
            "community_strength",
            tags$small("Share of links kept inside communities"),
            min = 0.5,
            max = 0.98,
            value = 0.85,
            step = 0.01
          )
        )
      )
    }

    NULL
  })

  selected_node_id <- reactiveVal(NULL)

  results <- eventReactive(input$generate, {
    selected_node_id(NULL)

    generated <- generate_network(
      type = input$structure,
      n = as.integer(input$n),
      seed = input$seed,
      mean_degree = input$mean_degree %||% 6,
      neighbours = input$neighbours %||% 2,
      rewiring = input$rewiring %||% 0.1,
      attachments = input$attachments %||% 2,
      groups = input$groups %||% 4,
      community_strength = input$community_strength %||% 0.85
    )

    analyse_network(generated)
  }, ignoreNULL = FALSE)

  output$network_title <- renderUI({
    result <- results()
    labels <- c(
      path = "Path",
      ring = "Ring",
      star = "Star",
      random = "Random graph",
      small_world = "Small-world",
      scale_free = "Scale-free",
      communities = "Planted communities"
    )

    tagList(
      labels[[result$type]],
      tags$small(class = "text-muted ms-2", "Node size represents degree")
    )
  })

  output$network <- renderSigmajs({
    result <- results()

    sigmajs(kill = FALSE) |>
      sg_nodes(
        result$nodes,
        id,
        label,
        size,
        color,
        degree,
        betweenness,
        clustering,
        estimated_group,
        true_group
      ) |>
      sg_edges(
        result$edges,
        id,
        source,
        target,
        color,
        size
      ) |>
      sg_settings(
        edgeColor = "default",
        defaultEdgeColor = "#D5DAE2",
        minNodeSize = 2,
        maxNodeSize = 14,
        minEdgeSize = 0.2,
        maxEdgeSize = 1.2,
        labelThreshold = 9,
        defaultLabelColor = "#343A40",
        enableEdgeHovering = FALSE,
        doubleClickEnabled = FALSE
      ) |>
      sg_drag_nodes() |>
      sg_neighbours(
        nodes = "#E9ECEF",
        edges = "#E9ECEF"
      ) |>
      sg_events(
        list(list(event = "clickNode", priority = "event"))
      ) |>
      sg_force(
        worker = TRUE,
        barnesHutOptimize = nrow(result$nodes) >= 250,
        strongGravityMode = FALSE,
        gravity = 1,
        scalingRatio = 3,
        slowDown = 1
      ) |>
      sg_force_stop(FORCE_DURATION)
  })

  observeEvent(input$network_click_node, {
    selected_node_id(extract_node_id(input$network_click_node))
  })

  observeEvent(input$start_layout, {
    result <- results()

    sigmajsProxy("network") |>
      sg_force_start_p(
        worker = TRUE,
        barnesHutOptimize = nrow(result$nodes) >= 250,
        gravity = 1,
        scalingRatio = 3,
        slowDown = 1
      )
  })

  observeEvent(input$stop_layout, {
    sigmajsProxy("network") |>
      sg_force_stop_p()
  })

  output$selected_node <- renderUI({
    result <- results()
    node_id <- selected_node_id()

    if (is.null(node_id)) {
      return(
        tags$small(
          class = "text-muted",
          "Click a node to highlight its neighbours and inspect its metrics."
        )
      )
    }

    node <- result$nodes |>
      filter(id == node_id)

    if (nrow(node) == 0) {
      return(NULL)
    }

    tagList(
      tags$h5(node$label),
      metric_row("Degree", format_number(node$degree, 0)),
      metric_row("Betweenness", format_number(node$betweenness, 3)),
      metric_row("Local clustering", format_number(node$clustering, 3)),
      metric_row("Detected community", format_number(node$estimated_group, 0)),
      if (!is.na(node$true_group)) {
        metric_row("Planted community", format_number(node$true_group, 0))
      }
    )
  })

  output$size_metrics <- renderUI({
    metrics <- results()$metrics

    tagList(
      metric_row("Nodes", format_number(metrics$nodes, 0)),
      metric_row("Edges", format_number(metrics$edges, 0)),
      metric_row("Density", format_percent(metrics$density, 2)),
      metric_row("Average degree", format_number(metrics$mean_degree, 2))
    )
  })

  output$connectivity_metrics <- renderUI({
    metrics <- results()$metrics

    tagList(
      metric_row("Components", format_number(metrics$components, 0)),
      metric_row("Largest component", format_percent(metrics$giant_share, 1)),
      metric_row("Mean shortest path", format_number(metrics$mean_path, 2)),
      metric_row("Diameter", format_number(metrics$diameter, 0))
    )
  })

  output$structure_metrics <- renderUI({
    result <- results()
    metrics <- result$metrics
    reference <- result$reference
    recovery <- result$recovery

    rows <- tagList(
      metric_row("Global clustering", format_number(metrics$clustering, 3)),
      metric_row("Reference clustering", format_number(reference$clustering, 3)),
      metric_row("Detected communities", format_number(metrics$communities, 0)),
      metric_row("Modularity", format_number(metrics$modularity, 3)),
      metric_row("Observed mean degree", format_number(metrics$mean_degree, 2)),
      metric_row("Reference mean degree", format_number(reference$mean_degree, 2))
    )

    if (!is.null(recovery)) {
      rows <- tagList(
        rows,
        tags$hr(),
        metric_row("Planted communities", format_number(recovery$true_groups, 0)),
        metric_row("Estimated communities", format_number(recovery$estimated_groups, 0)),
        metric_row("Adjusted Rand index", format_number(recovery$adjusted_rand, 3)),
        metric_row("Planted modularity", format_number(recovery$true_modularity, 3))
      )
    }

    tagList(
      rows,
      tags$small(class = "text-muted d-block mt-2", reference$note)
    )
  })
}

shinyApp(ui, server)
