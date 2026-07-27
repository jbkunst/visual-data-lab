# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(scales)
library(stringr)
library(tibble)
library(tidyr)
library(jpeg)
library(imager)
library(plotly)
library(markdown) # needed by htmltools::includeMarkdown in Shinylive
library(shinyWidgets)

# theme options -----------------------------------------------------------
thematic::thematic_shiny(font = "auto")

apptheme <- bs_theme()

sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))
 
# app options -------------------------------------------------------------
img_files <- dir("www/imgs/", full.names = TRUE)

img_choices <- setNames(
  img_files,
  str_to_title(tools::file_path_sans_ext(basename(img_files)))
)

scene_common <- list(
  xaxis = list(title = "R", range = c(0, 1)),
  yaxis = list(title = "G", range = c(0, 1)),
  zaxis = list(title = "B", range = c(0, 1)),
  aspectmode = "cube",
  dragmode = "orbit",
  camera = list(
    projection = list(type = "perspective")
  ),
  uirevision = "camera"
)

# helpers -----------------------------------------------------------------

sync_camera_js <- "
function(el, x, options) {
  function bindCameraSync() {
    if (typeof window.kmeansSyncPlotlyCamera === 'function') {
      window.kmeansSyncPlotlyCamera(el, x, options);
      return;
    }

    requestAnimationFrame(bindCameraSync);
  }

  bindCameraSync();
}
"

sync_plotly_camera <- function(plot, target, direction, ready_input) {
  htmlwidgets::onRender(
    plot,
    sync_camera_js,
    data = list(
      target = target,
      direction = direction,
      group = "kmeans_images_rgb",
      readyInput = ready_input
    )
  )
}

empty_scatter_data <- tibble(
  r = numeric(),
  g = numeric(),
  b = numeric(),
  color = character(),
  label = character()
)

base_rgb_scatter <- function() {
  plot_ly(
    data = empty_scatter_data,
    x = ~r,
    y = ~g,
    z = ~b,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      color = ~color,
      symbol = "circle"
    ),
    text = ~label
  ) |>
    layout(scene = scene_common, uirevision = "camera")
}

update_rgb_scatter <- function(output_id, data, color_col, label_col, session) {
  plotlyProxy(output_id, session, deferUntilFlush = TRUE) |>
    plotlyProxyInvoke(
      "restyle",
      list(
        x = list(data$r),
        y = list(data$g),
        z = list(data$b),
        text = list(as.character(data[[label_col]])),
        "marker.color" = list(as.character(data[[color_col]]))
      ),
      list(0)
    )
}

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  tags$head(
    htmltools::includeScript("www/camera-sync.js")
  ),
  tags$style(HTML(".selectize-dropdown-content{max-height:none!important;overflow-y:visible!important;}")),
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "K-means on images",
      withMathJax(),
      selectizeInput("image_file", tags$span("Image"), sample(img_choices), width = "100%"),
      sliderTextInput("k", tags$span("Parameter \\(k\\) for \\(K\\)-Means"), c(1:5, 10, 20, 50, 100, 500), selected = 5, grid = TRUE, force_edges = TRUE),
      checkboxInput("use_xy", tags$small("Use pixel position for clustering \\((r_i, g_i, b_i, x_i, y_i)\\)")),
      checkboxInput("scale", tags$small("Scale to \\([0, 1]\\) all columns before kmeans")),
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
      col_widths = 6,
      row_heights = 1,
      card(
        card_header("Image"),
        card_body(plotOutput("originalImage"))
        ),
      card(
        card_header(tags$small("Sample of pixels")),
        card_body(plotlyOutput("scatterplot3d"))
        ),
      card(
        card_header("Result Image"),
        card_body(plotOutput("resultImage"))
        ),
      card(
        card_header(tags$small("Clustered pixels")),
        card_body(plotlyOutput("scatterplot3dresults"))
        )
      )
    )
  )

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # input <- list(
  #   image_file = "imgs/chess.jpg",
  #   k = 4,
  #   use_xy = FALSE,
  #   scale = FALSE
  #   ); input
  # 
  # input <- list(
  #   image_file = sample(img_choices, 1),
  #   k = sample(1:10, size = 1),
  #   use_xy = TRUE,
  #   scale = FALSE
  #   ); input
  
  image <- reactive({
    
    # image <- readJPEG(input$image_file)
    
    image <- load.image(input$image_file)
    image
    
  })
  
  df_image <- reactive({
    
    # image <- image()
    
    image <- readJPEG(input$image_file)
    
    cli::cli_alert_info("processing image {input$image_file}")
    
    df_image <- map(1:3, function(i) image[,,i]) %>% 
      map(function(m) {
        # m <- matrix(round(runif(12), 2), nrow = 4)
        m |> 
          as.data.frame() |> 
          as_tibble() |> 
          mutate(y = row_number()) |> 
          gather(x, c, -y) |> 
          mutate(
            x = as.numeric(gsub("V", "", x)),
            y = rev(y)
          ) |> 
          select(x, y, c)
      }) %>% 
      reduce(left_join, by = c("x", "y")) %>% 
      rename(r = c.x, g = c.y, b = c) %>% 
      mutate(rgb = rgb(r, g, b)) 
    
    df_image
    
  })
  
  df_image_kmeans <- reactive({
    
    cli::cli_inform("running stats::kmeans")
    
    df_image <- df_image()
    
    df_image_input <- df_image |> 
      select(r, g, b, x, y)
    
    if(!input$use_xy) {
      df_image_input <- df_image_input |> 
        select(-x, -y)
    }
    
    if(input$scale){
      df_image_input <- df_image_input |> 
        mutate(across(everything(), .fns = ~ rescale(.x, to = c(0, 1))))
    }
    
    kMeans <- kmeans(df_image_input, centers = as.integer(input$k))
    
    df_image_kmeans <- df_image %>%
      mutate(
        rgb_app= rgb(kMeans$centers[kMeans$cluster, c("r", "g", "b")])
      )
    
    df_image_kmeans2 <- df_image_kmeans |>
      pull(rgb_app) |>
      # head() |>
      col2rgb() |>
      t() |>
      as_tibble() |>
      mutate(across(everything(), ~ ./255)) |>
      set_names(c("r", "g", "b")) |>
      rename_with(~ str_c(., "_app"), .cols = everything())
    
    df_image_kmeans <- bind_cols(df_image_kmeans, df_image_kmeans2)
    
    df_image_kmeans
    
  })
  
  output$originalImage <- renderPlot({
    image <- image()
    
    plot(image, axes = FALSE)
    
  })
  
  sample_pixels <- reactive({
    pixels <- df_image()
    
    pixels |>
      slice_sample(n = min(1000, nrow(pixels))) |>
      mutate(across(c(r, g, b), function(x) round(x, 2))) |>
      mutate(label = sprintf("rgb (%s, %s, %s)", r, g, b))
  })
  
  clustered_pixels <- reactive({
    pixels <- df_image_kmeans()
    
    pixels |>
      slice_sample(n = min(1000, nrow(pixels))) |>
      mutate(across(c(r, g, b), function(x) round(x, 2))) |>
      mutate(across(c(r_app, g_app, b_app), function(x) round(x, 2))) |>
      mutate(
        label = sprintf("rgb (%s, %s, %s)", r, g, b),
        label_app = sprintf("rgb (%s, %s, %s)", r_app, g_app, b_app),
        label_result = glue::glue("{label_app}\n{label}")
      )
  })
  
  output$scatterplot3d <- renderPlotly({
    base_rgb_scatter() |>
      sync_plotly_camera(
        target = "scatterplot3dresults",
        direction = "sample_to_clusters",
        ready_input = "scatterplot3d_ready"
      )
    
  })
  
  observe({
    req(input$scatterplot3d_ready)
    
    update_rgb_scatter(
      output_id = "scatterplot3d",
      data = sample_pixels(),
      color_col = "rgb",
      label_col = "label",
      session = session
    )
  })
  
  output$resultImage <- renderPlot({
    
    df_image_kmeans <- df_image_kmeans()
    image           <- image()
    
    daux <- df_image_kmeans |>
      mutate(y = max(y) - y + 1) |>
      filter(TRUE) |>
      mutate()
    
    image_result <- image
    
    image_result[,,,1] <- daux |> select(x, y, r_app) |> spread(y, r_app) |> select(-x) |> as.matrix()
    image_result[,,,2] <- daux |> select(x, y, g_app) |> spread(y, g_app) |> select(-x) |> as.matrix()
    image_result[,,,3] <- daux |> select(x, y, b_app) |> spread(y, b_app) |> select(-x) |> as.matrix()
    
    plot(image_result, axes = FALSE)
    
  })
  
  output$scatterplot3dresults <- renderPlotly({
    base_rgb_scatter() |>
      sync_plotly_camera(
        target = "scatterplot3d",
        direction = "clusters_to_sample",
        ready_input = "scatterplot3dresults_ready"
      )
    
  })
  
  observe({
    req(input$scatterplot3dresults_ready)
    
    update_rgb_scatter(
      output_id = "scatterplot3dresults",
      data = clustered_pixels(),
      color_col = "rgb_app",
      label_col = "label_result",
      session = session
    )
  })
  
}

shinyApp(ui, server)