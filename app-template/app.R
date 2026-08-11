# packages ----------------------------------------------------------------
library(shiny)
library(bslib)

# theme -------------------------------------------------------------------
apptheme <- bs_theme()

sidebar <- purrr::partial(bslib::sidebar, width = 300)

card <- purrr::partial(bslib::card, full_screen = TRUE, wrapper = purrr::partial(bslib::card_body, padding = 0))

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme,
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "App title",
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
      col_widths = 12,
      gap = "0.75rem",
      card(
        card_header("Main view"),
        card_body(
          p("Replace this content with the app UI.")
        )
      )
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Add server logic here.
}

shinyApp(ui, server)
