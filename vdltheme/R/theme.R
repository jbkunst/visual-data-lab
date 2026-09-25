#' Visual Data Lab Bootstrap theme
#'
#' @param base_font Base font passed to [bslib::bs_theme()].
#' @param tooltip_bg Background color for Bootstrap tooltips.
#' @param tooltip_color Text color for Bootstrap tooltips.
#' @param ... Additional arguments passed to [bslib::bs_theme()].
#'
#' @return A `bslib` theme.
#' @export
theme_vdl <- function(
  base_font = font_vdl(),
  tooltip_bg = "#f1f3f5",
  tooltip_color = "#343a40",
  ...
) {
  theme <- bslib::bs_theme(
    base_font = base_font,
    "tooltip-bg" = tooltip_bg,
    "tooltip-color" = tooltip_color,
    "tooltip-opacity" = 1,
    ...
  )

  bslib::bs_add_rules(
    theme,
    paste(
      ".tooltip-inner { border: 1px solid #ced4da; box-shadow: 0 0.125rem 0.25rem rgba(0, 0, 0, 0.08); }",
      ".bs-tooltip-top .tooltip-arrow::before, .bs-tooltip-auto[data-popper-placement^='top'] .tooltip-arrow::before { filter: drop-shadow(0 1px 0 #ced4da); }",
      ".bs-tooltip-end .tooltip-arrow::before, .bs-tooltip-auto[data-popper-placement^='right'] .tooltip-arrow::before { filter: drop-shadow(-1px 0 0 #ced4da); }",
      ".bs-tooltip-bottom .tooltip-arrow::before, .bs-tooltip-auto[data-popper-placement^='bottom'] .tooltip-arrow::before { filter: drop-shadow(0 -1px 0 #ced4da); }",
      ".bs-tooltip-start .tooltip-arrow::before, .bs-tooltip-auto[data-popper-placement^='left'] .tooltip-arrow::before { filter: drop-shadow(1px 0 0 #ced4da); }",
      sep = "\n"
    )
  )
}

font_vdl <- function() {
  fonts_dir <- system.file("fonts", package = "vdltheme")

  font <- bslib::font_collection("IBM Plex Sans")
  font$html_deps <- htmltools::tagFunction(function() {
    htmltools::htmlDependency(
      name = "ibm-plex-sans-vdl",
      version = "0.0.2",
      src = c(file = fonts_dir),
      stylesheet = "fonts.css"
    )
  })

  font
}

#' Visual Data Lab Highcharts theme
#'
#' @param ... Additional options merged into the Highcharts theme.
#'
#' @return A `highcharter` theme.
#' @export
highcharter_theme_vdl <- function(...) {
  if (!requireNamespace("highcharter", quietly = TRUE)) {
    stop("Package 'highcharter' is required for highcharter_theme_vdl().", call. = FALSE)
  }

  theme <- theme_vdl()
  defaults <- highcharter::hc_theme(
    chart = list(style = list(fontFamily = "IBM Plex Sans, sans-serif")),
    legend = list(itemStyle = list(fontWeight = "normal")),
    colors = unname(bslib::bs_get_variables(
      theme,
      c("primary", "danger", "warning", "success", "info", "secondary")
    )),
    tooltip = list(
      valueDecimals = 3,
      shared = TRUE,
      backgroundColor = "#f1f3f5",
      borderColor = "#ced4da",
      borderWidth = 1,
      style = list(color = "#343a40")
    ),
    xAxis = list(gridLineWidth = 1),
    plotOptions = list(
      spline = list(marker = list(enabled = FALSE, symbol = "circle")),
      line = list(marker = list(enabled = FALSE, symbol = "circle")),
      scatter = list(marker = list(symbol = "circle"))
    )
  )

  if (!length(list(...))) return(defaults)

  highcharter::hc_theme_merge(defaults, highcharter::hc_theme(...))
}
