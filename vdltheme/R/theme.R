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
  base_font = bslib::font_google("IBM Plex Sans"),
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
    ".tooltip-inner { border: 1px solid #ced4da; box-shadow: 0 0.125rem 0.25rem rgba(0, 0, 0, 0.08); }"
  )
}

#' Visual Data Lab Highcharts theme
#'
#' @param ... Additional options merged into the Highcharts theme.
#'
#' @return A `highcharter` theme.
#' @export
highcharter_theme_vdl <- function(...) {
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
