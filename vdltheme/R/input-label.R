#' Input label with contextual help
#'
#' @param label Label text or HTML content.
#' @param help Short explanation shown in a tooltip.
#' @param placement Bootstrap tooltip placement.
#'
#' @return An HTML label suitable for a Shiny input.
#' @export
input_label_vdl <- function(label, help, placement = "right") {
  icon <- htmltools::tags$span(
    bsicons::bs_icon(
      "info-circle", size = "0.75em", a11y = "none"
    ),
    class = "ms-1",
    style = "display:inline-flex;align-items:center;vertical-align:0.03em;color:#6c757d;",
    tabindex = "0"
  )

  htmltools::tags$small(htmltools::tagList(
    label,
    bslib::tooltip(
      icon,
      help,
      placement = placement,
      options = list(trigger = "hover focus click")
    )
  ))
}
