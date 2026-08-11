\name{theme_vdl}
\alias{theme_vdl}
\alias{highcharter_theme_vdl}
\title{Visual Data Lab themes}
\usage{
theme_vdl(
  base_font = bslib::font_google("IBM Plex Sans"),
  tooltip_bg = "#f1f3f5",
  tooltip_color = "#343a40",
  ...
)

highcharter_theme_vdl(...)
}
\arguments{
  \item{base_font}{Base font passed to \code{bslib::bs_theme()}.}
  \item{tooltip_bg}{Background color for Bootstrap tooltips.}
  \item{tooltip_color}{Text color for Bootstrap tooltips.}
  \item{...}{Additional arguments passed to \code{bslib::bs_theme()} in
  \code{theme_vdl()}, or options merged into the Highcharts theme in
  \code{highcharter_theme_vdl()}.}
}
\value{
\code{theme_vdl()} returns a \code{bslib} theme. \code{highcharter_theme_vdl()}
returns a \code{highcharter} theme.
}
\description{
Shared Bootstrap and Highcharts themes for Visual Data Lab applications.
}
