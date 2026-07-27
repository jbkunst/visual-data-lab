# packages ---------------------------------------------------------------
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(fs)
library(glue)
library(yaml)
library(cli)

# parameters -------------------------------------------------------------
chrome_path  <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
preview_port <- 8000

# helpers ----------------------------------------------------------------
value <- function(desc, name, default = "") {
  x <- desc[[name]]
  if (is.null(x) || is.na(x) || !nzchar(x)) x <- default

  x |>
    str_replace_all("[\r\n\t]+", " ") |>
    str_squish()
}

as_csv <- function(x) {
  x <- str_squish(x)
  if (!nzchar(x)) return(character())

  x |>
    str_split(",") |>
    pluck(1) |>
    str_squish() |>
    discard(~ !nzchar(.x))
}

screenshot_generate_and_copy <- function(app, slug) {
  screenshot <- path(app, "screenshot.png")

  if (!file_exists(screenshot)) {
    tryCatch(
      webshot2::appshot(app, file = screenshot, delay = 20, vwidth = 1440, vheight = 900),
      error = function(e) cli::cli_alert_warning("{app}: screenshot failed: {conditionMessage(e)}")
    )
  }

  image <- "site-assets/placeholder.svg"

  if (file_exists(screenshot)) {
    image <- path("site-assets", "screenshots", paste0(slug, ".png"))
    file_copy(screenshot, image, overwrite = TRUE)
  }

  chartr("\\", "/", image)
}

shinylive_export_catch <- function(meta) {
  cli::cli_h2(glue("Exporting Shinylive app: {meta$app}"))

  tryCatch(
    {
      shinylive::export(
        meta$app,
        "docs/live",
        subdir = meta$slug,
        template_params = list(title = meta$title)
      )

      index_file <- path("docs/live", meta$slug, "index.html")

      if (!file_exists(index_file)) {
        stop("Shinylive export completed, but index.html is missing.", call. = FALSE)
      }

      list(ok = TRUE, message = "Shinylive export completed.")
    },
    error = function(e) list(ok = FALSE, message = conditionMessage(e))
  )
}

# setup ------------------------------------------------------------------
cli::cli_h1("Setup")

if (file_exists("apps.yml")) file_delete("apps.yml")
if (dir_exists("docs")) dir_delete("docs")
if (dir_exists("site-assets/screenshots")) dir_delete("site-assets/screenshots")

dir_create(c("site-assets/screenshots", "docs", "docs/live"))
writeLines("", "docs/.nojekyll", useBytes = TRUE)

if (interactive()) {
  httpuv::runStaticServer("docs", port = preview_port, browse = FALSE, background = TRUE)
}

if (!file_exists("site-assets/placeholder.svg")) {
  writeLines(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1600 900"><rect width="1600" height="900" fill="#f5f7f8"/><text x="80" y="820" font-family="Arial" font-size="72" fill="#111315">Preview coming soon</text></svg>',
    "site-assets/placeholder.svg"
  )
}

# find apps ---------------------------------------------------------------
cli::cli_h1("Find apps")

app_dirs <- dir() |>
  keep(~ file_exists(path(.x, "DESCRIPTION"))) |>
  discard(~ .x %in% c("app-template", "docs")) |>
  discard(~ startsWith(.x, "."))

apps <- map_dfr(app_dirs, function(app) {
  desc <- read.dcf(path(app, "DESCRIPTION"))
  desc <- as.list(desc[1, , drop = TRUE])

  tibble(
    app = app,
    title = value(desc, "Title"),
    description = value(desc, "Description"),
    slug = app,
    categories = list(as_csv(value(desc, "Categories"))),
    runtime = str_to_lower(value(desc, "Runtime", "shinylive")),
    app_url = value(desc, "AppURL"),
    status = str_to_lower(value(desc, "Status"))
  )
})

draft_apps <- apps |>
  filter(.data$status == "draft")

if (nrow(draft_apps) > 0) {
  cli::cli_alert_info("Draft apps skipped: {paste(draft_apps$app, collapse = ', ')}")
  apps <- apps |>
    filter(.data$status != "draft")
}

if (nrow(apps) == 0) {
  stop("No app DESCRIPTION files found.", call. = FALSE)
}

metadata_errors <- apps |>
  mutate(
    missing = pmap_chr(
      list(.data$title, .data$description, .data$categories, .data$runtime, .data$app_url),
      function(title, description, categories, runtime, app_url) {
        missing <- c(
          if (!nzchar(title)) "Title",
          if (!nzchar(description)) "Description",
          if (length(categories) == 0) "Categories",
          if (!runtime %in% c("shinylive", "server")) "Runtime",
          if (identical(runtime, "server") && !nzchar(app_url)) "AppURL"
        )

        paste(missing, collapse = ", ")
      }
    )
  ) |>
  filter(nzchar(.data$missing))

if (nrow(metadata_errors) > 0) {
  stop(
    paste(glue("{metadata_errors$app}: missing or invalid {metadata_errors$missing}"), collapse = "\n"),
    call. = FALSE
  )
}

# shinylive ---------------------------------------------------------------
cli::cli_h1("Shinylive")

shinylive_apps <- apps |>
  filter(.data$runtime == "shinylive")

server_apps <- apps |>
  filter(.data$runtime == "server")

if (nrow(server_apps) > 0) {
  cli::cli_alert_info("Server apps skipped by Shinylive: {paste(server_apps$app, collapse = ', ')}")
}

shinylive_results <- shinylive_apps$app |>
  set_names() |>
  map(function(app) {
    meta <- shinylive_apps |>
      filter(.data$app == .env$app) |>
      slice(1)

    shinylive_export_catch(meta)
  })

shinylive_failed <- names(discard(shinylive_results, ~ .x$ok))

if (length(shinylive_failed) > 0) {
  messages <- map_chr(
    shinylive_failed,
    ~ glue("{.x}: {shinylive_results[[.x]]$message}")
  )

  stop(
    paste(c("Shinylive export failed:", messages), collapse = "\n"),
    call. = FALSE
  )
}

# cards ------------------------------------------------------------------
cli::cli_h1("Gallery cards")

cards <- apps$app |>
  set_names() |>
  map(function(app) {
    meta <- apps |>
      filter(.data$app == .env$app) |>
      slice(1)

    image <- screenshot_generate_and_copy(meta$app, meta$slug)
    launch_url <- if (meta$runtime == "shinylive") {
      glue("live/{meta$slug}/index.html")
    } else {
      meta$app_url
    }

    list(
      title = meta$title,
      description = meta$description,
      image = image,
      categories = meta$categories[[1]],
      path = as.character(launch_url)
    )
  })

write_yaml(unname(cards[sort(names(cards))]), "apps.yml")

# quarto -----------------------------------------------------------------
cli::cli_h1("Quarto")

quarto::quarto_render(".", quarto_args = "--no-clean")

if (interactive()) {
  browseURL(glue("http://127.0.0.1:{preview_port}/index.html"), browser = chrome_path)
}

# done -------------------------------------------------------------------
cli::cli_h1("Done")
cli::cli_alert_success("Built {nrow(apps)} apps: {nrow(shinylive_apps)} Shinylive, {nrow(server_apps)} server.")
message("Rendered Quarto site to docs/")
