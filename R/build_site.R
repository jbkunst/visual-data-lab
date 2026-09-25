# This script is executed by GitHub Actions. Do not run it locally: it rebuilds
# generated outputs such as apps.yml and docs/.

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

site_config       <- read_yaml("_quarto.yml")
site_url          <- str_remove(site_config$website[["site-url"]], "/+$")
site_title        <- site_config$website$title
site_lang         <- site_config$lang
site_author       <- site_config[["author-meta"]]
ga_measurement_id <- site_config$website[["google-analytics"]]

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

html_attribute <- function(x) {
  x |>
    str_replace_all(fixed("&"), "&amp;") |>
    str_replace_all(fixed('"'), "&quot;") |>
    str_replace_all(fixed("<"), "&lt;") |>
    str_replace_all(fixed(">"), "&gt;")
}

page_metadata_inject <- function(index_file, meta) {
  html <- readLines(index_file, warn = FALSE, encoding = "UTF-8")
  head_end <- which(str_detect(html, fixed("</head>")))[1]

  if (is.na(head_end)) {
    stop("Shinylive index.html is missing </head>.", call. = FALSE)
  }

  canonical_url <- glue("{site_url}/live/{meta$shinylive_pool}/{meta$slug}/")
  image_url <- glue("{site_url}/site-assets/screenshots/{meta$slug}.png")
  title <- html_attribute(meta$title)
  description <- html_attribute(meta$description)
  image_alt <- html_attribute(meta$image_alt)

  metadata <- c(
    glue('<meta name="description" content="{description}">'),
    glue('<meta name="author" content="{html_attribute(site_author)}">'),
    glue('<link rel="canonical" href="{canonical_url}">'),
    '<meta property="og:type" content="website">',
    glue('<meta property="og:site_name" content="{html_attribute(site_title)}">'),
    glue('<meta property="og:title" content="{title}">'),
    glue('<meta property="og:description" content="{description}">'),
    glue('<meta property="og:url" content="{canonical_url}">'),
    glue('<meta property="og:image" content="{image_url}">'),
    glue('<meta property="og:image:alt" content="{image_alt}">'),
    '<meta name="twitter:card" content="summary_large_image">',
    glue('<meta name="twitter:title" content="{title}">'),
    glue('<meta name="twitter:description" content="{description}">'),
    glue('<meta name="twitter:image" content="{image_url}">'),
    glue('<meta name="twitter:image:alt" content="{image_alt}">')
  )

  if (!is.null(ga_measurement_id) && nzchar(ga_measurement_id)) {
    metadata <- c(
      metadata,
      glue('<script async src="https://www.googletagmanager.com/gtag/js?id={ga_measurement_id}"></script>'),
      "<script>",
      "  window.dataLayer = window.dataLayer || [];",
      "  function gtag(){dataLayer.push(arguments);}",
      "  gtag('js', new Date());",
      glue("  gtag('config', '{ga_measurement_id}');"),
      "</script>"
    )
  }

  html <- str_replace(
    html,
    regex('<html lang="[^"]*">'),
    glue('<html lang="{html_attribute(site_lang)}">')
  )
  html <- append(html, metadata, after = head_end - 1L)
  writeLines(html, index_file, useBytes = TRUE)

  invisible(TRUE)
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
  pool_dir <- path("docs/live", meta$shinylive_pool)
  cli::cli_h2(glue("Exporting Shinylive app: {meta$app} ({meta$shinylive_pool} pool)"))

  tryCatch(
    {
      dir_create(pool_dir)
      shinylive::export(
        meta$app,
        pool_dir,
        subdir = meta$slug,
        template_params = list(title = meta$title)
      )

      index_file <- path(pool_dir, meta$slug, "index.html")

      if (!file_exists(index_file)) {
        stop("Shinylive export completed, but index.html is missing.", call. = FALSE)
      }

      page_metadata_inject(index_file, meta)
      cli::cli_alert_success("Exported {meta$app}")
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
  keep(~ file_exists(path(.x, "app.R")) || file_exists(path(.x, "global.R"))) |>
  discard(~ .x %in% c("app-template", "docs")) |>
  discard(~ startsWith(.x, "."))

apps <- map_dfr(app_dirs, function(app) {
  desc <- read.dcf(path(app, "DESCRIPTION"))
  desc <- as.list(desc[1, , drop = TRUE])

  tibble(
    app = app,
    title = value(desc, "Title"),
    description = value(desc, "Description"),
    image_alt = value(desc, "ImageAlt"),
    slug = app,
    categories = list(as_csv(value(desc, "Categories"))),
    runtime = str_to_lower(value(desc, "Runtime", "shinylive")),
    shinylive_pool = value(desc, "ShinylivePool"),
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
      list(.data$title, .data$description, .data$image_alt, .data$categories, .data$runtime, .data$shinylive_pool, .data$app_url),
      function(title, description, image_alt, categories, runtime, shinylive_pool, app_url) {
        missing <- c(
          if (!nzchar(title)) "Title",
          if (!nzchar(description)) "Description",
          if (!nzchar(image_alt)) "ImageAlt",
          if (length(categories) == 0) "Categories",
          if (!runtime %in% c("shinylive", "server")) "Runtime",
          if (identical(runtime, "shinylive") && !str_detect(shinylive_pool, "^[a-z0-9][a-z0-9-]*$")) "ShinylivePool",
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
  filter(.data$runtime == "shinylive") |>
  arrange(.data$shinylive_pool, .data$app)

shinylive_pools <- shinylive_apps |>
  count(.data$shinylive_pool, name = "apps") |>
  arrange(.data$shinylive_pool)

cli::cli_alert_info(
  "Shinylive pools: {paste(glue('{shinylive_pools$shinylive_pool} ({shinylive_pools$apps})'), collapse = ', ')}"
)

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

shinylive_pool_stats <- shinylive_pools |>
  mutate(
    package_names = map(
      .data$shinylive_pool,
      ~ names(readRDS(path("docs/live", .x, "shinylive/webr/packages/metadata.rds")))
    ),
    packages = map_int(.data$package_names, length),
    bytes = map_dbl(
      .data$shinylive_pool,
      ~ sum(as.numeric(file_info(dir_ls(path("docs/live", .x), recurse = TRUE, type = "file"))$size))
    )
  )

shinylive_pool_stats |>
  pwalk(function(shinylive_pool, apps, package_names, packages, bytes) {
    cli::cli_alert_info(
      "{shinylive_pool} pool: {apps} apps, {packages} Wasm packages, {format(round(bytes / 1024^2, 1), trim = TRUE)} MiB"
    )
    cli::cli_text("Packages: {paste(package_names, collapse = ', ')}")
  })

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
      glue("live/{meta$shinylive_pool}/{meta$slug}/index.html")
    } else {
      meta$app_url
    }

    list(
      title = meta$title,
      description = meta$description,
      image = image,
      `image-alt` = meta$image_alt,
      # Keep editorial categories in DESCRIPTION focused on subject matter.
      # Server apps receive one generated tag so the gallery can filter the
      # faster-to-demo Connect deployments without labeling every browser app.
      categories = unique(c(
        meta$categories[[1]],
        if (meta$runtime == "server") "runtime-server"
      )),
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
