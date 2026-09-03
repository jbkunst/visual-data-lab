run_app <- function(app = "kmeans") {
  project_name <- "Visual Data Lab"
  repo_slug <- "visual-data-lab"
  url <- paste0("https://github.com/jbkunst/", repo_slug, "/archive/refs/heads/main.zip")

  filePath <- tempfile("shinyapp", fileext = ".zip")
  fileDir  <- tempfile("shinyapp")

  cli::cli_inform("Downloading {project_name} app '{app}' from {url}")

  download.file(url, filePath)

  try(utils::unzip(filePath, exdir = fileDir))

  fp <- file.path(fileDir, paste0(repo_slug, "-main"), app)

  shiny::runApp(fp)

  # app_rmd <- stringr::str_subset(dir(fp, full.names = TRUE), "app.Rmd")
  # rmarkdown::run(app_rmd)
}

# run_app("kmeans")
