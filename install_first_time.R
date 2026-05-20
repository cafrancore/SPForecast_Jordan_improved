# One-time setup for local users.
# Run this once in RStudio before starting the app.

if (is.null(getOption("repos")) || getOption("repos")[["CRAN"]] %in% c("", "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

project_lib <- file.path(getwd(), "r-lib")
if (!dir.exists(project_lib)) {
  dir.create(project_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(normalizePath(project_lib, winslash = "/"), .libPaths()))

runtime_pkgs <- c(
  "shiny", "shinydashboard", "shinyWidgets",
  "dplyr", "tidyr", "readxl", "DT",
  "leaflet", "sf", "terra", "tseries", "ggplot2", "vars"
)

optional_pkgs <- c("extrafont")

installed_now <- rownames(installed.packages(lib.loc = .libPaths()))
to_install <- setdiff(c(runtime_pkgs, optional_pkgs), installed_now)

if (length(to_install)) {
  message("Installing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}

if (Sys.info()[["sysname"]] == "Windows" && requireNamespace("extrafont", quietly = TRUE)) {
  try(extrafont::loadfonts(device = "win", quiet = TRUE), silent = TRUE)
}

message("Setup complete. You can now run source('run_app.R').")
