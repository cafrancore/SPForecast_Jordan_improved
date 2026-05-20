# ==== Bootstrap for a brand new computer ====

# 0) Set a stable CRAN repo
if (is.null(getOption("repos")) || getOption("repos")[["CRAN"]] %in% c("", "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# 1) Create/use a LOCAL library in the project (./r-lib)
project_lib <- file.path(getwd(), "r-lib")
if (!dir.exists(project_lib)) dir.create(project_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(normalizePath(project_lib, winslash = "/"), .libPaths()))

message("Using project library: ", .libPaths()[1])

# 2) Runtime package requirements (no install at app startup)
runtime_pkgs <- c(
  "shiny", "shinydashboard", "shinyWidgets",
  "dplyr", "tidyr", "readxl", "DT",
  "leaflet", "sf", "terra", "tseries", "ggplot2", "vars"
)

installed_now <- rownames(installed.packages(lib.loc = .libPaths()))
missing_pkgs <- setdiff(runtime_pkgs, installed_now)

if (length(missing_pkgs)) {
  stop(
    "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
    "\nRun source('install_first_time.R') in RStudio, then restart the app.",
    call. = FALSE
  )
}

invisible(lapply(runtime_pkgs, require, character.only = TRUE, quietly = TRUE))

# 3) Global options
options(scipen = 999, dplyr.summarise.inform = FALSE)

# 4) Fonts on Windows (optional)
if (Sys.info()[["sysname"]] == "Windows" && requireNamespace("extrafont", quietly = TRUE)) {
  try(extrafont::loadfonts(device = "win", quiet = TRUE), silent = TRUE)
}

# 5) Project folder structure
userLocation        <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
root_dir <- userLocation
inputLocation   <- file.path(root_dir, "Input")
dataLocation    <- file.path(root_dir, "Data")
Gislocation     <- file.path(inputLocation, "GIS")
dictionaryLocation <- file.path(inputLocation, "Dictionary")

# Create folders if they do not exist
for (p in c(inputLocation, dataLocation, Gislocation, dictionaryLocation)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# 6) Load the dictionary
dict_path <- file.path(dictionaryLocation, "DictionaryR.xlsx")
if (!file.exists(dict_path)) {
  stop("Dictionary not found at: ", dict_path,
       "\nPlease place 'DictionaryR.xlsx' in 'Input/Dictionary'.")
}
VarLabels <- as.matrix(readxl::read_excel(dict_path, sheet = "Dashboard", range = "A1:C180"))
VarLabels[is.na(VarLabels)] <- ""
VarLabels[,1] <- trimws(VarLabels[,1])  # your fix

# 7) Default language
language <- 1

message("Bootstrap complete. Runtime packages loaded, folders ready, dictionary loaded.")
