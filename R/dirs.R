# DESCRIPTION ──────────────────────────────────────────────────────────────── #

# Functions and variable definitions related to project structure and directory
# management.

# PATH VARIABLES ───────────────────────────────────────────────────────────── #

dirs <- list()
dirs$root <- rprojroot::find_rstudio_root_file()
dirs$R <- file.path(dirs$root, "R")
dirs$plots <- file.path(dirs$root, "plots")
dirs$data <- file.path(dirs$root, "data")
dirs$output <- file.path(dirs$root, "output")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}