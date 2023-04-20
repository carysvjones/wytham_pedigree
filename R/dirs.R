# DESCRIPTION ──────────────────────────────────────────────────────────────── #

# Functions and variable definitions related to project structure and directory
# management.

# PATH VARIABLES ───────────────────────────────────────────────────────────── #

dirs <- list()
dirs$root <- rprojroot::find_rstudio_root_file()
dirs$R <- file.path(dirs$root, "R")
dirs$plots <- file.path(dirs$root, "plots")
dirs$data_raw <- file.path(dirs$root, "data_raw")
dirs$data_output <- file.path(dirs$root, "data_output")
dirs$model_output <- file.path(dirs$root, "model_output")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}