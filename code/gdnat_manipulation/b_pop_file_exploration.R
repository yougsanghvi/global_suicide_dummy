# Please load r-spatial using 'module-load r-spatial' before running this code

# ------ 1. Library Management ------

#' Helper function to install and load CRAN packages if they are missing
#'
#' @param pkg Character string: The name of the package to install and load.
#' @param ... Additional arguments passed to `install.packages()`.
install_if_missing <- function(pkg, ...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(
      pkg,
      repos = "[https://cloud.r-project.org](https://cloud.r-project.org)",
      ...
    )
  }
  library(pkg, character.only = TRUE)
}

# Install and load CRAN packages essential for spatial data handling and manipulation.
install_if_missing("dplyr")

# Install `stagg` from GitHub if it's not already installed.
# `remotes` is required to install packages directly from GitHub.
if (!requireNamespace("stagg", quietly = TRUE)) {
  install_if_missing("remotes")
  remotes::install_github("tcarleton/stagg")
}
library(stagg) # Load the stagg package

# ------ 2. Population file exploration ------

print(pop_world_2015_era5)
any(is.na(pop_world_2015_era5))

