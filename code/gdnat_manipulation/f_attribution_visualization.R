# ----- I. Libraries and File Paths ------------
#' Helper function to install and load CRAN packages if they are missing
#'
#' @param pkg Character string: The name of the package to install and load.
#' @param ... Additional arguments passed to `install.packages()`.
install_if_missing <- function(pkg, ...) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(
            pkg,
            repos = "https://cloud.r-project.org",
            ...
        )
    }
    library(pkg, character.only = TRUE)
}

install_if_missing("dplyr")

# Set file paths
dir_path <- "/global/scratch/users/yougsanghvi"
attribution_output_folderpath <- file.path(
    dir_path,
    "gdnat_era5_compare_output"
)
attribution_output_filename <- "merged_data_panel_extended.csv"
attribution_output_filepath <- file.path(
    attribution_output_folderpath,
    attribution_output_filename
)

# load files
attribution_output_data <- read.csv(attribution_output_filepath)

attribution_output_data <- attribution_output_data %>%
    mutate(diff_suicide = y_hat_era5 - y_hat_gdnat)
