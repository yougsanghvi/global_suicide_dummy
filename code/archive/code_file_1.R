if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(stars, tidyverse, BiocManager)
BiocManager::install("Rarr")

data_dir <- file.path(
  "/global",
  "scratch",
  "users",
  "cmolitor",
  "global_suicide"
)

gdnat_1 <- file.path(
  data_dir, 
  "climate_data",
  "gdnat_ACCESS-CM2_tas_1979-1999_v2025-02-11.zarr", 
  "tas"
)

data <- Rarr::zarr_overview(gdnat_1)

