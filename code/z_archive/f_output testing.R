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


# ------


# ----- NA diagnostics
# ANSI escape codes for red color and reset
RED <- "\033[31m"
RESET <- "\033[0m"

# Load necessary libraries (if not already loaded)
# install.packages("dplyr")
# install.packages("lubridate") # For easy date handling
library(dplyr)
library(lubridate)

# --- Assuming gdnat_final and era5_final are already loaded in your environment ---
# For demonstration purposes, let's create some dummy dataframes if they don't exist
# In your actual environment, you would skip this block and use your existing data.
if (!exists("gdnat_final")) {
  gdnat_final <- data.frame(
    poly_id_int = c(1, 1, 2, 2, 3, 3, 4),
    year = c(2000, 2000, 2000, 2001, 2000, 2001, 2000),
    month = c(1, 2, 1, 1, 1, 2, 3),
    value_gdnat = c(10, 12, 15, 18, 20, 22, 25)
  )
}

if (!exists("era5_final")) {
  era5_final <- data.frame(
    adm2_id = c(1, 1, 2, 2, 3, 5), # Note: ID 5 in era5_final, but not gdnat_final
    year = c(2000, 2000, 2000, 2001, 2000, 2000),
    month = c(1, 2, 1, 1, 1, 1), # Note: month 1 for ID 5, but no match in gdnat
    value_era5 = c(100, 105, 110, 115, 120, 130)
  )
}


cat(RED, "--- Date Alignment Checks ---", RESET, "\n")
# Ensure 'month' and 'year' are numeric for proper min/max calculation
gdnat_final$month <- as.numeric(gdnat_final$month)
gdnat_final$year <- as.numeric(gdnat_final$year)
era5_final$month <- as.numeric(era5_final$month)
era5_final$year <- as.numeric(era5_final$year)


# Print first and last year + month for gdnat_final
cat(RED, "\nGDNAT_FINAL Date Range:\n", RESET)
cat(RED, paste0("  First Date: ", min(gdnat_final$year), "-", min(gdnat_final$month[gdnat_final$year == min(gdnat_final$year)]), "\n"), RESET)
cat(RED, paste0("  Last Date:  ", max(gdnat_final$year), "-", max(gdnat_final$month[gdnat_final$year == max(gdnat_final$year)]), "\n"), RESET)

# Print first and last year + month for era5_final
cat(RED, "\nERA5_FINAL Date Range:\n", RESET)
cat(RED, paste0("  First Date: ", min(era5_final$year), "-", min(era5_final$month[era5_final$year == min(era5_final$year)]), "\n"), RESET)
cat(RED, paste0("  Last Date:  ", max(era5_final$year), "-", max(era5_final$month[era5_final$year == max(era5_final$year)]), "\n"), RESET)

# --- Percentage of Rows That Match ---
cat(RED, "\n--- Matching Row Percentage Check (via Inner Join) ---", RESET, "\n")

# Perform an inner join to find only the matching rows
matched_rows <- inner_join(
  gdnat_final,
  era5_final,
  by = c(
    "poly_id_int" = "adm2_id",
    "month",
    "year"
  )
)

# Calculate percentage of gdnat_final rows that found a match
percent_gdnat_matched <- (nrow(matched_rows) / nrow(gdnat_final)) * 100
cat(RED, paste0("Percentage of gdnat_final rows that found a match: ", round(percent_gdnat_matched, 2), "%\n"), RESET)

# Calculate percentage of era5_final rows that found a match
percent_era5_matched <- (nrow(matched_rows) / nrow(era5_final)) * 100
cat(RED, paste0("Percentage of era5_final rows that found a match: ", round(percent_era5_matched, 2), "%\n"), RESET)

# --- Other Quick Checks ---
cat(RED, "\n--- Other Quick Checks ---", RESET, "\n")

# Number of unique IDs in each dataset
cat(RED, paste0("Number of unique IDs in gdnat_final (poly_id_int): ", length(unique(gdnat_final$poly_id_int)), "\n"), RESET)
cat(RED, paste0("Number of unique IDs in era5_final (adm2_id): ", length(unique(era5_final$adm2_id)), "\n"), RESET)

# Total rows after full_join vs. sum of original rows
# Re-run your full_join to ensure it's the current state
merged_data_panel <- full_join(
  gdnat_final,
  era5_final,
  by = c(
    "poly_id_int" = "adm2_id",
    "month",
    "year"
  )
)

cat(RED, paste0("Number of rows in gdnat_final: ", nrow(gdnat_final), "\n"), RESET)
cat(RED, paste0("Number of rows in era5_final: ", nrow(era5_final), "\n"), RESET)
cat(RED, paste0("Number of rows in merged_data_panel (full_join): ", nrow(merged_data_panel), "\n"), RESET)
cat(RED, paste0("Sum of rows in original datasets: ", nrow(gdnat_final) + nrow(era5_final), "\n"), RESET)


# Check NAs in the merged dataset (overall and per column)
cat(RED, "\nNA summary for merged_data_panel:\n", RESET)
# print() output generally cannot be colored with ANSI codes directly.
print(colSums(is.na(merged_data_panel)))
cat(RED, paste0("Total NAs in merged_data_panel: ", sum(is.na(merged_data_panel)), "\n"), RESET)

# Display a sample of the merged data with NAs (first 10 rows)
cat(RED, "\nHead of merged_data_panel (showing NAs) - Note: Data frame output itself may not be red:\n", RESET)
# print() output generally cannot be colored with ANSI codes directly.
print(head(merged_data_panel, 10))

# To identify specific rows causing NAs from gdnat_final:
# These are rows from gdnat_final that didn't find a match in era5_final
cat(RED, "\nRows from gdnat_final that did NOT find a match in era5_final:\n", RESET)
gdnat_unmatched <- anti_join(gdnat_final, era5_final,
                             by = c("poly_id_int" = "adm2_id", "month", "year"))
print(gdnat_unmatched)

# To identify specific rows causing NAs from era5_final:
# These are rows from era5_final that did NOT find a match in gdnat_final
cat(RED, "\nRows from era5_final that did NOT find a match in gdnat_final:\n", RESET)
era5_unmatched <- anti_join(era5_final, gdnat_final,
                            by = c("adm2_id" = "poly_id_int", "month", "year"))
print(era5_unmatched)

cat(RED, "\nSummary of NAs per column in merged_data_panel:\n", RESET); print(colSums(is.na(merged_data_panel)))

cat(RED, "\n--- Percentage of Unique IDs Matching Across Datasets ---\n", RESET)

# Percentage of gdnat_final poly_id_int that have a match in era5_final
matched_gdnat_ids <- gdnat_final %>% semi_join(era5_final, by = c("poly_id_int" = "adm2_id", "month", "year"))
percent_gdnat_ids_matched <- (length(unique(matched_gdnat_ids$poly_id_int)) / length(unique(gdnat_final$poly_id_int))) * 100
cat(RED, paste0("Percentage of unique poly_id_int in gdnat_final that found a match in era5_final: ", round(percent_gdnat_ids_matched, 2), "%\n"), RESET)

# Percentage of era5_final adm2_id that have a match in gdnat_final
matched_era5_ids <- era5_final %>% semi_join(gdnat_final, by = c("adm2_id" = "poly_id_int", "month", "year"))
percent_era5_ids_matched <- (length(unique(matched_era5_ids$adm2_id)) / length(unique(era5_final$adm2_id))) * 100
cat(RED, paste0("Percentage of unique adm2_id in era5_final that found a match in gdnat_final: ", round(percent_era5_ids_matched, 2), "%\n"), RESET)







# ---- mapping 

# --- IMPORTANT: If you encounter "could not find function" or dependency errors (e.g., 'gtable' version mismatch) ---
# 1. RESTART YOUR R SESSION COMPLETELY (e.g., in RStudio: Session > Restart R).
# 2. After restarting, run the following installation commands to ensure all packages and their dependencies are up-to-date:
# install.packages("dplyr")
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("patchwork")
# 3. Then, proceed to run the rest of the script.

# ANSI escape codes for red color and reset (for console messages)
RED <- "\033[31m"
RESET <- "\033[0m"

# --- Re-run the identification of unmatched keys (from previous step, ensures data is fresh) ---
cat(RED, "\n--- Re-identifying Unmatched Keys for Map ---", RESET, "\n")
# Assuming gdnat_final and era5_final are available in your environment from previous runs

# Get unique poly_id_int from gdnat_final
gdnat_ids <- unique(gdnat_final$poly_id_int)
# Get unique adm2_id from era5_final
era5_ids <- unique(era5_final$adm2_id)

unmatched_gdnat_ids_list <- gdnat_final %>%
  anti_join(era5_final, by = c("poly_id_int" = "adm2_id")) %>%
  distinct(poly_id_int) %>%
  pull(poly_id_int)

unmatched_era5_ids_list <- era5_final %>%
  anti_join(gdnat_final, by = c("adm2_id" = "poly_id_int")) %>%
  distinct(adm2_id) %>%
  pull(adm2_id)

# Filter the shapefile to include only the unmatched IDs
# IMPORTANT: Adjust 'shapefile_id_column_name' if your shapefile's ID column is different!
shapefile_id_column_name <- "GEOID" # e.g., "GEOID", "ADM2_CODE", "FIPS", etc.
cat(RED, paste0("Using shapefile ID column: '", shapefile_id_column_name, "'\n"), RESET)

unmatched_gdnat_sf <- usa_shapefile %>%
  filter(!!sym(shapefile_id_column_name) %in% unmatched_gdnat_ids_list) %>%
  mutate(Source = "Unmatched from GDNAT")

unmatched_era5_sf <- usa_shapefile %>%
  filter(!!sym(shapefile_id_column_name) %in% unmatched_era5_ids_list) %>%
  mutate(Source = "Unmatched from ERA5")

unmatched_locations_sf <- bind_rows(unmatched_gdnat_sf, unmatched_era5_sf)


# --- Map Generation with Insets for Alaska and Hawaii ---
cat(RED, "\n--- Generating Optimized Map of Unmatched Keys ---\n", RESET)

# --- DEBUGGING TIP: Print shapefile column names to identify the correct state column ---
cat(RED, "\nAvailable columns in usa_shapefile:\n", RESET)
print(colnames(usa_shapefile))
cat(RED, "Please check the above list and ensure 'state_column_name' below matches the actual state identifier column in your shapefile (e.g., 'STUSPS', 'NAME', 'STATE_ABBR', 'ID_1').\n", RESET)

# IMPORTANT: Set this to the column in your shapefile that identifies the state
# Based on your `glimpse` output, `ID_1` appears to be the state FIPS code.
state_column_name <- "ID_1" # <--- ADJUSTED THIS LINE TO "ID_1"

if (nrow(unmatched_locations_sf) > 0) {

  # Separate states for plotting using FIPS codes for AK ("02") and HI ("15")
  contiguous_us <- usa_shapefile %>% filter(!(!!sym(state_column_name) %in% c("02", "15")))
  alaska <- usa_shapefile %>% filter(!!sym(state_column_name) == "02")
  hawaii <- usa_shapefile %>% filter(!!sym(state_column_name) == "15")

  # Filter unmatched locations for each region
  unmatched_contiguous <- unmatched_locations_sf %>% filter(!(!!sym(state_column_name) %in% c("02", "15")))
  unmatched_alaska <- unmatched_locations_sf %>% filter(!!sym(state_column_name) == "02")
  unmatched_hawaii <- unmatched_locations_sf %>% filter(!!sym(state_column_name) == "15")

  # Define color scale
  color_scale <- scale_fill_manual(values = c("Unmatched from GDNAT" = "red", "Unmatched from ERA5" = "blue"),
                                   name = "Unmatched Data Source")

  # 1. Plot for Contiguous US
  p_contiguous <- ggplot() +
    geom_sf(data = contiguous_us, fill = "lightgray", color = "white", linewidth = 0.2) +
    geom_sf(data = unmatched_contiguous, aes(fill = Source), color = "black", linewidth = 0.3) +
    color_scale +
    coord_sf(crs = st_crs(contiguous_us), expand = FALSE) + # Ensure a proper projection for contiguous US
    theme_minimal() +
    theme(
      plot.title = element_blank(), # Title will be on combined plot
      plot.subtitle = element_blank(),
      legend.position = "none", # Legend will be on combined plot
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )

  # 2. Plot for Alaska (inset)
  p_alaska <- ggplot() +
    geom_sf(data = alaska, fill = "lightgray", color = "white", linewidth = 0.2) +
    geom_sf(data = unmatched_alaska, aes(fill = Source), color = "black", linewidth = 0.3) +
    color_scale +
    # Adjust CRS for Alaska to make it look better in its own inset
    # A common Alaska projection is EPSG:3338 (Alaska Albers)
    # You might need to install `sfdata` for some projections or check your data's CRS
    coord_sf(crs = st_crs(alaska), expand = FALSE) + # Use Alaska's native CRS or another suitable one
    labs(title = "Alaska") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = unit(c(0,0,0,0), "cm")
    )

  # 3. Plot for Hawaii (inset)
  p_hawaii <- ggplot() +
    geom_sf(data = hawaii, fill = "lightgray", color = "white", linewidth = 0.2) +
    geom_sf(data = unmatched_hawaii, aes(fill = Source), color = "black", linewidth = 0.3) +
    color_scale +
    coord_sf(crs = st_crs(hawaii), expand = FALSE) + # Use Hawaii's native CRS or another suitable one
    labs(title = "Hawaii") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = unit(c(0,0,0,0), "cm")
    )

  # Combine plots using patchwork
  # Create the final combined map
  final_map <- p_contiguous +
    inset_element(p_alaska, left = 0, bottom = 0, right = 0.3, top = 0.3, align_to = 'panel') +
    inset_element(p_hawaii, left = 0.25, bottom = 0, right = 0.55, top = 0.2, align_to = 'panel') +
    plot_annotation(
      title = "Geographic Distribution of Unmatched Keys",
      subtitle = paste0("IDs not found in the other dataset (based on '", shapefile_id_column_name, "')"),
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50")
      )
    ) & theme(legend.position = "bottom") # Apply legend to the combined plot

  print(final_map)

  # --- Save the map ---
  cat(RED, "\n--- Saving the Map ---\n", RESET)
  # You can specify width, height, and dpi for better quality
  ggsave(
    filename = "unmatched_keys_usa_map.png", # or .pdf, .jpeg
    plot = final_map,
    width = 10, # Adjust width as needed
    height = 7, # Adjust height as needed
    dpi = 300 # Dots per inch for image quality
  )
  cat(RED, "Map saved as 'unmatched_keys_usa_map.png'\n", RESET)

} else {
  cat(RED, "No unmatched IDs found to plot on the map. No map will be generated or saved.\n", RESET)
}
