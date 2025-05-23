# Load necessary libraries
library(shiny)
library(bslib) # For modern Bootstrap themes
library(dplyr)
library(tidyr) # For pivot_longer
library(readr) # For read_csv
library(sf) # For spatial data (st_read, st_transform)
library(leaflet) # For interactive maps
library(ggplot2)
library(plotly) # For interactive ggplotly plots
library(forecast) # For ARIMA, auto.arima
library(lubridate) # For date/time manipulation (if Year needs conversion)
library(DT) # For interactive tables


# --- Load Price Data ---
tryCatch({
  raw_price_data <- read_csv("consolidated_farm_prices_imputed.csv", show_col_types = FALSE)
}, error = function(e) {
  stop("Failed to load consolidated_farm_prices_imputed.csv. Ensure it's in the app directory. Error: ", e$message)
})

# Data Preprocessing for Price Data
price_data <- raw_price_data %>%
  mutate(
    Year = as.integer(Year), # Ensure Year is integer
    Farmgate = as.numeric(Farmgate),
    Wholesale = as.numeric(Wholesale),
    Retail = as.numeric(Retail)
  ) %>%
  pivot_longer(cols = c(Farmgate, Wholesale, Retail),
               names_to = "Price_Level",
               values_to = "Price") %>%
  filter(!is.na(Price)) # Remove rows where price became NA after coercion

# --- Load Spatial Data (GeoJSON) ---
# Path to your GeoJSON file. Assumes ph.json is in the app's root directory.
# If in a 'data' subdirectory, change to "data/ph.json"
geojson_path <- "ph.json" 

# This variable MUST match the property name in your GeoJSON that contains the region names.
# Based on your ph.json structure, this is "name".
region_col_in_geojson <- "name" 

regions_sf <- NULL # Initialize
region_col_for_join <- "AppRegionName" # New column to be created for joining

if (file.exists(geojson_path)) {
  tryCatch({
    loaded_sf <- st_read(geojson_path, quiet = TRUE)
    
    message("GeoJSON loaded successfully. Path: ", geojson_path)
    message("Available columns (properties) in GeoJSON: ", paste(names(loaded_sf), collapse=", "))
    
    # Ensure the GeoJSON has the expected region name column
    if (!(region_col_in_geojson %in% names(loaded_sf))) {
      stop(paste0("The specified 'region_col_in_geojson' ('", region_col_in_geojson, 
                  "') was NOT FOUND in the loaded GeoJSON. Please check the GeoJSON properties and update the variable in global.R. ",
                  "Available columns are: ", paste(names(loaded_sf), collapse=", ")))
    }
    
    regions_sf <- loaded_sf %>%
      st_transform(crs = 4326) # Ensure WGS84 CRS for Leaflet
    
  }, error = function(e) {
    warning(paste("Error loading or processing GeoJSON '", geojson_path, "': ", e$message))
    regions_sf <- NULL 
  })
} else {
  warning(paste("GeoJSON file NOT FOUND at expected path: '", geojson_path, 
                "'. Please check the path. Map functionality will be disabled."))
  regions_sf <- NULL 
}

# --- Define Mappings ---
# KEY = Name as it appears in the GeoJSON 'name' property (e.g., "Davao", "National Capital Region")
# VALUE = Name as it appears in your 'price_data$Region' column (e.g., "Davao Region", "Metro Manila")
# **ADJUST THESE MAPPINGS CAREFULLY TO MATCH YOUR DATA**
geojson_to_app_region_mapping <- c(
  "National Capital Region" = "Metro Manila",
  "Ilocos" = "Ilocos Region",
  "Cagayan Valley" = "Cagayan Valley",        
  "Central Luzon" = "Central Luzon",          
  "Bicol" = "Bicol Region",
  "Western Visayas" = "Western Visayas",      
  "Central Visayas" = "Central Visayas",      
  "Eastern Visayas" = "Eastern Visayas",      
  "Zamboanga Peninsula" = "Zamboanga Peninsula",
  "Northern Mindanao" = "Northern Mindanao",  
  "Davao" = "Davao Region",
  "Soccsksargen" = "SOCCSKSARGEN",            
  "Caraga" = "CARAGA",                        
  "Autonomous Region in Muslim Mindanao" = "BARMM", 
  "Cordillera Administrative Region" = "CAR",       
  "Calabarzon" = "CALABARZON",                
  "Mimaropa" = "MIMAROPA"                     
  # Add other mappings like "Region I" = "Ilocos Region" if your data/GeoJSON uses such names
)

# Modify regions_sf AFTER loading to add the 'AppRegionName' for joining
if (!is.null(regions_sf)) {
  if (region_col_in_geojson %in% names(regions_sf)) {
    regions_sf <- regions_sf %>%
      mutate(
        !!region_col_for_join := recode(!!sym(region_col_in_geojson), 
                                        !!!geojson_to_app_region_mapping, 
                                        .default = !!sym(region_col_in_geojson)) 
      )
    
    original_geojson_names <- unique(regions_sf[[region_col_in_geojson]])
    mapped_app_names <- unique(regions_sf[[region_col_for_join]])
    
    default_or_direct_match_regions <- intersect(original_geojson_names, mapped_app_names)
    
    truly_unmapped_or_self_mapped <- character(0)
    if (length(default_or_direct_match_regions) > 0) {
      is_explicitly_mapped_differently <- names(geojson_to_app_region_mapping)[names(geojson_to_app_region_mapping) != geojson_to_app_region_mapping]
      
      truly_unmapped_or_self_mapped <- setdiff(default_or_direct_match_regions, geojson_to_app_region_mapping[names(geojson_to_app_region_mapping) %in% is_explicitly_mapped_differently])
      truly_unmapped_or_self_mapped <- setdiff(truly_unmapped_or_self_mapped, names(geojson_to_app_region_mapping)[geojson_to_app_region_mapping == names(geojson_to_app_region_mapping)])
      
    }
    
    if(length(unique(truly_unmapped_or_self_mapped)) > 0) {
      message("Diagnostic: The following region names from GeoJSON (column '", region_col_in_geojson, 
              "') were not found as keys in 'geojson_to_app_region_mapping' for a different value and will use their original GeoJSON names for joining: ", 
              paste(unique(truly_unmapped_or_self_mapped), collapse=", "),
              ". Ensure these names directly match entries in 'price_data$Region' or add specific mappings.")
    }
  } else {
    warning(paste0("GeoJSON was loaded, but the specified 'region_col_in_geojson' ('", region_col_in_geojson, "') was NOT found in its properties. ",
                   "The '", region_col_for_join, "' column for joining could not be properly created."))
    region_col_for_join <- paste0("ERROR_COLUMN_", region_col_in_geojson) # Prevent bad joins
  }
}

# --- Define Global Variables / Constants (from price_data) ---
available_regions_from_data <- sort(unique(price_data$Region))
available_price_levels <- sort(unique(price_data$Price_Level))
min_year_from_data <- if(nrow(price_data) > 0) min(price_data$Year, na.rm = TRUE) else as.integer(format(Sys.Date(), "%Y")) - 10
max_year_from_data <- if(nrow(price_data) > 0) max(price_data$Year, na.rm = TRUE) else as.integer(format(Sys.Date(), "%Y"))
has_national_data <- "Philippines" %in% available_regions_from_data

# --- Diagnostic: Check for regions in price_data not covered by the mapping's *target* values ---
if(exists("price_data") && nrow(price_data) > 0 && exists("geojson_to_app_region_mapping") && !is.null(regions_sf)) {
  app_regions_in_data_for_check <- unique(price_data$Region)
  target_mapped_app_regions <- unique(unname(geojson_to_app_region_mapping))
  
  all_possible_join_names_from_sf_check <- character(0)
  if (region_col_in_geojson %in% names(regions_sf)) { # Check if original column exists
    # Names that would be kept by .default because they are not keys in the mapping
    default_kept_geojson_names <- unique(regions_sf[[region_col_in_geojson]][!regions_sf[[region_col_in_geojson]] %in% names(geojson_to_app_region_mapping)])
    all_possible_join_names_from_sf_check <- unique(c(target_mapped_app_regions, default_kept_geojson_names))
  } else {
    all_possible_join_names_from_sf_check <- target_mapped_app_regions
  }
  
  unmatched_data_regions <- setdiff(app_regions_in_data_for_check, all_possible_join_names_from_sf_check)
  unmatched_data_regions <- setdiff(unmatched_data_regions, "Philippines") 
  
  if(length(unmatched_data_regions) > 0) {
    warning("The following regions from 'price_data$Region' do not appear to be target values in 'geojson_to_app_region_mapping' AND do not directly match original GeoJSON names that weren't mapped (potential .default cases): ", 
            paste(unmatched_data_regions, collapse=", "), 
            ". These will likely not join with any GeoJSON region if their names are not identical to an unmapped GeoJSON region name.")
  }
}