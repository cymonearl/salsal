library(tidyverse)
library(readxl)

PHILIPPINES <- read_excel("farmgate.xlsx", sheet = 1)
ILOCOS_REGION <- read_excel("farmgate.xlsx", sheet = 2)
CAR <- read_excel("farmgate.xlsx", sheet = 3)
CAGAYAN_VALLEY <- read_excel("farmgate.xlsx", sheet = 4)
CENTRAL_LUZON <- read_excel("farmgate.xlsx", sheet = 5)
CALABARZON <- read_excel("farmgate.xlsx", sheet = 6)
MIMAROPA <- read_excel("farmgate.xlsx", sheet = 7)
BICOL_REGION <- read_excel("farmgate.xlsx", sheet = 8)
WESTERN_VISAYAS <- read_excel("farmgate.xlsx", sheet = 9)
CENTRAL_VISAYAS <- read_excel("farmgate.xlsx", sheet = 10)
EASTERN_VISAYAS <- read_excel("farmgate.xlsx", sheet = 11)
NORTHERN_MINDANAO <- read_excel("farmgate.xlsx", sheet = 12)
ZAMBOANGA_PENINSULA <- read_excel("farmgate.xlsx", sheet = 13)
DAVAO_REGION <- read_excel("farmgate.xlsx", sheet = 14)
SOCCKSARGEN <- read_excel("farmgate.xlsx", sheet = 15)
CARAGA <- read_excel("farmgate.xlsx", sheet = 16)
BARMM <- read_excel("farmgate.xlsx", sheet = 17)
SOUTHERN_TAGALOG <- read_excel("farmgate.xlsx", sheet = 18)
METRO_MANILA <- read_excel("farmgate.xlsx", sheet = 19)

glimpse(PHILIPPINES)
glimpse(ILOCOS_REGION)
glimpse(CAR)
glimpse(CAGAYAN_VALLEY)
glimpse(CENTRAL_LUZON)
glimpse(CALABARZON)
glimpse(MIMAROPA)
glimpse(BICOL_REGION)
glimpse(WESTERN_VISAYAS)
glimpse(CENTRAL_LUZON)
glimpse(EASTERN_VISAYAS)
glimpse(NORTHERN_MINDANAO)
glimpse(ZAMBOANGA_PENINSULA)
glimpse(DAVAO_REGION)
glimpse(SOCCKSARGEN)
glimpse(CARAGA)
glimpse(BARMM)
glimpse(SOUTHERN_TAGALOG)
glimpse(METRO_MANILA)
glimpse(CENTRAL_LUZON)

pivot_and_combine_rice_prices <- function(data_frames_list, region_names) {
  require(tidyr)
  require(dplyr)
  require(purrr)
  
  # Check if the lengths of inputs match
  if(length(data_frames_list) != length(region_names)) {
    stop("The number of data frames must match the number of region names")
  }
  
  # Map through each data frame and its corresponding region name
  combined_data <- map2_dfr(data_frames_list, region_names, function(df, region) {
    df %>%
      mutate(Region = region) %>%
      pivot_longer(
        cols = -c(Year, Region),
        names_to = "Price_Type",
        values_to = "Price"
      )
  })
  
  return(combined_data)
}

region_data <- pivot_and_combine_rice_prices(
  list(BARMM, BICOL_REGION, CAGAYAN_VALLEY, CALABARZON, CAR, CARAGA, CENTRAL_LUZON,
       CENTRAL_VISAYAS, DAVAO_REGION, EASTERN_VISAYAS, ILOCOS_REGION, METRO_MANILA, MIMAROPA,
       NORTHERN_MINDANAO, SOCCKSARGEN, SOUTHERN_TAGALOG, WESTERN_VISAYAS, ZAMBOANGA_PENINSULA),
  c("BARMM","BICOL_REGION","CAGAYAN_VALLEY","CALABARZON","CAR","CARAGA","CENTRAL_LUZON",
    "CENTRAL_VISAYAS","DAVAO_REGION", "EASTERN_VISAYAS", "ILOCOS_REGION","METRO_MANILA","MIMAROPA",
    "NORTHERN_MINDANAO","SOCCKSARGEN", "SOUTHERN_TAGALOG", "WESTERN_VISAYAS", "ZAMBOANGA_PENINSULA")
)

sum(is.na(region_data))
sum(duplicated(region_data))

# Count of missing values
sum(is.na(data$Price))

# Summary of missing data by Price_Type
library(dplyr)

region_data %>%
  group_by(Price_Type) %>%
  summarize(missing = sum(is.na(Price)), total = n(), percent_missing = mean(is.na(Price)) * 100)

# Missing by Region
region_data %>%
  group_by(Region) %>%
  summarize(missing = sum(is.na(Price)), total = n(), percent_missing = mean(is.na(Price)) * 100)
