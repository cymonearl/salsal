library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(DT)

# Read and clean data
data <- read.csv("wfp_food_prices_phl.csv", 
                 comment.char = "#",
                 strip.white = TRUE,
                 stringsAsFactors = FALSE) %>%
  rename_with(~ gsub("[.#+]", "", .x)) %>% 
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date, label = TRUE),
    across(c(latitude, longitude, price, usdprice), as.numeric)
  ) %>%
  # Add commodity categories
  mutate(
    category_group = case_when(
      grepl("Rice|Maize", commodity) ~ "Grains & Cereals",
      grepl("Meat|Chicken", commodity) ~ "Meat & Protein",
      grepl("Eggs", commodity) ~ "Eggs",
      grepl("Fish|Crab|Shrimp", commodity) ~ "Seafood",
      grepl(paste0("Banana|Bottle|Bitter|Mandarin|Pineapple|",
                  "Papaya|Tomato|Coconut|Choko|Calamansi|Mango"), 
            commodity) ~ "Fruits",
      grepl(paste0("Bean|Vegetable|Cabbage|Carrot|Choko|Eggplant|Anchovies|",
                  "Garlic|Ginger|Onion|Potato|Squash|Tomato|Water"),
            commodity, ignore.case = TRUE) ~ "Vegetables & Legumes",
      TRUE ~ "Other Food Items"
    )
  )

# CSS for dark mode and styling
css <- HTML("
  /* Dark mode styles */
  body.dark-mode { background-color: #2d2d2d; color: #dfdfdf; }
  body.dark-mode .content-wrapper,
  body.dark-mode .main-sidebar,
  body.dark-mode .main-header .navbar,
  body.dark-mode .main-header .logo {
    background-color: #2d2d2d !important;
    color: #dfdfdf !important;
  }
  body.dark-mode .box {
    background-color: #404040;
    border-color: #606060;
    color: #dfdfdf;
  }
  body.dark-mode .selectize-input {
    background-color: #555 !important;
    color: #dfdfdf !important;
    border-color: #666 !important;
  }
  body.dark-mode .selectize-dropdown-content {
     background-color: #555 !important;
     color: #dfdfdf !important;
  }
  body.dark-mode .selectize-dropdown .active {
    background-color: #6a6a6a !important;
  }
  body.dark-mode .sidebar-menu > li > a { color: #dfdfdf; }
  body.dark-mode .sidebar-menu > li.active > a,
  body.dark-mode .sidebar-menu > li > a:hover {
    background: #404040;
    color: #FFF;
    border-left-color: #FFF;
  }
  body.dark-mode .shiny-input-container label { color: #dfdfdf; }
  body.dark-mode .form-control:not(.selectize-input) {
    background-color: #555;
    color: #dfdfdf;
    border-color: #666;
  }
  body.dark-mode .input-group-addon {
    background-color: #555;
    color: #dfdfdf;
    border-color: #666;
  }
  body.dark-mode .dataTables_wrapper .dataTables_length,
  body.dark-mode .dataTables_wrapper .dataTables_filter,
  body.dark-mode .dataTables_wrapper .dataTables_info,
  body.dark-mode .dataTables_wrapper .dataTables_processing,
  body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button {
    color: #dfdfdf !important;
  }
  body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
    color: #888 !important;
  }
  body.dark-mode table.dataTable tbody tr {
    background-color: #404040;
  }
  body.dark-mode table.dataTable tbody tr:hover {
    background-color: #505050 !important;
  }
  body.dark-mode table.dataTable th, body.dark-mode table.dataTable td {
    color: #dfdfdf;
    border-color: #606060;
  }
  body.dark-mode table.dataTable thead th {
    border-bottom: 1px solid #606060;
  }
") 