library(tidyverse)

markets <- read_csv("wfp_markets_phl.csv")

glimpse(markets)
markets <- markets[-1, ]

markets |>
  group_by(market) |>
  summarize(missing = sum(is.na(markets)))
sum(is.na(markets))
sum(duplicated(markets))

# Plotting
markets$latitude <- as.numeric(markets$latitude)
markets$longitude <- as.numeric(markets$longitude)

ggplot(markets, aes(x = longitude, y = latitude)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Market Locations in the Philippines",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Install/load maps package if needed
if (!require(maps)) install.packages("maps")
library(maps)

# Philippines map background
map("world", regions = "Philippines", fill = TRUE, col = "lightgray", bg = "white", lwd = 0.5)

# Add market points
points(markets$longitude, markets$latitude, col = "red", pch = 19)


################################################################################

fpphl <- read_csv("wfp_food_prices_phl.csv")

fpphl <- fpphl[-1, ]
glimpse(fpphl)

sum(is.na(fpphl))
sum(duplicated(fpphl))

fpphl$latitude <- as.numeric(fpphl$latitude)
fpphl$longitude <- as.numeric(fpphl$longitude)
fpphl$price <- as.numeric(fpphl$price)

# Get latest date
latest_date <- max(as.Date(fpphl$date))

# Filter data
rice_data <- fpphl %>%
  filter(commodity == "Rice (milled, superior)",
         pricetype == "Retail",
         as.Date(date) == latest_date)

# Basic Philippines map
ph_map <- map_data("world", region = "Philippines")

ggplot() +
  # Base map
  geom_polygon(data = ph_map, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray60") +
  
  # Add market price points
  geom_point(data = rice_data, 
             aes(x = longitude, y = latitude, color = price, size = price), 
             alpha = 0.8) +
  
  scale_color_viridis_c(option = "plasma", name = "Price (PHP)") +
  scale_size_continuous(name = "Price (PHP)") +
  
  labs(title = "Retail Price of Rice (Milled, Superior)",
       subtitle = paste("As of", latest_date),
       x = "Longitude", y = "Latitude") +
  
  theme_minimal()

install.packages("leaflet")
library(leaflet)

leaflet(rice_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    radius = ~sqrt(price),  # size by price
    color = "blue",
    stroke = FALSE, fillOpacity = 0.7,
    label = ~paste0(market, ": PHP ", price)
  ) %>%
  addLegend(position = "bottomright", title = "Rice Price (PHP)")

################################################################################

fpphlqc <- read_csv("wfp_food_prices_phl_qc.csv")

fpphlqc <- fpphlqc[-1, ]
glimpse(fpphlqc)

sum(is.na(fpphlqc))
sum(duplicated(fpphlqc))
