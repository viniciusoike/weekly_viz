library(tmap)
library(tmaptools)
library(h3)
library(tidyverse)
library(here)
library(sf)
library(leaflet)
tmap_mode(mode = "view")

data <- read_csv(here("data/2021_10/skyscrapper_webscrape_geolocated.csv"))
geodata <- st_as_sf(data, coords = c("long", "lat"), crs = 4326, remove = FALSE)

spmuni <- geobr::read_municipality(3550308, simplified = F)
spmuni <- st_transform(spmuni, crs = 4326)

geodata_valid <- geodata %>%
  st_join(spmuni) %>%
  filter(!is.na(code_muni))

full_data <- read_csv(here("data/2021_10/skyscrapper_webscrape.csv"))

data_problem <- full_data %>%
  filter(!(link %in% geodata_valid$link))

View(data_problem)

leaflet(geodata) %>%
  addTiles() %>%
  addMarkers()

library(sf)

geodata_valid %>%
  mutate(
    hex_code = coords_
  )

