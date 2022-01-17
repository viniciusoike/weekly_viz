library(tidyverse)
library(here)
library(sf)
library(mapview)

data_full <- read_csv(here("data/2021_10/skyscrapper_webscrape.csv"))
data_geo <- read_csv(here("data/2021_10/skyscrapper_webscrape_geolocated.csv"))

sp_muni <- geobr::read_municipality(3550308)
bbox <- st_bbox(sp_muni)

data_geo_valid <- data_geo %>%
  filter(
    lat > bbox[2],
    lat < bbox[4],
    long > bbox[1],
    long < bbox[3]
  )

geo <- st_as_sf(data_geo_valid, coords = c("long", "lat"), crs = 4326)

mapview(geo)


# Focus on specific areas (Centro, Paulista, and Itaim)
x1 <- data_full$address
x2 <- data_geo$address  

x1[!(x1 %in% x2)]
