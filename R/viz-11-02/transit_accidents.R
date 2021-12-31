
# 0. Preamble -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
# https://github.com/crazycapivara/h3-r
library(h3)
library(sf)
library(geobr)
library(tmap)
library(tmaptools)
tmap_mode(mode = "view")

library(showtext)
showtext_opts(dpi = 300)
showtext_auto()

colors <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")

theme_vini <- theme_minimal() +
  theme(
    text = element_text(family = "Roboto", size = 8),
    plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 90),
    
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Import data from CETSP by INFORMSP http://www.respeitoavida.sp.gov.br/relatorios/
# http://painelderesultados.infosiga.sp.gov.br/bases/acidentes_fatais.xlsx
# http://painelderesultados.infosiga.sp.gov.br/bases/acidentes_naofatais.csv

fatal <- readxl::read_excel(here("data/2021_11/acidentes_fatais.xlsx"))
non_fatal <- data.table::fread(here("data/2021_11/acidentes_naofatais.csv"))
# Import city shapefile (for spatial join)
shp_sp <- read_municipality(3550308, simplified = F)
# Hex indexes for sao paulo
hexid <- polyfill(shp_sp, res = 9)
hexsp <- h3_to_geo_boundary_sf(hexid)
hexsp$hex_code <- hexid
# 1. Data Clean -----------------------------------------------------------


## Non-fatal automobile accidents ------------------------------------------

non_fatal <- non_fatal %>%
  # Clean names using janitor
  janitor::clean_names() %>%
  # Rename some variables manually
  rename(
    date = data_do_acidente,
    lat = lat_geo,
    lng = long_geo,
    climate_conditions = condicoes_climaticas,
    daytime = turno_dia_de_semana
  ) %>%
  mutate(
    date = ymd(as.Date(date)),
    # Latitude and longitude values stores with , as decimal
    lat = as.numeric(str_replace(lat, ",", ".")),
    lng = as.numeric(str_replace(lng, ",", ".")),
    # Sum victims
    victims = pessoas_envolvidas_ileso + pessoas_envolvidas_leve + pessoas_envolvidas_grave,
    # Convert daytime into ordered factor
    daytime = factor(daytime,
                     levels = c("MADRUGADA", "MANHA", "TARDE", "NOITE"),
                     labels = c("Madrugada", "Manhã", "Tarde", "Noite")),
    # Weekday and year using lubridate
    weekday = wday(date, label = T),
    year = year(date)
  ) %>%
  select(date, lat, lng, victims, climate_conditions, daytime, weekday, year)

geo_non_fatal <- non_fatal %>%
  # Remove observations without lat/lng
  filter(!is.na(lat) | !is.na(lng)) %>%
  mutate(
    # Convert coordinates to hexid
    hex_code = geo_to_h3(latlng = c(lat, lng), res = 9)
  )


## Fatal Accidents ---------------------------------------------------------

fatal <- fatal %>%
  # Clean names using janitor
  janitor::clean_names() %>%
  # Rename some variables manually
  rename(
    date = data_do_acidente,
    lat = lat_geo,
    lng = long_geo,
    climate_conditions = condicoes_climaticas_siopm,
    daytime = turno,
    victims = quantidade_de_vitimas,
  ) %>%
  select(
    date, lat, lng, victims, climate_conditions, daytime
  ) %>%
  mutate(
    date = ymd(as.Date(date)),
    # Latitude and longitude values stores with , as decimal
    lat = as.numeric(str_replace(lat, ",", ".")),
    lng = as.numeric(str_replace(lng, ",", ".")),
    # Convert daytime into ordered factor
    daytime = factor(daytime,
                     levels = c("MADRUGADA", "MANHA", "TARDE", "NOITE"),
                     labels = c("Madrugada", "Manhã", "Tarde", "Noite")),
    # Weekday and year using lubridate
    weekday = wday(date, label = T),
    year = year(date)
  )

geo_fatal <- fatal %>%
  filter(!is.na(lat) | !is.na(lng)) %>%
  mutate(
    hex_code = geo_to_h3(latlng = c(lat, lng), res = 9)
  )

## Join and time-series ----------------------------------------------------

# Join tables
data <- bind_rows(list(fatal = fatal, non_fatal = non_fatal), .id = "type_aci")
geodata <- bind_rows(list(fatal = geo_fatal, non_fatal = geo_non_fatal), .id = "type_aci")

# Aggregate by month and convert to time-series
data_series <- data %>%
  mutate(
    yearmon = zoo::as.yearmon(date)
  ) %>%
  group_by(yearmon, type_aci) %>%
  summarise(monthly_victims = sum(victims, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(id_cols = "yearmon", names_from = "type_aci", values_from = "monthly_victims") %>%
  mutate(
    date = zoo::as.Date(yearmon),
    total = rowSums(.[2:3], na.rm = T)
  ) %>%
  filter(
    date >= as.Date("2019-01-01")
  ) %>%
  select(-yearmon) %>%
  pivot_longer(
    cols = -date
  ) %>%
  group_by(name) %>%
  mutate(
    mm4 = RcppRoll::roll_meanr(value, n = 4)
  ) %>%
  ungroup()

data_series <- reshape2::melt(data_series, id.vars = c("date", "name"))

# Convert to sf and filter only hex in Sao Paulo
point_fatal <- st_as_sf(geo_fatal, coords = c("lng", "lat"), crs = 4326, remove = F)
point_fatal <- filter(point_fatal, hex_code %in% local(hexid))

point_fatal19 <- filter(point_fatal, year == 2019)

# Sum victims by hexagon for 2019
hex_fatal19 <- geo_fatal %>%
  filter(year == 2019) %>%
  group_by(hex_code) %>%
  summarise(total_victims = sum(victims))
# Join summarized data with hexagon shape
hex_fatal19 <- left_join(hexsp, hex_fatal19, by = "hex_code")

# Maps --------------------------------------------------------------------

## Hex tutorial ------------------------------------------------------------

m0 <- tm_shape(point_fatal19) +
  tm_dots(alpha = 0.5, size = "victims", col = "white") + 
  tm_basemap(server = "CartoDB.DarkMatter") +
  tm_view(set.view = c(-46.634501, -23.527559, 13))

mapview::mapshot(tmap_leaflet(m0), file = here("graphics/2021_11/tutorial_hex_1.png"))

m01 <- tm_shape(hex_fatal19) +
  tm_borders() +
  tm_shape(point_fatal19) +
  tm_dots(alpha = 0.5, size = "victims", col = "white") + 
  tm_basemap(server = "CartoDB.DarkMatter") +
  tm_view(set.view = c(-46.634501, -23.527559, 13))

mapview::mapshot(tmap_leaflet(m01), file = here("graphics/2021_11/tutorial_hex_2.png"))

m02 <- tm_shape(hex_fatal19) +
  tm_borders() +
  tm_fill(col = "total_victims",
          alpha = 0.5,
          breaks = c(0, 1, 2, 3, 4),
          palette = "viridis",
          title = "Total Victims",
          colorNA = NULL) +
  tm_basemap(server = "CartoDB.DarkMatter") +
  tm_view(set.view = c(-46.634501, -23.527559, 13))

mapview::mapshot(tmap_leaflet(m02), file = here("graphics/2021_11/tutorial_hex_3.png"))
## TT maps -----------------------------------------------------------------
# Summarize 2019-2020 Accidents
hex_aci <- geodata %>%
  filter(year %in% c(2019, 2020)) %>%
  group_by(hex_code, year) %>%
  summarise(total_victims = sum(victims, na.rm = T))

# Summarize 2019-2020 Fatalities
hex_fatal <- geo_fatal %>%
  filter(year %in% c(2019, 2020)) %>%
  group_by(hex_code, year) %>%
  summarise(total_victims = sum(victims, na.rm = T))

hex_aci_19 <- left_join(hexsp,
                        filter(hex_aci, year == 2019),
                        by = "hex_code")

hex_aci_20 <- left_join(hexsp,
                        filter(hex_aci, year == 2020),
                        by = "hex_code")

hex_fatal_19 <- left_join(hexsp,
                          filter(hex_fatal, year == 2019),
                          by = "hex_code")

hex_fatal_20 <- left_join(hexsp,
                          filter(hex_fatal, year == 2020),
                          by = "hex_code")

m1 <- tm_shape(hex_fatal_19) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "total_victims",
          alpha = 0.4,
          breaks = c(0, 1, 2, 3, 5, 7),
          palette = "Reds",
          title = "Total Fatalities 2019",
          colorNA = NULL) +
  tm_basemap(server = "CartoDB.DarkMatter") +
  tm_view(set.view = c(-46.675752, -23.555924, 12))

mapview::mapshot(tmap_leaflet(m1), file = here("graphics/2021_11/map_road_fatalities_2019.png"))

m2 <- tm_shape(hex_fatal_20) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "total_victims",
          alpha = 0.4,
          breaks = c(0, 1, 2, 3, 5, 7),
          palette = "Reds",
          title = "Total Fatalities 2020",
          colorNA = NULL) +
  tm_basemap(server = "CartoDB.DarkMatter") +
  tm_view(set.view = c(-46.675752, -23.555924, 12))

mapview::mapshot(tmap_leaflet(m2), file = here("graphics/2021_11/map_road_fatalities_2020.png"))

m3 <- tm_shape(hex_aci_20) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "total_victims",
          breaks = c(0, 3, 6, 12, 20, 35, 90, 152),
          palette = "Reds",
          alpha = 0.4,
          title = "Total Victims 2020",
          colorNA = NULL) +
  tm_basemap(server = "CartoDB.DarkMatter") +
  tm_view(set.view = c(-46.675752, -23.555924, 12))

mapview::mapshot(tmap_leaflet(m3), file = here("graphics/2021_11/map_road_accidents_2019.png"))

m4 <- tm_shape(hex_aci_19) +
  tm_borders(alpha = 0.5) +
  tm_fill(col = "total_victims",
          breaks = c(0, 3, 6, 12, 20, 35, 90, 189),
          palette = "Reds",
          alpha = 0.4,
          title = "Total Victims 2020",
          colorNA = NULL) +
  tm_basemap(server = "CartoDB.DarkMatter") +
  tm_view(set.view = c(-46.675752, -23.555924, 12))

mapview::mapshot(tmap_leaflet(m4), file = here("graphics/2021_11/map_road_accidents_2020.png"))

# Plots -------------------------------------------------------------------

p1 <- ggplot(filter(data_series, name == "total"), aes(x = date, y = value, colour = variable)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_colour_manual(values = colors[c(1, 5)]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".")) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%Y-%b"
  ) +
  labs(
    x = NULL,
    y = "Total Number of Monthly Victims",
    title = "Road Accidents in São Paulo",
    subtitle = "Moving-average trend",
    caption = "Source: CETSP. @viniciusoike"
  ) +
  guides(colour = "none") +
  theme_vini

data_series <- mutate(data_series,
                      name = factor(name,
                                    levels = c("fatal", "non_fatal", "total"),
                                    labels = c("Fatal", "Non-fatal", "Total")))

p2 <- ggplot(filter(data_series, name != "total"), aes(x = date, y = value, colour = variable)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_colour_manual(values = colors[c(1, 5)]) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".")) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%Y-%b"
  ) +
  labs(
    x = NULL,
    y = "Total Number of Monthly Victims",
    title = "Road Accidents in São Paulo",
    subtitle = "Moving-average trend",
    caption = "Source: CETSP. @viniciusoike"
  ) +
  facet_wrap(~name, scales = "free", ncol = 1) +
  guides(colour = "none") +
  theme_vini

cowplot::save_plot(
  here("graphics/2021_11/road_accidents.png"),
  p1,
  base_width = 5,
  dpi = 300
)
cowplot::save_plot(
  here("graphics/2021_11/road_accidents_2.png"),
  p2,
  base_width = 6,
  base_height = 6.5,
  dpi = 300
)