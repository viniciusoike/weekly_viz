library(geobr)
library(sf)
library(here)
library(tidyverse)
library(MetBrewer)
library(showtext)

font_add("Gill Sans", "GillSans.ttc")
showtext_auto()
showtext_opts(dpi = 300)

theme_map <- theme_void() +
  theme(
    text = element_text(family = "Gill Sans", size = 10, colour = "gray10"),
    
    plot.title = element_text(
      face = "bold", size = 16, hjust = 0.5, colour = "black"
    ),
    plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "gray20"),
    plot.caption = element_text(size = 6),
    # Legend
    legend.margin = margin(0, unit = "pt"),
    legend.box.margin = margin(0, unit = "pt"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = c(0.15, 0.25),
    # Background
    panel.background = element_rect(fill = "#fff7bc", color = "#fff7bc"),
    plot.background = element_rect(fill = "#fff7bc", color = "#fff7bc")
  )

# Colo scheme
cores <- met.brewer("Hokusai1", n = 27, type = "continuous")


# Data ------------------------------------------------------------------

# Shapes
munis <- read_municipality()
munis <- mutate(munis, code_muni = as.character(code_muni))
geostate <- read_state()

data <- read_csv(here("data/2022_01/pib_clean.csv"))


## Clean -------------------------------------------------------------------

pib_share <- data %>%
  filter(ano == 2019) %>%
  select(code_muni, pib) %>%
  mutate(pib_perc = pib / sum(pib)) %>%
  arrange(desc(pib_perc)) %>%
  mutate(pib_acum = cumsum(pib_perc))

limit50 <- pib_share %>%
  mutate(dist = abs(pib_acum - 0.5)) %>%
  slice_min(dist) %>%
  pull(pib)

limit90 <- pib_share %>%
  mutate(dist = abs(pib_acum - 0.9)) %>%
  slice_min(dist) %>%
  pull(pib)

pib50 <- pib_share %>%
  filter(pib >= limit50) %>%
  pull(code_muni)

pib90 <- pib_share %>%
  filter(pib >= limit90) %>%
  pull(code_muni)

points <- munis %>%
  st_make_valid() %>%
  st_centroid()

points <- mutate(points, code_muni = as.numeric(code_muni))

geo <- left_join(points, pib_share, by = "code_muni")


# Maps --------------------------------------------------------------------

## Map 1 -------------------------------------------------------------------

p1 <- ggplot() +
  geom_sf(
    data = geostate,
    size = 0.3,
    color = "gray20",
    fill = "gray95"
  ) +
  geom_sf(
    data = filter(geo, code_muni %in% pib50),
    aes(size = pib_perc, color = code_state),
    alpha = 0.8
  ) +
  coord_sf(xlim = c(NA, -34.5)) + 
  scale_size_continuous(
    name = "Participação no PIB (%)",
    breaks = c(0.005, 0.01, 0.05, 0.1),
    labels = c(0.5, 1, 5, 10),
    range = c(0, 20)
  ) +
  #scale_color_viridis_d() +
  scale_color_manual(
    values = cores
  ) +
  labs(
    title = "Concentração do PIB no Brasil",
    subtitle = "Quase 50% do PIB do Brasil é produzido 71 municípios (1,2% do total).\nSão Paulo é a cidade com maior participação no PIB (10,3%).",
    caption = "Fonte: IBGE (Contas Nacionais 2019). Cores: MetBrewer. Autor: @viniciusoike"
  ) +
  guides(color = "none") +
  theme_map

## Map 2 -------------------------------------------------------------------

p2 <- ggplot() +
  geom_sf(
    data = geostate,
    size = 0.3,
    color = "gray20",
    fill = "gray95"
  ) +
  geom_sf(
    data = filter(geo, code_muni %in% pib90),
    aes(size = pib_perc, color = code_state),
    alpha = 0.8
  ) +
  coord_sf(xlim = c(NA, -34.5)) + 
  scale_size_continuous(
    name = "Participação no PIB (%)",
    breaks = c(0.005, 0.01, 0.05, 0.1),
    labels = c(0.5, 1, 5, 10),
    range = c(0, 20)
  ) +
  #scale_color_viridis_d() +
  scale_color_manual(
    values = cores
  ) +
  labs(
    title = "Concentração do PIB no Brasil",
    subtitle = "Para chegar em 90% do PIB nacional precisamos somar a produção de 1.308 municípios (23,5% do total).",
    caption = "Fonte: IBGE (Contas Nacionais 2019). Cores: MetBrewer. Autor: @viniciusoike"
  ) +
  guides(color = "none") +
  theme_map


# Export ------------------------------------------------------------------

cowplot::save_plot(here("graphics", "2022_01", "mapa_concentracao_50.png"),
                   p1,
                   base_height = 6)

cowplot::save_plot(here("graphics", "2022_01", "mapa_concentracao_90.png"),
                   p2,
                   base_height = 6)