library(tidyverse)
library(here)
library(rvest)
library(geobr)
library(MetBrewer)
library(data.table)

# Import CPI data #
ipca <- GetBCBData::gbcbd_get_series(433, first.date = as.Date("2000-01-01"))
ipca <- setDT(ipca)
ipca[, ano := lubridate::year(ref.date)]
# Acumulate monthly -> yearly
ipca_ano <- ipca[, .(acum = prod(1 + value / 100) - 1), by = ano]

# Normalize prices in 2010
ipca_ano[ano == 2010, index := 1]
ipca_ano[ano > 2010 , index := cumprod(1 + acum)]
ipca_ano[ano < 2010 , index := rev(1 / cumprod(1 + acum))]
# Deflator IPCA 2010
ipca_ano[, ajuste := 1 / index]
ipca_ano <- ipca_ano[, c("ano", "ajuste")]

# Import GDP #
gdp <- read_csv(here("data/2022_01/pib_clean.csv"))

gdppc <- gdp %>%
  select(ano, code_muni, name_muni, pib_pc) %>%
  arrange(ano) %>%
  arrange(code_muni) %>%
  left_join(ipca_ano, by = "ano") %>%
  group_by(code_muni, name_muni) %>%
  mutate(pibpc_chg = (pib_pc / lag(pib_pc) - 1) * 100) %>%
  mutate(pib_pc_real = pib_pc * ajuste,
         pib_pc_real_chg = (pib_pc_real / lag(pib_pc_real) - 1) * 100)

# Plot theme
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

# Color scheme
cores <- met.brewer("Hokusai1", n = 27, type = "continuous")

# Scrape list of Brazilian capital cities from Wikipedia

url <- "https://pt.wikipedia.org/wiki/Lista_de_capitais_do_Brasil_por_área"

tbl_scrape <- url %>%
  read_html() %>%
  html_table() %>%
  .[[1]]

cities <- janitor::clean_names(tbl_scrape)

# IBGE identicators for cities

geostate <- read_state()

geoid <- tibble(code_muni = cities$codigo_do_ibge)
munis <- geobr::read_municipality()
munis <- sf::st_drop_geometry(munis)
geoid <- left_join(geoid, munis, by = "code_muni")
geoid <- geoid %>%
  mutate(
    code_region = str_sub(code_state, 1, 1),
    abbrev_region = case_when(
      code_region == 1 ~ "N",
      code_region == 2 ~ "NE",
      code_region == 3 ~ "SE",
      code_region == 4 ~ "S",
      code_region == 5 ~ "CO"
    ),
    name_region = case_when(
      code_region == 1 ~ "North",
      code_region == 2 ~ "Northeast",
      code_region == 3 ~ "Southeast",
      code_region == 4 ~ "South",
      code_region == 5 ~ "Midwest"
    )
  )

geoid <- geoid %>%
  arrange(name_muni) %>%
  arrange(code_region)

geo03 <- left_join(geoid, filter(gdppc, ano == 2003))
geo13 <- left_join(geoid, filter(gdppc, ano == 2013))
geo19 <- left_join(geoid, filter(gdppc, ano == 2019))

pib_capitais <- filter(gdppc, code_muni %in% cities$codigo_do_ibge)

pib_capitais %>%
  ggplot(aes(x = ano, y = pib_pc_real)) +
  geom_line() +
  facet_wrap(~name_muni)

# Plots -------------------------------------------------------------------

ggplot() +
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

