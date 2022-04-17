# House prices and demographic trends #
library(tidyverse)
library(ggrepel)
library(here)
library(WDI)
library(MetBrewer)
library(showtext)
font_add_google("Montserrat", "Montserrat")
showtext_auto()
source(here("R/theme.R"))

colors <- met.brewer("Hokusai1", n = 8)[c(3, 7, 8)]

# Import house price indexes from BIS #
bis <- read_csv(here("data/2022_04/rppi_bis_data.csv"))

bis <- bis %>%
  filter(year >= 1960, unit == "Index, 2010 = 100")

bis_real <- bis %>%
  filter(nominal == "Real") %>%
  select(year, country = reference_area, value) %>%
  group_by(year, country) %>%
  summarise(index_house = mean(value, na.rm = T))

bis_nominal <- bis %>%
  filter(nominal == "Nominal") %>%
  select(year, country = reference_area, value) %>%
  group_by(year, country) %>%
  summarise(index_house = mean(value, na.rm = T))

# Import demographic data from WBD #

wdi_names <- as_tibble(WDI_data$country)

#search <- WDIsearch("population")
# Total population
pop <- WDI(indicator = "SP.POP.TOTL")
# Population growth
pop_growth <- WDI(indicator = "SP.POP.GROW")
# Adult population
codes <- c(
  "SP.POP.2024.MA", "SP.POP.2529.MA", "SP.POP.3034.MA", "SP.POP.3539.MA",
  "SP.POP.4044.FE", "SP.POP.2024.FE", "SP.POP.2529.FE", "SP.POP.3034.FE",
  "SP.POP.3539.FE", "SP.POP.4044.FE")

adult_pop <- WDI(indicator = codes)

tbl_pop_adult <- adult_pop %>%
  select(-iso2c) %>%
  pivot_longer(cols = -c(year, country)) %>%
  group_by(year, country) %>%
  summarise(pop_adult = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(
    country = str_replace_all(country, "Hong Kong SAR, China", "Hong Kong SAR"),
    country = str_replace_all(country, "Korea, Rep.", "Korea"),
    country = str_replace_all(country, "Russian Federation", "Russia"))

tbl_pop <- pop %>%
  mutate(
    country = str_replace_all(country, "Hong Kong SAR, China", "Hong Kong SAR"),
    country = str_replace_all(country, "Korea, Rep.", "Korea"),
    country = str_replace_all(country, "Russian Federation", "Russia")) %>%
  select(year, country, pop = SP.POP.TOTL) %>%
  arrange(year) %>%
  left_join(tbl_pop_adult)

base_pop <- tbl_pop %>%
  filter(year == 2010) %>%
  select(country, base_pop = pop, base_pop_adult = pop_adult)

tbl_pop <- tbl_pop %>%
  left_join(base_pop) %>%
  mutate(index_pop = pop / base_pop * 100,
         index_pop_adult = pop_adult / base_pop_adult * 100)

# Join information across tables

# Real property prices
tbl <- bis_real %>%
  filter(year >= 1970, year <= 2020) %>%
  left_join(tbl_pop) %>%
  select(year, country, index_house, index_pop, index_pop_adult) %>%
  pivot_longer(cols = -c(year, country), names_to = "index")
# Nominal property prices
tbl_nominal <- bis_nominal %>%
  filter(year >= 1970, year <= 2020) %>%
  left_join(tbl_pop) %>%
  select(year, country, index_house, index_pop, index_pop_adult) %>%
  pivot_longer(cols = -c(year, country), names_to = "index")

# Convert real prices to wide and compare evolution
tbl_wide <- tbl %>%
  filter(year %in% c(2010, 2020),
         index %in% c("index_house", "index_pop")) %>%
  group_by(country, index) %>%
  mutate(diff = diff(value)) %>%
  ungroup() %>%
  filter(year == 2020) %>%
  pivot_wider(id_cols = country, names_from = index, values_from = diff) %>%
  left_join(select(wdi_names, country, iso3c)) %>%
  mutate(highlight = factor(if_else(country == "World", 1, 0)))

tbl_wide_lr <- tbl_nominal %>%
  filter(year %in% c(1990, 2020),
         index %in% c("index_house", "index_pop")) %>%
  group_by(country, index) %>%
  mutate(diff = diff(value)) %>%
  ungroup() %>%
  filter(year == 2020, !is.na(diff)) %>%
  pivot_wider(id_cols = country, names_from = index, values_from = diff) %>%
  left_join(select(wdi_names, country, iso3c)) %>%
  mutate(highlight = factor(if_else(country == "World", 1, 0)))


sel <- c("Australia", "United States", "Japan", "Germany", "Austria", "France")
sel_latam <- c("Chile", "Brazil", "Peru", "Mexico", "Colombia")

ggplot(filter(tbl, country == "United States"),
       aes(x = year, y = value, color = index)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = colors,
    name = "",
    labels = c("Real House Prices", "Population", "Adult Pop.")) +
  theme_vini

ggplot(tbl,
       aes(x = year, y = value, colour = index)) +
  geom_line(size = 1) +
  facet_wrap(~country) +
  guides(color = "none") +
  theme_vini


sel_conforme <- c("Singapore", "Belgium", "France", "Japan", "Korea", "Italy")
sel_acima <- c("Switzerland", "Hong Kong SAR", "Chile", "India", "Malaysia", "Estonia")

ggplot(filter(tbl, country %in% sel_conforme, year >= 2000),
       aes(x = year, y = value, colour = index)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 100, linetype = 2, color = "gray60") +
  facet_wrap(~country) +
  scale_color_manual(
    values = colors,
    name = "",
    labels = c("Real House Prices", "Population", "Adult Pop.")) +
  scale_y_continuous(breaks = seq(60, 140, 20)) +
  labs(title = "House prices follow population growth",
       x = NULL,
       y = "Index (2010 = 100)",
       caption = "Source: Real House Price Indexes (BIS), Population (UN).\nAdult Pop. defined as population from 20-44 years.") +
  theme_vini

ggplot(filter(tbl, country %in% sel_acima, year >= 2000),
       aes(x = year, y = value, colour = index)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 100, linetype = 2, color = "gray60") +
  facet_wrap(~country) +
  scale_color_manual(
    values = colors,
    name = "",
    labels = c("Real House Prices", "Population", "Adult Pop.")) +
  scale_y_continuous(breaks = seq(60, 200, 20)) +
  labs(title = "In some markets, hose prices have detached from pop. growth",
       x = NULL,
       y = "Index (2010 = 100)",
       caption = "Source: Real House Price Indexes (BIS), Population (UN).\nAdult Pop. defined as population from 20-44 years.") +
  theme_vini

ggplot(filter(tbl, country %in% sel_latam, year >= 2000),
       aes(x = year, y = value, colour = index)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 100, linetype = 2, color = "gray60") +
  facet_wrap(~country) +
  scale_color_manual(
    values = colors,
    name = "",
    labels = c("Real House Prices", "Population", "Adult Pop.")) +
  scale_y_continuous(breaks = seq(60, 200, 20)) +
  labs(title = "LATAM markets are not synched",
       x = NULL,
       y = "Index (2010 = 100)",
       caption = "Source: Real House Price Indexes (BIS), Population (UN).\nAdult Pop. defined as population from 20-44 years.") +
  theme_vini



ggplot(data = na.omit(tbl_wide),
       aes(x = index_pop, y = index_house)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = colors[1]) +
  geom_point(aes(color = highlight)) +
  geom_text_repel(aes(label = iso3c, color = highlight)) +
  scale_x_continuous(breaks = seq(-10, 30, 5)) +
  scale_y_continuous(breaks = seq(-30, 90, 15)) +
  scale_color_manual(values = c("black", colors[1])) +
  guides(color = "none") +
  labs(
    title = "Real House Prices x Population (2010/20)",
    x = "Population (2010/2020)",
    y = "RPPI (2010/2020)",
    caption = "Source: Real House Price Indexes (BIS), Population (UN).") +
  theme_vini


