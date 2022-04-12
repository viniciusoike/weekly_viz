# House prices and demographic trends #
library(tidyverse)
library(here)
library(showtext)
font_add_google("Montserrat", "Montserrat")
showtext_auto()
source(here("R/theme.R"))

# Import house price indexes from BIS #
bis <- read_csv(here("data/2022_04/rppi_bis_data.csv"))

bis <- bis %>%
  filter(year >= 1960, unit == "Index, 2010 = 100")

bis_real <- bis %>%
  filter(nominal == "Real", month == 6) %>%
  select(year, country = reference_area, value) %>%
  group_by(year, country) %>%
  summarise(index_house = mean(value, na.rm = T))

# Import demographic data from WBD #
library(WDI)
search <- WDIsearch("population")

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

# Join information across tables
tbl <- bis_nominal %>%
  filter(year >= 1970) %>%
  left_join(tbl_pop) %>%
  left_join(base_pop) %>%
  mutate(index_pop = pop / base_pop * 100,
         index_pop_adult = pop_adult / base_pop_adult * 100) %>%
  select(year, country, index_house, index_pop, index_pop_adult) %>%
  pivot_longer(cols = -c(year, country), names_to = "index")

sel <- c("Australia", "United States", "Japan", "Germany", "Austria", "France")
sel <- c("Chile", "Brazil", "Peru", "Mexico")

ggplot(filter(tbl, country %in% sel),
       aes(x = year, y = value, colour = index)) +
  geom_line(size = 1) +
  facet_wrap(~country) +
  theme_vini

ggplot(tbl,
       aes(x = year, y = value, colour = index)) +
  geom_line(size = 1) +
  facet_wrap(~country) +
  guides(color = "none") +
  theme_vini
