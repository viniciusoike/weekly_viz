library(GetBCBData)
library(tidyverse)
library(here)

gdp <- read_csv(here("data/2022_01/pib_clean.csv"))

gdp <- read_csv(here("data/2022_01/pib_clean.csv"))

info_sidra(1737)

ipca <- get_sidra(1737, period = "199912-202001", variable = 2266)

ipca <- ipca %>%
  janitor::clean_names() %>%
  mutate(date = as.Date(paste0(mes_codigo, "01"), format = "%Y%m%d")) %>%
  select(date, value = valor) %>%
  arrange(date) %>%
  mutate(chg = value / lag(value))

base2010 <- ipca %>%
  filter(date >= as.Date("2010-01-01") & date <= as.Date("2010-12-31")) %>%
  pull(value) %>%
  mean()

ipca <- ipca %>%
  mutate(index_2010 = value / base2010 * 100)

gdp %>%
  select(ano, code_muni, name_muni, pib, pib_pc) %>%
  left_join(ipca)
