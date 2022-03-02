library(here)
library(tidyverse)
library(readxl)

# Import data -------------------------------------------------------------

## Births by state ---------------------------------------------------------

nascimentos <- readxl::read_excel(here("data/2022_02/tabela2612.xlsx"),
                                  range = "A7:AAC1249")

nx <- paste(rep(2003:2020, each = 13), c("total", month.abb), sep = "_")
nx <- paste(rep(nx, each = 3), c("total", "male", "female"), sep = "_")

names(nascimentos)[1:3] <- c("name_state", "number_of_children_born",
                             "mother_age_group")
names(nascimentos)[-(1:3)] <- nx

tbl_nascimento <- nascimentos %>%
  fill(name_state, number_of_children_born) %>%
  pivot_longer(
    cols = -c(name_state, number_of_children_born, mother_age_group)
    ) %>%
  separate(name, into = c("year", "month", "sex"), sep = "_") %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%b-%d"),
    value = as.numeric(value)
    ) %>%
  select(
    date, year, month, name_state, number_of_children_born, mother_age_group,
    sex_children_born = sex, total_births = value)

## Population projections by state -----------------------------------------

pop <- readxl::read_excel(here("data/2022_02/tabela7358.xlsx"),
                          range = "A6:LW1714")

names(pop)[1:2] <- c("name_state", "year")
names(pop) <- str_remove(names(pop), "\\.\\.\\..+")

names(pop)[-(1:2)] <- paste(rep(c("total", "male", "female"), each = 111),
                            names(pop)[-(1:2)],
                            sep = "_")
tbl_pop <- pop %>%
  fill(name_state) %>%
  mutate(across(-name_state, as.numeric)) %>%
  pivot_longer(cols = -c(name_state, year)) %>%
  separate(name, into = c("sex", "age_group"), sep = "_")

tbl_pop_adult <- tbl_pop %>%
  filter(!str_detect(age_group, "( a )|Total")) %>%
  mutate(age = as.numeric(str_extract(age_group, "[:digit:]+"))) %>%
  filter(age >= 15,
         age <= 65,
         sex == "total",
         year >= 2003) %>%
  group_by(name_state, year) %>%
  summarise(pop_adult = sum(value, na.rm = T))

## Weddings ----------------------------------------------------------------

casamentos <- readxl::read_excel(here("data/2022_02/tabela2759.xlsx"),
                                 range = "A8:HK3895",
                                 col_names = F)

col_names <- paste(rep(2003:2020, each = 12), month.abb, sep = "-")
col_names <- c(c("name_state", "age_male", "age_female"), col_names)
names(casamentos) <- col_names

tbl_casamentos <- casamentos %>%
  fill(name_state, age_male, age_female) %>%
  mutate(across(starts_with("2"), as.numeric)) %>%
  pivot_longer(cols = -c(name_state, age_male, age_female)) %>%
  mutate(
    date = as.Date(paste0(name, "-01"), format = "%Y-%b-%d"),
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = TRUE, locale = "pt_BR")
    ) %>%
  select(
    date, year, month, age_male, age_female, name_state, total_weddings = value
    )


## Join data ---------------------------------------------------------------

tbl_nasc_state <- tbl_nascimento %>%
  filter(!is.na(date),
         mother_age_group == "Total",
         sex_children_born == "total") %>%
  select(date, name_state, total_births)

tbl_casa_state <- tbl_casamentos %>%
  group_by(date, year, month, name_state) %>%
  summarise(weddings_month = sum(total_weddings, na.rm = T))

data <- inner_join(tbl_nasc_state, tbl_casa_state, by = c("date", "name_state"))
data <- inner_join(data, tbl_pop_adult, by = c("year", "name_state"))

state_id <- geobr::read_state()
state_id <- sf::st_drop_geometry(state_id)

data <- data %>%
  mutate(name_state = str_to_title(name_state)) %>%
  left_join(state_id, by = "name_state")

data <- data %>%
  mutate(
    b_rate = total_births / (pop_adult / 100000),
    w_rate = weddings_month / (pop_adult / 100000)
  )

# Export ------------------------------------------------------------------

temp <- list(data = data,
             weddings = tbl_casamentos,
             births = tbl_nascimento,
             pop = tbl_pop)

write_rds(temp, here("data/temp_data.rds"))

# tbl_casamentos_mes <- tbl_casamentos %>%
#   group_by(date, year, month, name_state) %>%
#   summarise(casamento_mes = sum(casamento, na.rm = T)) %>%
#   left_join(tbl_pop_adult, by = c("year", "name_state")) %>%
#   mutate(taxa = casamento_mes / (pop_adult / 100000),
#          log_casamento = log(casamento_mes),
#          ma6 = RcppRoll::roll_meanr(log_casamento, n = 6))
# 
# ggplot(casamentos_mes, aes(x = date, y = taxa)) +
#   geom_line() +
#   facet_wrap(~name_state, scales = "free")
# 
# ggplot(casamentos_ano, aes(x = year, y = log_casamento, colour = name_state)) +
#   geom_line() +
#   facet_wrap(~name_state)