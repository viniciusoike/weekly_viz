
# 0. Preamble -------------------------------------------------------------

library(tidyverse)
library(sidrar)
library(here)
library(showtext)
font_add_google("Montserrat", "Montserrat")
showtext_auto()
source(here("R/theme.R"))

# Function: gets the midpoint between two different numeric values that are
# stored as a string (e.g. 5 to 10 years -> 7).
get_df_midpoint <- function(df, variable = NULL) {
  
  df %>%
    rowwise() %>%
    mutate(
      min = as.numeric(unlist(str_extract_all({{variable}}, "\\d+"))[1]),
      max = as.numeric(unlist(str_extract_all({{variable}}, "\\d+"))[2]),
      midpoint = ifelse(is.na(min) | is.na(max), NA, min + (max - min) / 2)
    ) %>%
    ungroup() %>%
    select(-min, -max)
  
}

# 1. Import Data ----------------------------------------------------------

# Read temp data created with R/viz-02-25/data.R
data <- read_rds(here("data/temp_data.rds"))

tbl_population <- data[[1]]

tbl_adult_pop <- tbl_population %>%
  select(year, code_state, abbrev_state, pop_adult) %>%
  distinct()

tbl_dim_state <- tbl_population %>%
  select(code_state, abbrev_state) %>%
  distinct()

# Imports divorces from IBGE SIDRA

divorcios_1 <- get_sidra(
  x = 2993,
  variable = c(230, 231),
  period = "2003-2013",
  classific = c("c267", "c266"),
  geo = "State"
)

divorcios_2 <- get_sidra(
  x = 5934,
  variable = c(230, 231),
  period = "2014-2020",
  classific = "c736",
  geo = "State"
  )


# 2. Data Clean -----------------------------------------------------------

# Select and rename columns

divorcios_1 <- janitor::clean_names(divorcios_1)

divorcios_clean_1 <- divorcios_1 %>%
  select(
    ano,
    unidade_da_federacao_codigo,
    grupos_de_idade_dos_conjuges_na_data_da_abertura_do_processo,
    tipo_de_conjuge,
    variavel_codigo,
    variavel,
    valor
    ) %>%
  rename(
    year = ano,
    code_state = unidade_da_federacao_codigo,
    age_group = grupos_de_idade_dos_conjuges_na_data_da_abertura_do_processo,
    spouse_type = tipo_de_conjuge,
    code_variable = variavel_codigo,
    variable = variavel,
    value = valor) %>%
  filter(spouse_type == "Mulher")

# Select and rename columns
divorcios_2 <- janitor::clean_names(divorcios_2)

divorcios_clean_2 <- divorcios_2 %>%
  select(
    ano,
    unidade_da_federacao_codigo,
    grupos_de_idade_da_mulher_na_data_da_abertura_do_processo,
    variavel_codigo,
    variavel,
    valor
    ) %>%
  rename(
    year = ano,
    code_state = unidade_da_federacao_codigo,
    age_group = grupos_de_idade_da_mulher_na_data_da_abertura_do_processo,
    code_variable = variavel_codigo,
    variable = variavel,
    value = valor
  )

# Rowbind tables
divorcios_clean <- rbind(select(divorcios_clean_1, -spouse_type),
                         divorcios_clean_2)

divorcios_clean <- as_tibble(divorcios_clean)

# Order levels for age groups
age_group_levels <- c(
  "Menos de 20 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
  "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
  "55 a 59 anos", "60 a 64 anos", "65 a 69 anos", "70 a 74 anos", 
  "75 anos ou mais", "Total", "Idade ignorada"
)

divorcios_clean <- divorcios_clean %>%
  mutate(year = as.numeric(year),
         code_state = as.numeric(code_state),
         code_variable = factor(code_variable),
         variable = factor(variable),
         age_group = factor(age_group, levels = age_group_levels)
         )
# Join with state ids
divorcios_clean <- divorcios_clean %>%
  left_join(tbl_dim_state)

divorcios_encerrados <- divorcios_clean %>%
  filter(code_variable == 230) 

# Aggregate by year and compute yearly rate (%adult pop.)
divorces_year <- divorcios_clean %>%
  filter(age_group == "Total", code_variable == 231) %>%
  select(year, code_state, divorces = value) %>%
  inner_join(tbl_adult_pop, by = c("year", "code_state")) %>%
  mutate(divorce_rate = divorces / pop_adult * 100)

divorces_rate_brazil <- divorces_year %>%
  group_by(year) %>%
  summarise(total_divorces = sum(divorces),
            total_adult_pop = sum(pop_adult)) %>%
  ungroup() %>%
  mutate(divorce_rate = 100 * total_divorces / total_adult_pop)

# Compute average age of divorce
divorcios_age_mean <- divorcios_encerrados %>%
  filter(!(age_group %in% c("Total", "Idade ignorada"))) %>%
  get_df_midpoint(variable = age_group) %>%
  mutate(midpoint = if_else(age_group == "Menos de 20 anos", 15, midpoint),
         midpoint = if_else(age_group == "75 anos ou mais", 77, midpoint),
         value = if_else(is.na(value), 0, value)) %>%
  group_by(year, code_state) %>%
  summarise(avg_age_divorce = weighted.mean(midpoint, value, na.rm = T)) %>%
  left_join(tbl_dim_state)

# Fix some extreme cases 
df <- divorcios_encerrados %>%
  filter(!(age_group %in% c("Total", "Idade ignorada"))) %>%
  get_df_midpoint(variable = age_group) %>%
  mutate(midpoint = if_else(age_group == "Menos de 20 anos", 15, midpoint),
         midpoint = if_else(age_group == "75 anos ou mais", 80, midpoint))

states <- geobr::read_state()
dim_state <- sf::st_drop_geometry(states)
age_states <- left_join(states, divorcios_age_mean %>% filter(year == 2019))

divorcios_encerrados %>%
  filter(year == 2019,
         !(age_group %in% c("Total", "Idade ignorada"))) %>%
  filter(is.na(abbrev_state))

divorcio_age_dist <- divorcios_encerrados %>%
  filter(year == 2019,
         !(age_group %in% c("Total", "Idade ignorada"))) %>%
  left_join(dim_state) %>%
  group_by(name_region, age_group) %>%
  summarise(divorce_region = sum(value)) %>%
  ungroup()

# 3. Plots ----------------------------------------------------------------

ggplot(data = divorces_year, aes(x = year, y = divorce_rate * 10)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~abbrev_state) +
  labs(
    title = "Divorce rate by state",
    y = "Number of divorces per 1000 adult pop.",
    x = NULL
  ) +
  theme_vini

ggplot(data = divorcios_age_mean, aes(x = year, y = avg_age_divorce)) +
  geom_line(size = 1) +
  facet_wrap(~abbrev_state) +
  theme_vini +
  labs(
    title = "Average age at divorce",
    y = "Years",
    x = NULL
  ) +
  theme(panel.border = element_rect(fill = NA, colour = "gray20"))

ggplot(data = divorces_rate_brazil, aes(x = year, y = divorce_rate * 10)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 2003:2020) +
  scale_y_continuous() +
  labs(
    title = "Divorce rate in Brazil",
    y = "Number of divorces per 1000 adult pop.",
    x = NULL
  ) +
  theme_vini

ggplot() +
  geom_sf(
    data = age_states,
    aes(fill = avg_age_divorce),
    size = 0.5) +
  scale_fill_viridis_c(name = "") +
  labs(
    title = "Average divorce age") +
  theme_void() +
  theme(
    legend.position = c(0.2, 0.25),
    text = element_text(family = "Montserrat"),
    plot.title = element_text(hjust = 0.5, family = "Montserrat", size = 14)
  )

ggplot(data = divorcio_age_dist,
       aes(x = age_group, y = divorce_region)) +
  geom_hline(yintercept = 0) +
  geom_col(fill = colors[1]) +
  coord_flip() +
  facet_wrap(~name_region, scales = "free_x") +
  scale_y_continuous(labels = scales::number_format(big.mark = ".")) +
  labs(
    title = "Age of Divorce Across Brazilian Regions",
    x = NULL,
    y = NULL
  ) +
  theme_vini

