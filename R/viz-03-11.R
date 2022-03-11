library(tidyverse)
library(here)
source(here("R/theme.R"))

data <- read_rds(here("data/temp_data.rds"))

data$weddings

library(sidrar)

sidrar::info_sidra(5934)

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

unique(divorcios_clean_1$age_group)
unique(divorcios_clean_2$age_group)

unique(divorcios_clean_1$variable)
unique(divorcios_clean_2$variable)

names(divorcios_clean_1)

divorcios_clean <- rbind(select(divorcios_clean_1, -spouse_type),
                         divorcios_clean_2)

divorcios_clean <- as_tibble(divorcios_clean)

age_group_levels <- c(
  "Menos de 20 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
  "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
  "55 a 59 anos", "60 a 64 anos", "65 a 69 anos", "70 a 74 anos", 
  "75 anos ou mais", "Total", "Idade ignorada"
)

get_midpoint <- function(string, sep = " a ") {
  
  if (!is.character(string)) {string <- as.character(string)}
  
  split <- stringr::str_extract_all(string, pattern = "[:digit:]{1,3}")
  
  split <- lapply(split, function(x) if (length(x) == 0) c("0", "0") else x)
  m <- as.matrix(reduce(split, rbind))
  num <- sapply(m, as.numeric)
  midpoint <- rowMeans(num)
 
  return(midpoint) 

}

get_midpoint(c("Total", "20 a 25 anos"))

test <- c("20 a 24 anos", "25 a 29 anos")
# 
# t1 <- str_split(test, "a")
# 
# t2 <- lapply(t1, function(x) na.omit(as.numeric(x)))
# 
# sapply(t2, function(x) x[[1]] + diff(x) / 2)
# 
# diff(t2[[1]])
# 
# sapply(t1, function(x) str_extract_all(x, "[:digit:]"))
# str_extract_all(test, "[:digit:]")

divorcios_clean <- divorcios_clean %>%
  mutate(year = as.numeric(year),
         code_state = as.numeric(code_state),
         code_variable = factor(code_variable),
         variable = factor(variable),
         age_group = factor(age_group, levels = age_group_levels)
         )

divorcios_encerrados <- divorcios_clean %>%
  filter(code_variable == 230) 

divorcios_age_mean <- divorcios_encerrados %>%
  filter(!(age_group %in% c("Total", "Idade ignorada"))) %>%
  mutate(midpoint = case_when(
    age_group == "Menos de 20 anos" ~ 15,
    age_group == "75 anos ou mais"  ~ 80,
    TRUE ~ get_midpoint(age_group)
  ))

divorcios_age_mean %>% head(15) %>% View()

divorcios_clean %>%
  filter(age_group == "Total") %>%
  ggplot(aes(x = year, y = value, color = as.factor(code_variable))) +
  geom_line() +
  facet_wrap(~code_state)

ggplot(data = divorcios_clean, aes(x = year))

divorcios_clean %>% select(code_variable, variable) %>% distinct()
