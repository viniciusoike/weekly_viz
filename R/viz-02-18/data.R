library(sidrar)
library(here)
library(tidyverse)
library(lubridate)

# Data --------------------------------------------------------------------

nasc <- get_sidra(
  x = 2612,
  period = "2003-2020",
  variable = 218,
  classific = "c235",
  geo = "Brazil"
)

nasc <- janitor::clean_names(nasc)

tbl_nasc <- nasc %>%
  filter(!(mes_do_nascimento %in% c("Total", "Ignorado"))) %>%
  mutate(
    mes = str_sub(str_to_lower(mes_do_nascimento), 1, 3),
    date_b = paste(ano, mes, "01", sep = "-"),
    date = parse_date_time(date_b, orders = "%Y-%b-%d", locale = "pt_BR"),
    date = ymd(date)) %>%
  select(date, ano, nascimentos = valor)

# Download mortality data (in batches because of API limit)

#windows <- as.character(seq(2003, 2020, 1))
#windows <- c("2003-2010", "2011-2018", "2019-2020")
#sidra_mort <- list()

# for (i in seq_along(windows)) {
#   sidra_mort[[i]] <- get_sidra(
#     x = 2681,
#     period = windows[i],
#     variable = 343,
#     classific = list("c244", "c260", "c1836"),
#     geo = "Brazil"
#   )
# }
# # Bind rows together
# mort <- bind_rows(sidra_mort)
# # Clean
# mort <- janitor::clean_names(mort)

mort <- readxl::read_excel(here("data/2022_02/tabela2681.xlsx"),
                           range = "B8:AVO88",
                           col_names = FALSE)

ano <- 2003:2020
mes <- c("total", month.abb, "ignorado")
causa <- c("total", "natural", "nao-natural", "outra", "ignorado")

x <- paste(rep(ano, each = length(mes)), mes, sep = "_")
x <- paste(rep(x, each = length(causa)), causa, sep = "_")

names(mort) <- c("idade", "drop", x)

mort <- mort %>%
  select(-drop) %>%
  mutate(across(starts_with("2"), as.numeric)) %>%
  pivot_longer(cols = -idade) %>%
  separate(name, into = c("year", "month", "cause"), sep = "_")

x <- c(1, 5, seq(10, 95, 5))
y <- c(4, 9, seq(14, 99, 5))

age_groups <- str_glue("{x} a {y} anos", x = x, y = y)
age_groups <- c(age_groups, "100 anos ou mais")

tbl_mort <- mort %>%
  filter(month != "total",
         idade %in% age_groups) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%b-%d"),
         idade = factor(idade, levels = local(age_groups))) %>%
  na.omit() %>%
  select(date, year, month, cause, mortes = value, grupo_idade = idade)

# Aggregate total deaths by month
tbl_mort_total <- tbl_mort %>%
  group_by(date) %>%
  summarise(mortes_total = sum(mortes, na.rm = TRUE))

# Join with total births
data <- inner_join(tbl_nasc, tbl_mort_total, by = c("date"))
# Convert to long and adjust factor labels
data <- data %>%
  select(-ano) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, labels = c("Deaths", "Births")))

write_rds(list(data = data, tbl_mort = tbl_mort), here("data/temp_data.rds"))