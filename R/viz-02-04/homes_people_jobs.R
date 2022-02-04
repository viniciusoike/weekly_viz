library(tidyverse)
library(sidrar)
library(here)
library(rvest)
library(imputeTS)
library(forecast)
library(MetBrewer)
library(showtext)

font_add("Helvetica", "Helvetica.ttc")
showtext_auto()

colors <- met.brewer("Hokusai1", 7)[c(2, 5, 7)]

theme_vini <- theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"),
    
    legend.position = "top",
    legend.box.margin = margin(0),
    legend.margin = margin(0),
    
    text = element_text(family = "Helvetica", size = 10, color = "gray15"),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8, color = "gray30"),
    plot.caption  = element_text(size = 6, color = "gray30"),
    axis.text.x = element_text(angle = 90)
  )


# Data --------------------------------------------------------------------

# Scrape list of Brazilian capital cities from Wikipedia

url <- "https://pt.wikipedia.org/wiki/Lista_de_capitais_do_Brasil_por_área"

tbl_scrape <- url %>%
  read_html() %>%
  html_table() %>%
  .[[1]]

cities <- janitor::clean_names(tbl_scrape)

# IBGE identicators for cities
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

## Population --------------------------------------------------------------

# Source: Estimativas da Populacao (misses 2000, 2007, and 2010)
pop_estima <- get_sidra(
  6579,
  geo = "City",
  period = "2000-2020",
  geo.filter = list("City" = cities$codigo_do_ibge))

# Source: Census (1970, 1980, 1991, 2000, 2010)
pop_censo <- get_sidra(
  200,
  geo = "City",
  period = "1970-2010",
  variable = 93,
  classific = c("c1"),
  geo.filter = list("City" = cities$codigo_do_ibge)
  )

# Clean data

pop_estima <- pop_estima %>%
  janitor::clean_names() %>%
  mutate(date = as.Date(paste0(ano_codigo, "-01-01"))) %>%
  select(code_muni = municipio_codigo, date, value = valor)

pop_censo <- pop_censo %>%
  janitor::clean_names() %>%
  filter(situacao_do_domicilio == "Total") %>%
  mutate(date = as.Date(paste0(ano_codigo, "-01-01"))) %>%
  select(code_muni = municipio_codigo, date, value = valor)

# Stack together and arrange
pop <- rbind(pop_censo, pop_estima)
pop <- arrange(pop, code_muni)
pop <- arrange(pop, date)

# Index on first value
pop <- pop %>%
  group_by(code_muni) %>%
  filter(value > 0) %>%
  mutate(
    index_pop = value / first(value) * 100
  ) %>%
  ungroup()

## Houses ------------------------------------------------------------------

# Source: Census (1970, 1980, 1991, 2000, 2010)
domi_censo <- get_sidra(
  x = 206,
  variable = 96,
  period = "1970-2010",
  geo = "City",
  geo.filter = list("City" = cities$codigo_do_ibge)
)

# Clean data
domi_censo <- domi_censo %>%
  janitor::clean_names() %>%
  filter(numero_de_comodos == "Total", situacao_do_domicilio == "Total") %>%
  mutate(date = as.Date(paste0(ano_codigo, "-01-01"))) %>%
  select(code_muni = municipio_codigo, date, value = valor)

# Source: PNAD/C
# Table 6788: Domicilios particulares permanentes 2012-2019 #
domi_pnad <- get_sidra(
  x = 6788,
  variable = 162,
  geo = "City",
  geo.filter = list("City" = cities$codigo_do_ibge),
  period = "2012-2020"
)

# Clean data
domi_pnad <- domi_pnad %>%
  janitor::clean_names() %>%
  filter(sexo_do_responsavel_pelo_domicilio == "Total",
         especie_de_unidade_domestica == "Total") %>%
  mutate(date = as.Date(paste0(ano_codigo, "-01-01")),
         valor = valor * 10^3) %>%
  select(code_muni = municipio_codigo, date, value = valor)

domi_pnad <- domi_pnad %>%
  arrange(date) %>%
  arrange(code_muni)


# Forecast values for 2020 using linear time trend
domi2020 <- lapply(split(domi_pnad, domi_pnad$code_muni),
                   function(df) {
                     x <- ts(df$value, start = c(2012))
                     yhat <- forecast(tslm(x ~ trend), h = 1)
                     #yhat <- forecast::forecast(forecast::auto.arima(x), h = 1)
                     return(as.numeric(yhat$mean))
                   })

prev2020 <- tibble(
  date = as.Date("2020-01-01"),
  code_muni = unique(domi_pnad$code_muni),
  value = unlist(domi2020)
)

domi_pnad <- rbind(domi_pnad, prev2020)

# Stack together
domi <- rbind(domi_censo, domi_pnad)
domi <- arrange(domi, code_muni)
domi <- arrange(domi, date)
# Index on first value
domi <- domi %>%
  group_by(code_muni) %>%
  filter(value > 0) %>%
  mutate(index_domi = value / first(value) * 100) %>%
  ungroup()

# Jobs --------------------------------------------------------------------
# Source: RAIS (2000/2020)
rais <- read_csv(here("data", "2022_02", "rais_capitais.csv"))


# Pop and Dom -------------------------------------------------------------

tbl_census_pop <- pop %>%
  mutate(ano = lubridate::year(date)) %>%
  filter(ano %in% c(1970, 1980, 1991, 2000, 2010, 2020)) %>%
  select(ano, code_muni, index_pop)

tbl_census_dom <- domi %>%
  mutate(ano = lubridate::year(date)) %>%
  filter(ano %in% c(1970, 1980, 1991, 2000, 2010, 2020)) %>%
  select(ano, code_muni, index_domi)

tbl_decenal <- full_join(tbl_census_pop, tbl_census_dom)
tbl_decenal <- pivot_longer(tbl_decenal, cols = -c("ano", "code_muni"))

sub_decenal <- tbl_decenal %>%
  mutate(code_muni = as.numeric(code_muni)) %>%
  rename(index = name) %>%
  left_join(geoid) %>%
  filter(!(name_muni %in% c("Boa Vista", "Palmas"))) %>%
  mutate(
    index = case_when(
      index == "index_domi" ~ "Houses",
      index == "index_jobs" ~ "Jobs",
      index == "index_pop"  ~ "People"
    )
  )

# Interpolation -----------------------------------------------------------

# Interpolates missing population years and missing household years

# Define full grid
grid <- expand_grid(ano = 2000:2020, code_muni = cities$codigo_do_ibge)

# Join grid with population
pop <- mutate(pop,
              ano = lubridate::year(date),
              code_muni = as.numeric(code_muni))

subpop <- left_join(grid, pop, by = c("ano", "code_muni"))
# Linear interpolation of missing values
subpop <- subpop %>%
  group_by(code_muni) %>%
  mutate(value = zoo::na.spline(value),
         index_pop = value / first(value) * 100) %>%
  ungroup() %>%
  select(ano, code_muni, index_pop)

# Join with household data
domi <- mutate(domi,
               ano = lubridate::year(date),
               code_muni = as.numeric(code_muni))

subdomi <- left_join(grid, domi, by = c("ano", "code_muni"))
# Impute missing values using kalman filter
imputation <- lapply(split(subdomi, subdomi$code_muni),
                     function(df) {
                       y <- ts(df$value, start = 2000, frequency = 1)
                       imp <- na_kalman(y, model = "StructTS", smooth = TRUE)
                       df$imputed <- imp
                       return(df)
                     })

subdomi_imp <- bind_rows(imputation)
# Re-index values based on imputed values
subdomi_imp <- subdomi_imp %>%
  group_by(code_muni) %>%
  mutate(index_domi = imputed / first(imputed) * 100) %>%
  ungroup() %>%
  select(ano, code_muni, index_domi)

# No need for imputation for job values
jobs <- left_join(grid, rais, by = c("ano", "code_muni"))

jobs <- jobs %>%
  arrange(ano) %>%
  arrange(code_muni) %>%
  group_by(code_muni) %>%
  mutate(index_jobs = value / first(value) * 100) %>%
  select(ano, code_muni, index_jobs)

# Merge population, houses, and jobs
data <- reduce(list(subpop, subdomi_imp, jobs), inner_join)
# Convert to long
data <- pivot_longer(data, cols = -c("ano", "code_muni"), names_to = "index")
# Join with IBGE geographic IDs
data <- left_join(data, geoid, by = "code_muni")
# Remame factors
data <- mutate(data,
               index = case_when(
                 index == "index_domi" ~ "Houses",
                 index == "index_jobs" ~ "Jobs",
                 index == "index_pop"  ~ "People"
               ),
               name_muni = factor(name_muni, levels = geoid$name_muni))


# Plots -------------------------------------------------------------------

# Create a facet line plot for each region

plot_full <- function (region_code) {
  
  df <- filter(data, code_region == region_code)
  name_region <- unique(filter(geoid, code_region == region_code)$name_region)
  title <- sprintf("Population, Houses, and Formal Jobs (%s)", name_region)
  
  p <- ggplot(df, aes(x = ano, y = value, colour = index)) +
    #geom_point() +
    geom_line(size = 1.1) +
    geom_hline(yintercept = 100, color = "gray20") +
    labs(
      x = NULL,
      y = "Index (100 = values in 2000)",
      title = title,
      subtitle = "Evolution of population, household, and formal jobs in capital cities in Brazil.",
      caption = "Source: IBGE (Censo, Estimativas da População, PNADC) e MTE (RAIS)."
    ) +
    facet_wrap(~name_muni) +
    scale_x_continuous(breaks = 2000:2020) +
    scale_color_manual(name = "", values = colors) +
    theme_vini
  
  return(p)
  
}

# Create a facet line plot for each region (only pop. and houses)

plot_historical <- function(region_code) {
  
  df <- filter(sub_decenal, code_region == region_code)
  name_region <- unique(filter(geoid, code_region == region_code)$name_region)
  title <- sprintf("Population and Houses (%s)", name_region)
  
  p <- ggplot(filter(sub_decenal, code_region == region_code),
              aes(x = ano, y = value, colour = index)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 100, color = "gray20") +
    facet_wrap(~name_muni) +
    labs(
      x = NULL,
      y = "Index (100 = values in 1970)",
      title = title,
      subtitle = "Evolution of population and household estimates in capital cities in Brazil.",
      caption = "Source: IBGE (Censo, Estimativas da População, PNADC)."
    ) +
    scale_color_manual(name = "", values = colors[c(1, 3)]) +
    theme_vini
  
  return(p)
  
}

panel <- ggplot(filter(data, code_region != 1),
       aes(x = ano, y = value, colour = index)) +
  #geom_point() +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 100, color = "gray20") +
  labs(
    x = NULL,
    y = "Index (100 = values in 2000)",
    title = "People, Houses, and Jobs",
    subtitle = "Evolution of population, household, and formal jobs in capital cities in Brazil. Omits capital cities from the northern region.",
    caption = "Source: IBGE (Censo, Estimativas da População, PNADC) e MTE (RAIS)."
  ) +
  facet_wrap(~name_muni) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  scale_y_continuous(breaks = seq(100, 260, 20)) +
  scale_color_manual(name = "", values = colors) +
  theme_vini

plots_historic <- lapply(1:5, plot_historical)
plots <- lapply(1:5, plot_full)

tab <- geoid %>%
  select(code_region, name_region) %>%
  distinct() %>%
  arrange(code_region)

height <- c(6, 6, 5.5, 5.5, 5.5)

for (i in 1:5) {
  
  name_region <- str_to_lower(tab$name_region[i])
  
  name_plot <- sprintf("people_houses_jobs_%s.png", name_region)
  name_plot_his <- sprintf("people_houses_census_%s.png", name_region)
  
  if (i == 4) {
    cowplot::save_plot(
      filename = here("graphics", "2022_02", name_plot),
      plot = plots[[i]],
      base_width = height[i]
    )
    cowplot::save_plot(
      filename = here("graphics", "2022_02", name_plot_his),
      plot = plots_historic[[i]],
      base_width = height[i]
    )
  }
  
  cowplot::save_plot(
    filename = here("graphics", "2022_02", name_plot),
    plot = plots[[i]],
    base_height = height[i]
  )
  cowplot::save_plot(
    filename = here("graphics", "2022_02", name_plot_his),
    plot = plots_historic[[i]],
    base_height = height[i]
  )
}


cowplot::save_plot(
  filename = here("graphics", "2022_02", "people_houses_jobs_north.png"),
  plot = plots[[1]],
  base_height = 6
)

cowplot::save_plot(
  filename = here("graphics", "2022_02", "people_houses_jobs_northeast.png"),
  plot = plots[[2]],
  base_height = 6
)

cowplot::save_plot(
  filename = here("graphics", "2022_02", "people_houses_jobs_southeast.png"),
  plot = plots[[3]],
  base_height = 5.5
)

cowplot::save_plot(
  filename = here("graphics", "2022_02", "people_houses_jobs_south.png"),
  plot = plots[[4]],
  base_height = 9 / 1.618
)

cowplot::save_plot(
  filename = here("graphics", "2022_02", "people_houses_jobs_midwest.png"),
  plot = plots[[5]],
  base_height = 5.5
)

plot_jobs <- ggplot(filter(data, index == "Jobs", name_muni != "Boa Vista"),
                    aes(x = ano, y = value, group = name_muni, color = name_region)) +
  geom_line(size = 1, alpha = 0.7) +
  geom_hline(yintercept = 100) +
  scale_color_manual(
    name = "",
    values = met.brewer("Hokusai1", n = 5)) +
  labs(
    x = NULL,
    y = "Index (100 = value in 2000)",
    title = "Jobs in Brazilian Capital Cities",
    subtitle = "Formal jobs growth in the capitals of Brazil.\nAlmost all cities exhibit strong growth in the 2000-2012 period, but then stagnate and/or decline.",
    caption = "Source: MTE (RAIS)."
  ) +
  scale_x_continuous(breaks = 2000:2020) +
  theme_vini

cowplot::save_plot(
  here("graphics", "2022_02", "jobs.png"),
  plot_jobs,
  base_height = 5
)

cowplot::save_plot(
  here("graphics", "2022_02", "people_houses_jobs_panel.png"),
  panel,
  base_height = 7
)
  