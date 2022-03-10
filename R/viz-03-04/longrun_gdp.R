library(tidyverse)
library(here)
source(here("R/theme.R"))

# Import downloaded data from Ipeadata
longpib <- read_csv2(here("data/2022_03/ipea_pib_anual.csv"))
longpop <- read_csv2(here("data/2022_03/pop_brasil.csv"))


# Data Clean --------------------------------------------------------------

names(longpib) <- c("year", "chg_pib")
names(longpop) <- c("year", "pop")

longpib <- longpib %>%
  select(year, chg_pib) %>%
  mutate(chg_pib = chg_pib / 100,
         acum_pib = cumprod(1 + chg_pib) - 1)

longpop <- longpop %>%
  select(year, pop) %>%
  mutate(chg_pop = pop / lag(pop) - 1)

data <- left_join(longpib, longpop, by = "year")

data_decade <- data %>%
  mutate(
    chg_pibpc = chg_pib - chg_pop,
    decade = paste0(str_sub(year, 1, 3), "0'")
  ) %>%
  group_by(decade) %>%
  arrange(year) %>%
  mutate(decade_acum = cumprod(1 + chg_pibpc) - 1,
         period = as.numeric(str_sub(year, 4, 4)))

data_labels <- data_decade %>%
  filter(period == 9) %>%
  mutate(x = 9.5, y = decade_acum * 100, label = decade)

trend_pib <- data_decade %>%
  ungroup() %>%
  mutate(ma6 = RcppRoll::roll_meanr(chg_pib, n = 6)) %>%
  select(year, chg_pib, ma6) %>%
  pivot_longer(cols = -year)


# Plots -------------------------------------------------------------------

p1 <- ggplot(data_decade, aes(x = period, y = chg_pib * 100)) +
  geom_hline(yintercept = 0) +
  geom_col(fill = "#224b5e") + 
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous() +
  facet_wrap(~decade) +
  theme_vini +
  labs(
    title = "GDP per capita growth across the decades",
    subtitle = "",
    x = NULL,
    y = "GDP per capita growth (%)",
    caption = "Source: IBGE, IPEA. C") +
  theme(axis.text.x = element_text(angle = 0),
        panel.border = element_rect(color = "gray15", fill = NA))

p2 <- ggplot(data_decade, aes(x = period, y = decade_acum * 100)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = decade)) + 
  geom_label_repel(data = data_labels, aes(x = x, y = y, label = label), size = 3) +
  scale_color_manual(values = met.brewer("Hokusai1", n = 13), name = "") +
  scale_x_continuous(breaks = 0:9, limits = c(NA, 11)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_vini +
  theme(axis.text.x = element_text(angle = 0))

p3 <- ggplot(trend_pib, aes(x = year, y = value * 100, color = name)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(breaks = seq(-5, 15, 2.5)) +
  scale_color_manual(values = met.brewer("Hokusai1")[c(7, 2)]) +
  guides(color = "none") +
  labs(
    title = "Real GDP Growth in Brazil",
    x = NULL,
    y = "(%)"
  ) +
  theme_vini

p4 <- ggplot(filter(longpop, year >= 1900), aes(x = year, y = pop / 10^6)) +
  geom_line(size = 1, color = "#224b5e") +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(breaks = seq(0, 220, 20)) +
  labs(title = "Resident Population Growth",
       y = "Inhabitants (millions)",
       x = NULL,
       caption = "Source: Ipeadata (years between Census are interpolated with cubic spline)") +
  theme_vini

p5 <- longpop %>% 
  mutate(pop_diff = pop - lag(pop)) %>%
  filter(year >= 1900) %>%
  ggplot(aes(x = year, y = pop_diff / 10^6)) +
  geom_hline(yintercept = 0) +
  geom_col(fill = "#224b5e") +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) +
  labs(title = "Resident Population Growth",
       y = "Inhabitants (millions)",
       x = NULL,
       caption = "Source: Ipeadata (years between Census are interpolated with cubic spline)") +
  theme_vini
  
p6 <- ggplot(longpib, aes(x = chg_pib * 100)) +
  geom_vline(xintercept = 0) +
  geom_histogram(color = "white", fill = "#224b5e") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(-5, 15, 2.5)) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  labs(title = "Distribution of Real GDP Growth",
       x = "%",
       y = NULL,
       caption = "Source: IPEA") +
  theme_vini +
  theme(axis.text.x = element_text(angle = 0))


# Export ------------------------------------------------------------------

plots <- list(cumgdp_decade = p2,
              series_gdp_growth = p3,
              series_pop = p4,
              pop_growth = p5,
              gdp_distribution = p6)

for (i in seq_along(plots)) {
  name_file <- sprintf("%s.png", names(plots)[[i]])
  path_file <- here("graphics/2022_03", name_file)
  cowplot::save_plot(filename = path_file, plot = plots[[i]])
}

ggsave(here("graphics/2022_03/panel_decade.png"), p1, width = 6, height = 6, dpi = 300)

