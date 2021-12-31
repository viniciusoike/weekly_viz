# Load libraries
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
# Add Georgia font (on Windows 10)
font_add("Georgia", regular = "georgia.ttf")

# Read data file
data <- read_csv("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Temperature_Anomalies_over_Land_and_over_Ocean/graph.csv", skip = 1)

data <- data %>%
  janitor::clean_names()

subdata <- data %>%
  select(year, land_annual, lowess_5_3) %>% 
  pivot_longer(-year)

# Auxliar data.frame for labels and title
df_aux_title = data.frame(x = 1930, y = 0, label = "The Climate Issue")

df_aux_anos = data.frame(
  label = c(1880, 1920, 1960, 2000, 2020),
  x = c(1888, 1925, 1960, 1995, 2015)
)

# Plot 1: line plot -------------------------------------------------------

p1 <- ggplot(
  data, 
  aes(x = year, y = land_annual)) +
  geom_hline(yintercept = 0) +
  geom_line(colour = "#264653") +
  geom_point(
    size = 1,
    colour = "#264653") +
  geom_line(
    data = data,
    aes(x = year, y = lowess_5_3),
    size = 1,
    colour = "#CB1B16") +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  labs(
    x = NULL,
    y = "Temperatura (desvios em relação à média de 1951-80)",
    title = "Anomalias de Temperatura",
    subtitle = "Suavização LOESS",
    caption = "Fonte: NASA (https://data.giss.nasa.gov/gistemp/graphs/)"
  ) +
  theme_bw() +
  theme(text = element_text(family = "Roboto", size = 8),
        plot.title = element_text(family = "Roboto", size = 12))


# Plot 2: Economist Plot --------------------------------------------------

p2 <- ggplot() +
  geom_tile(
    data = data,
    aes(x = year, y = 0, fill = land_annual)
    ) +
  geom_text(
    data = df_aux_anos,
    aes(x = x, y = 0, label = label),
    vjust = 1.5,
    colour = "white",
    size = 5,
    family = "Georgia"
  ) +
  geom_text(
    data = df_aux_title,
    aes(x = 1950, y = 0., label = label),
    family = "Georgia",
    size = 10,
    colour = "white"
  ) +
  geom_hline(yintercept = 0,
             colour = "white",
             size = 1) +
  scale_fill_gradientn(
    colours = c("#033270", "#9DCEE2", "#FFFFFF", "#F5A6A3", "#CB1B16"),
    values = scales::rescale(c(
      -1, 0 - .Machine$double.eps, 0, 0 + .Machine$double.eps, 1
    ))
  ) +
  guides(fill = "none") +
  labs(x = "", y = "") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.margin = margin(c(0, 0, 0, 0))
  )


# Export ------------------------------------------------------------------

cowplot::save_plot(filename = here::here("post/tt-10-05/p1.png"), plot = p1, dpi = 300)
ggsave(filename = here::here("post/tt-10-05/p2.png"), plot = p2, dpi = 300, width = 1920, height = 1080, units = "px")