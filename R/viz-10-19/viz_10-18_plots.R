library(tidyverse)
library(cowplot)
library(RcppRoll)
library(showtext)

showtext_opts(dpi = 300)
showtext_auto()

data <- read_csv(here("data/2021_10/skyscraper_webscrape.csv"))
colors <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")

data <- data %>%
  mutate(
    floors = as.numeric(floors),
    year = as.numeric(year),
    decade = round(year, - 1)
  )

avg_hf <- data %>%
  select(floors, height) %>%
  na.omit() %>%
  summarise(x = mean(height / floors)) %>%
  pull(x)

data <- data %>%
  mutate(
    height_est = ifelse(is.na(height) & !is.na(floors),
                        floors * local(avg_hf),
                        height)
  )

# Summarise information by year
data_year <- data %>%
  group_by(year) %>%
  summarise(avg_floor = mean(floors, na.rm = T),
            avg_height = mean(height, na.rm = T),
            max_height = max(height, na.rm = T),
            avg_height_est = mean(height_est, na.rm = T),
            max_height_est = max(height_est, na.rm = T),
            count = n()) %>%
  filter(year >= 1950)

# Smooth data by year
data_year_smooth <- data_year %>%
  pivot_longer(cols = -year) %>%
  group_by(name) %>%
  mutate(interp = forecast::na.interp(value),
         mm5 = roll_meanr(interp, n = 5)
         #mm5 = ifelse(year >= 1955, forecast::na.interp(mm5), value)
         #mm5 = ifelse(year >= 1955, zoo::na.spline(value), value)
         ) %>%
  select(-interp) %>%
  ungroup()

# Summarise information by decade
data_dec <- data %>%
  group_by(decade) %>%
  summarise(avg_floor = mean(floors, na.rm = T),
            avg_height = mean(height, na.rm = T),
            max_height = max(height, na.rm = T),
            count = n()) 

theme_vini <- theme_minimal() +
  theme(
    text = element_text(family = "Roboto", size = 10),
    plot.title = element_text(size = 12),
    axis.text.x = element_text(angle = 90),
    
    panel.grid.minor.y = element_blank()
    )

# Plots -------------------------------------------------------------------

# 1. Average number of floors by year/decade ------------------------------

p1 <- data_year_smooth %>%
  filter(name == "avg_floor") %>%
  select(-name) %>%
  pivot_longer(cols = -"year") %>%
  ggplot(aes(x = year, y = value, colour = name)) +
  geom_line(size = 1) +
  scale_colour_manual(values = colors[c(5, 1)]) +
  scale_x_continuous(
    breaks = seq(1910, 2020, 10)
  ) +
  scale_y_continuous(
    breaks = seq(10, 40, 5)
  ) +
  labs(
    x = "Year",
    y = "Average Number of Floors",
    title = "Average Skyscraper Number of Floors",
    subtitle = "Moving-average trend",
    caption = "Source: https://skyscraperpage.com. @viniciusoike"
  ) +
  guides(colour = "none") +
  theme_vini


# 2. Number of skyscrapers by year/decade --------------------------------

p2 <- ggplot(data = filter(data_dec, !is.na(decade)),
       aes(x = decade, y = count)) +
  geom_col(fill = colors[2], colour = "gray80") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(
    breaks = seq(1910, 2020, 10)
  ) +
  scale_y_continuous(
    breaks = seq(0, 200, 20)
  ) +
  labs(
    x = "Decade",
    y = "Number of skyscrapers built",
    title = "Number of Skyscrapers Built in SP by Decade"
  ) +
  theme_vini +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# 3. Average height by year -----------------------------------------------

p3 <- data_year_smooth %>%
  filter(name == "avg_height_est") %>%
  select(-name) %>%
  pivot_longer(cols = -"year") %>%
  ggplot(aes(x = year, y = value, colour = name)) +
  geom_line(size = 1) +
  scale_colour_manual(values = colors[c(5, 1)]) +
  scale_x_continuous(
    breaks = seq(1910, 2020, 10)
  ) +
  scale_y_continuous(
    breaks = seq(200, 550, 50)
  ) +
  labs(
    x = "Year",
    y = "Average Height (meters)",
    title = "Average Skyscraper Height (meters)",
    subtitle = "Moving-average trend",
    caption = "Source: https://skyscraperpage.com. @viniciusoike"
  ) +
  guides(colour = "none") +
  theme_vini

# 4. Maximum height by year -----------------------------------------------

p4 <- data_year_smooth %>%
  filter(name == "max_height_est") %>%
  select(-name) %>%
  pivot_longer(cols = -"year") %>%
  ggplot(aes(x = year, y = value, colour = name)) +
  geom_line(size = 1) +
  scale_colour_manual(values = colors[c(5, 1)]) +
  scale_x_continuous(
    breaks = seq(1910, 2020, 10)
  ) +
  scale_y_continuous(
    breaks = seq(200, 550, 50)
  ) +
  labs(
    x = "Year",
    y = "Average Height (meters)",
    title = "Average Skyscraper Height (meters)",
    caption = "Source: https://skyscraperpage.com. @viniciusoike"
  ) +
  guides(colour = "none") +
  theme_vini

# 5. Height histogram -----------------------------------------------------

p5 <- ggplot(data, aes(x = height)) +
  geom_histogram(colour = "white",
                 fill = colors[1]) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 600, 50)) +
  labs(
    x = "Height (meters)",
    y = "Count",
    title = "Distribution of Skyscraper Height"
  ) +
  theme_vini

# 6. Mix of Use -----------------------------------------------------------

data_type_use <- data %>%
  mutate(
    type_simplified = ifelse(is.na(type_simplified), "Other", type_simplified)
  ) %>%
  group_by(decade, type_simplified) %>%
  summarise(count = n()) %>%
  group_by(decade) %>%
  mutate(part = 100 * count / sum(count)) %>%
  filter(decade >= 1930)

p6 <- ggplot(data_type_use, aes(x = decade, y = count, fill = type_simplified)) +
  geom_col() +
  scale_x_continuous(
    breaks = seq(1910, 2020, 10)
  ) +
  scale_y_continuous(
    breaks = seq(0, 200, 20)
  ) +
  labs(
    x = "Decade",
    y = "Number of skyscrapers built",
    title = "Number of Skyscrapers Built in SP by Decade"
  ) +
  scale_fill_manual(
    name = "Building Uses",
    values = rev(colors)
  ) +
  theme_vini +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

df_aux <- as.data.frame(
  expand.grid(unique(data_type_use$decade),
              unique(data_type_use$type_simplified))
  )

df_aux <- df_aux %>%
  rename(decade = Var1, type_simplified = Var2) %>%
  left_join(data_type_use) %>%
  mutate(part = ifelse(is.na(part), 0, part))

p7 <- ggplot(df_aux, aes(x = decade, y = part, fill = type_simplified)) +
  geom_area() +
  scale_fill_manual(
    name = "Building Uses",
    values = rev(colors)
  ) +
  theme_vini +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

library(patchwork)

(p6 + theme(legend.position = "bottom") + p5) / (p1 + theme(plot.caption = element_blank()) + p3)
