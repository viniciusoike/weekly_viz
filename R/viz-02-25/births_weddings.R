library(here)
library(lubridate)
library(MetBrewer)
library(showtext)
library(tidyverse)
library(RcppRoll)

font_add("Helvetica", "Helvetica.ttc")
showtext_auto()

colors <- met.brewer("Hokusai1", 7)[c(2, 5, 7)]

colors <- met.brewer("Hokusai1", 7)[c(6, 7, 5)]

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

data <- read_rds(here("data/temp_data.rds"))

rates <- data$data

tbl_rates <- rates %>%
  mutate(name_state = factor(name_state),
         name_state = fct_reorder(name_state, code_state)) %>%
  select(date, name_state, b_rate, w_rate) %>%
  pivot_longer(cols = -c(date, name_state)) %>%
  group_by(name_state, name) %>%
  mutate(ma = roll_meanr(value, n = 12))


tbl1 <- data$births

tbl1 <- tbl1 %>%
  filter(!is.na(date),
         !(mother_age_group %in% c("Total", "Ignorada", "Menos de 15 anos")),
         !str_detect(mother_age_group, " a "),
         sex_children_born == "total") %>%
  select(date, name_state, mother_age_group, total_births) %>%
  mutate(age_mother = as.numeric(str_extract(mother_age_group, "[:digit:]+")),
         total_births = ifelse(is.na(total_births), 0, total_births)
         )

tbl_avg_birth <- tbl1 %>%
  group_by(date, name_state) %>%
  summarise(avg_age_b = weighted.mean(age_mother, total_births, na.rm = T))

tbl2 <- data$weddings

tbl2 <- tbl2 %>%
  group_by(date, name_state, age_female) %>%
  summarise(wedding = sum(total_weddings, na.rm = T)) %>%
  ungroup() %>%
  mutate(ages = str_remove(age_female, " anos")) %>%
  separate(ages, into = c("age_min", "age_max"), sep = " a ") %>%
  mutate(age_max = ifelse(age_female == "65 anos ou mais", 70, age_max),
         age_max = ifelse(age_female == "Menos de 15 anos", 15, age_max),
         age_min = ifelse(age_female == "65 anos ou mais", 65, age_min),
         age_min = ifelse(age_female == "Menos de 15 anos", 15, age_min),
         age_min = as.numeric(age_min),
         age_max = as.numeric(age_max),
         midpoint = age_min + (age_max - age_min) / 2)

tbl_avg_wedding <- tbl2 %>%
  group_by(date, name_state) %>%
  summarise(avg_age_w = weighted.mean(midpoint, wedding))

tbl_avg <- inner_join(tbl_avg_birth, tbl_avg_wedding, by = c("name_state", "date"))

tbl_avg <- tbl_avg %>%
  pivot_longer(cols = -c(date, name_state)) %>%
  group_by(name_state, name) %>%
  mutate(ma6 = roll_meanr(value, n = 6)) %>%
  ungroup() %>%
  mutate(
    name_state = str_to_title(name_state),
    name_state = factor(name_state, levels = levels(tbl_rates$name_state))
  )


# Plots -------------------------------------------------------------------


p1 <- ggplot(tbl_rates, aes(x = date, y = ma, colour = name)) +
  geom_line(size = 1) +
  facet_wrap(~name_state) +
  scale_colour_manual(
    name = "",
    values = colors,
    labels = c("Births", "Weddings")
  ) + 
  labs(
    title = "While wedding rates stabilize, birth rate falls",
    subtitle = "12 month moving average shows steady decline in the birth rate.\nWedding rates stay roughly equal until the Covid-19 pandemic.",
    y = "Rate per 100.000",
    x = NULL,
    caption = "Source: IBGE"
  ) + 
  theme_vini

p2 <- ggplot(tbl_avg, aes(x = date, y = ma6, colour = name)) +
  geom_line(size = 1) +
  facet_wrap(~name_state) +
  scale_y_continuous(
    breaks = seq(24, 34, 2)) +
  scale_colour_manual(
    name = "Average age",
    values = colors[c(1, 3)],
    labels = c("Wedding", "Children")
  ) +
  labs(
    title = "Women are having children and marrying at older ages",
    subtitle = "Average age of wedding lags significantly behind even when including\nweddings from formerly divorced of widowed.",
    x = NULL,
    y = "Age",
    caption = "Source: IBGE"
  ) +
  theme_vini

cowplot::save_plot(
  here("graphics", "2022_02", "births_weddings.png"),
  p1,
  base_height = 7
)

cowplot::save_plot(
  here("graphics", "2022_02", "births_weddings_avg_age.png"),
  p2,
  base_height = 7
)

