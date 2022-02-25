library(here)
library(lubridate)
library(MetBrewer)
library(showtext)
library(tidyverse)
library(gganimate)

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

grouped_median <- function(df, freq.var = NULL, min.var = NULL, max.var = NULL, interval = 5) {
  # L is the lower class boundary of the group containing the median
  # n is the total number of values
  # B is the cumulative frequency of the groups before the median group
  # G is the frequency of the median group
  # w is the group width
  
  freq <- df[[freq.var]]
  acum_freq <- cumsum(df[[freq.var]])
  l <- findInterval(max(acum_freq) / 2, acum_freq) + 1
  
  L <- as.numeric(df[l, min.var])
  n <- max(acum_freq)
  B <- acum_freq[l - 1]
  G <- df[l, freq.var]
  w <- interval
  
  med <- L + (((n / 2) - B) / G) * w
  return(med)
  #return(list(l = l, L = L, n = n, B = B, G = G, w = w, med = med))
}

# Data --------------------------------------------------------------------

data <- read_rds(here("data/temp_data.rds"))

tbl_mort <- data$tbl_mort
tbl_mort <- filter(tbl_mort, cause == "natural")
# Auxiliar data.frame with midpoint for each group
df_aux <- tbl_mort %>%
  select(grupo_idade) %>%
  distinct()

df_aux <- df_aux %>%
  mutate(
    a1 = as.numeric(str_trim(str_extract(grupo_idade, "[0-9]+ "))),
    a2 = as.numeric(str_trim(str_extract(grupo_idade, " [0-9]+ "))),
    a2 = ifelse(is.na(a2), 104, a2))

df_aux$midpoint <- map_dbl(apply(df_aux[, 2:3], 1, function(x) x[1]:x[2]), median)
# Join midpoint data.frame with mortality data
tbl_mort <- left_join(tbl_mort, df_aux, by = "grupo_idade")

# Compute average age of death
morte_avg <- tbl_mort %>%
  filter(a1 >= 30) %>%
  mutate(w1 = mortes * midpoint) %>%
  group_by(date) %>%
  summarise(idade_avg = sum(w1) / sum(mortes))

# Compute median age of death
morte_med <- tbl_mort %>%
  filter(a1 >= 30) %>%
  split(.$date) %>%
  map(function(df) grouped_median(df, freq.var = "mortes", min.var = "a1")) %>%
  bind_rows(.id = "date") %>%
  rename(idade_med = mortes) %>%
  mutate(date = as.Date(date))

# Join estimates into single data.frame
tbl_stats <- inner_join(morte_avg, morte_med, by = "date")
tbl_stats <- tbl_stats %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, labels = c("Average", "Median")))

# Aggregate deaths by year and age_group for gif
mort_year <- tbl_mort %>%
  mutate(year = as.integer(year)) %>%
  group_by(year, grupo_idade) %>%
  summarise(mortes_year = sum(mortes, na.rm = T)) %>%
  ungroup()

# Compare deaths in 2019 and 2020
mort_compare <- mort_year %>%
  filter(year %in% c(2019, 2020)) %>%
  pivot_longer(cols = -c(year, grupo_idade))

# Plots -------------------------------------------------------------------

cores <- met.brewer("Hokusai1")[c(2, 6)]

p1 <- ggplot(data$data, aes(x = date, y = log(value), colour = name, group = name)) +
  geom_line(size = 1) +
  scale_colour_manual(
    name = "",
    values = cores
  ) +
  scale_x_date(
    breaks = c(as.Date("2003-01-01"),
               seq(as.Date("2005-01-01"), as.Date("2020-01-01"), "5 year")),
    date_labels = "%Y"
  ) +
  labs(
    title = "Recorded Births and Deaths in Brazil",
    y = "Records (log-scale)",
    x = NULL,
    caption = "Source: IBGE") +
  theme_vini

p2 <- ggplot(tbl_stats, aes(x = date, y = value, colour = name)) +
  geom_line(size = 1) +
  scale_x_date(
    breaks = c(as.Date("2003-01-01"),
               seq(as.Date("2005-01-01"), as.Date("2020-01-01"), "5 year")),
    date_labels = "%Y"
  ) +
  scale_color_manual(
    name = "",
    values = cores
  ) +
  labs(
    title = "Age of Death in Brazil",
    y = "Median Age of Death",
    caption = "Source: IBGE"
  ) +
  theme_vini

cores2 <- met.brewer("Hokusai1", n = 9)[c(6, 2)]

p3 <- ggplot(data = mort_compare,
       aes(x = grupo_idade, y = value, fill = as.factor(year))) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0,
             color = "gray20") +
  scale_fill_manual(
    name = "Year of Death",
    values = cores2
  ) +
  scale_y_continuous(
    breaks = seq(0, 150000, 25000),
    labels = format(seq(0, 150000, 25000), big.mark = ".")
  ) +
  labs(
    title = "Deaths by Age Group (2019x2020)",
    y = "",
    x = NULL,
    caption = "Source: IBGE"
  ) +
  coord_flip() +
  theme_vini

anim <- ggplot(data = mort_year, aes(x = grupo_idade, y = mortes_year)) +
  geom_col() +
  coord_flip() +
  labs(title = "year: {frame_time}", x = NULL, y = "Obitos") +
  transition_time(year, range = c(2003L, 2020L)) +
  ease_aes("linear")


# Export ------------------------------------------------------------------

cowplot::save_plot(here("graphics/2022_02/births_deaths.png"),
                   p1,
                   dpi = 300)

cowplot::save_plot(here("graphics/2022_02/age_of_death.png"),
                   p2,
                   dpi = 300)

cowplot::save_plot(here("graphics/2022_02/age_of_death_distribution.png"),
                   p3,
                   dpi = 300)

anim_save(here("graphics/2022_02/age_of_death_distribution.gif"),
          animation = anim)

# Fitting Deaths ----------------------------------------------------------

# mort19 <- mort_year %>%
#   filter(year == 2019) %>%
#   left_join(df_aux)
# 
# mort20 <- mort_year %>%
#   filter(year == 2020) %>%
#   left_join(df_aux)
# 
# 
# x19 <- unlist(mapply(rep, mort19$midpoint, mort19$mortes_year))
# x20 <- unlist(mapply(rep, mort20$midpoint, mort20$mortes_year))
# 
# fit_pois <- fitdistrplus::fitdist(x19[x19 > 3], distr = "pois")
# summary(fit_pois)
# 
# plot(fit_pois)
# 
# library(fitdistrplus)
# descdist(x19, discrete = T)
# fit_sn <- fitdistrplus::fitdist(x19[x19 > 30], distr = "gompertz", start = list(shape = 0.0006, rate = 0.095))
# 
# hist(x19, prob = TRUE, nclass = "scott")
# plot(dgompertz(0:105, shape = 0.0006, rate = 0.095), add = TRUE)
# 
# 
# bc19 <- boxcox(x19 ~ 1)
# 
# skew_mod = selm(x19 ~ 1)
# summary(skew_mod)
# 
# hist(x19,prob=TRUE,nclass="scott") # "scott" is from MASS
# plot(function(x) dsn(x, dp=skew_mod@param$dp), from=0, to=105, col="red", add=TRUE)
# 
# library(MASS) # We will use this later
# library(car) # We use boxCox from car
# boxCox(x19 ~ 1, family="yjPower",param="gamma")
# plot(fit_gamma)
# 
# library(sn)
# skew_mod <-  selm(dat ~ 1) # selm is "skew-elliptic lm"
# summary(skew_mod)