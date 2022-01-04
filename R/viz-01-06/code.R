
# 0. Preamble -------------------------------------------------------------


library(tidyverse)
library(ggfx)
library(here)
library(rvest)
library(ghibli)
library(MetBrewer)
library(showtext)

showtext_auto()

font_add("Gill Sans", "GillSans.ttc")
# Theme

theme_vini <- theme(
  # Font e tamanho
  text = element_text(family = "Gill Sans", size = 12, colour = "gray10"),
  
  # Fundo do gráfico
  #panel.grid = element_blank(),
  
  # Define as margens do gráfico
  plot.margin = unit(c(1, 1, .5, 1), "cm"),
  
  # Eixos
  axis.text.y = element_text(vjust = .4),
  axis.ticks = element_line(size = .4),
  axis.text = element_text(family = "Helvetica", size = 8, colour = "gray10"),
  
  # Legenda
  legend.position = "top"
)

theme_vini_grid <- theme_vini +
  theme(
    panel.grid.major = element_line(linetype = 2, colour = "gray65")
  )


colors <- ghibli_palettes$PonyoMedium
colors2 <- met.brewer(name = "Hiroshige")

# Get list of LA countries from wikipedia
wiki <- "https://en.wikipedia.org/wiki/Latin_Americans"
wiki_html <- read_html(wiki)
tbl <- html_table(wiki_html)[[2]]

# Import GDP Per Capita Annual Change from WB
gdp <- WDI(indicator = "NY.GDP.PCAP.KD.ZG", start = 1960)
# Import GDP Per Capita from WB
gdppc <- WDI(indicator = "NY.GDP.PCAP.KD", start = 1960)

# Aggregates
aggs <- c("World", "Latin America & Carribean", "Low Income", "Middle Income",
          "Low Income")
# Latin America Countries
latin_america <- tbl$Country

# Check if name of countries is compatible
all(latin_america %in% gdp$country)
latin_america[!(latin_america %in% gdp$country)]

# Adjust gdp data.frame with dummies
gdppc <- gdppc %>%
  as_tibble() %>%
  rename(gdppc = `NY.GDP.PCAP.KD`) %>%
  mutate(
    country = if_else(country == "Venezuela, RB", "Venezuela", country),
    lamerica = if_else(country %in% latin_america, 1, 0),
    aggregate = if_else(country %in% aggs, 1, 0)
  )

# Left join with USA average to compute GAP
usa_avg <- gdppc %>%
  filter(country == "United States") %>%
  select(year, usa = gdppc)

gdppc <- gdppc %>%
  filter(lamerica == 1) %>%
  left_join(usa_avg) %>%
  mutate(gap = (gdppc / usa) * 100)

# Adjust gdp data.frame with dummies
gdp <- gdp %>%
  as_tibble() %>%
  rename(gdppc = `NY.GDP.PCAP.KD.ZG`) %>%
  mutate(
    country = if_else(country == "Venezuela, RB", "Venezuela", country),
    lamerica = if_else(country %in% latin_america, 1, 0),
    aggregate = if_else(country %in% aggs, 1, 0)
  )

# Left join with world average to compute deviations
gdp_avg <- gdp %>%
  filter(country == "World") %>%
  select(year, world_avg = gdppc)

gdp <- gdp %>%
  left_join(gdp_avg, by = "year") %>%
  mutate(
    deviation = (gdppc - world_avg) / world_avg,
    growth = if_else(gdppc > 0, 1, 0)
  )

# Select only observations from LATAM
latam <- gdp %>%
  filter(lamerica == 1 | aggregate == 1)
# Truncate deviation using boxplot
outlier <- boxplot.stats(latam$deviation)$stats[c(1, 5)]

latam <- gdp %>%
  mutate(
    deviation_trunc = if_else(deviation > max(outlier), max(outlier), deviation),
    deviation_trunc = if_else(deviation < min(outlier), min(outlier), deviation_trunc)
  )

# Filter only Brazil and create cycle variable
bra <- gdp %>%
  filter(country == "Brazil") %>%
  mutate(cycle = if_else(year %in% 2000:2012, "Commodity Boom", "Other"))

# Compute average and median growth
med_bra <- median(gdp$gdppc, na.rm = T)
avg_bra <- mean(bra$gdppc, na.rm = T)

# Panel
df_panel <- latam %>%
  filter(aggregate == 1 | lamerica == 1) %>%
  mutate(
    country = factor(country),
    country = fct_rev(country)
  )

# 2. Plots ----------------------------------------------------------------


## 2.1 Histogram -----------------------------------------------------------

p1 <- ggplot(data = filter(gdp, country == "Brazil"),
       aes(x = gdppc)) +
  geom_histogram(
    colour = "white",
    fill = colors[3],
    bins = 10
    ) +
  geom_hline(yintercept = 0, colour = colors[1]) +
  geom_vline(xintercept = med_bra, colour = colors[1]) +
  scale_y_continuous(
    breaks = seq(0, 15, 2.5,)
  ) +
  scale_x_continuous(
    breaks = seq(-10, 12, 2)
  ) +
  labs(
    title = "Annual Per Capita GPD Growth in Brazil (1960/2020)",
    x = "(%)",
    y = NULL,
    subtitle = "Distribution of annual GPD growth in Brazil. Median value is equal to 2,12%."
  ) +
  theme_vini +
  theme(
    panel.grid.major.y = element_line(linetype = 2, colour = "gray75"),
    panel.grid.minor.y = element_line(linetype = 2, colour = "gray75"),
    panel.background = element_rect(fill = "white")
  )


## 2.2 Density -------------------------------------------------------------

p2 <- ggplot(
  data = filter(bra, year >= 1980),
  aes(x = gdppc, colour = cycle, fill = cycle)
) +
  geom_density(
    size = 1.1,
    alpha = 0.5
    ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(
    breaks = seq(-10, 12, 2)
  ) +
  scale_colour_manual(
    name = "",
    values = colors[c(3, 6)]
  ) +
  scale_fill_manual(
    name = "",
    values = colors[c(3, 6)]
  ) +
  labs(
    y = NULL,
    x = "(%)"
  ) +
  theme_vini +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(linetype = 2, colour = "gray75"),
    panel.grid.major.y = element_line(linetype = 2, colour = "gray75"),
    panel.background = element_rect(fill = "white")
  )

## 2.3 Panel ---------------------------------------------------------------

p3 <- ggplot(
  data = df_panel,
  aes(x = year, y = country)
  ) +
  geom_tile(
    aes(fill = factor(growth))
    ) +
  geom_vline(
    xintercept = seq(1960, 2020, 10),
    linetype = 2,
    colour = "gray75") +
  scale_fill_manual(
    name = "",
    values = colors2[c(1, 7)],
    labels = c("Recession", "Growth"),
    na.value = "gray90",
    na.translate = F
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Growth/Recession"
  ) +
  scale_x_continuous(breaks = seq(1960, 2020, 10),
                     expand = c(0, 0),
                     sec.axis = dup_axis()) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  )


# 2.4 Panel ---------------------------------------------------------------

p4 <- ggplot(
  data = filter(df_panel, country != "World"),
  aes(x = year, y = country)) +
  geom_tile(
    aes(fill = deviation_trunc)
  ) +
  scale_fill_gradientn(
    breaks = seq(-3, 3, 1),
    colours = colors2[c(1, 3, 5, 6, 8)],
    values = scales::rescale(c(
      -1, 0 - .Machine$double.eps, 0, 0 + .Machine$double.eps, 1
    )
  )) +
  labs(
    x = NULL,
    y = NULL,
    title = "Growth Compared to World Average",
    subtitle = ""
  ) +
  scale_x_continuous(breaks = seq(1960, 2020, 10),
                     expand = c(0, 0),
                     sec.axis = dup_axis()) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  )




vec <- c("Argentina", "Brazil", "Chile", "Puerto Rico", "Mexico")

p5 <- ggplot() +
  with_blur(
  geom_line(
    data = gdppc,
    aes(x = year, y = gap, group = country),
    colour = "gray60")
  ) +
  geom_line(
    data = filter(gdppc, country %in% vec),
    aes(x = year, y = gap, colour = country),
    size = 1
  ) +
  scale_y_continuous(
    breaks = seq(0, 60, 10)
  ) +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  scale_colour_manual(
    name = "",
    values = met.brewer("Juarez", n = 5)
  ) + 
  labs(
    title = "GDP Per Capita Gap in relation to USA",
    subtitle = "Over the 60 year period, Puerto Rico closed the gap the most (+16,6) while Argentina lost track (-19.1).\nBrazil started catching up in the 1970s but stagnated and has barely changed in the overall period (+0.3)",
    x = NULL,
    y = "Country GDPpc / USA GDPpc"
  ) +
  theme_vini +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(linetype = 2, colour = "gray75"),
    panel.grid.minor.x = element_blank(),
    
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom"
  )

plots <- list(p1, p2, p3, p4, p5)

for (i in seq_along(plots)) {
  name_file <- sprintf("p%s.png", i)
  path_file <- here::here("graphics", "2022_01", name_file)
  cowplot::save_plot(path_file, plots[[i]])
}

path <- here::here("graphics", "2022_01", "")
cowplot::save_plot(glue::glue("{path}.pdf"), p, device = cairo_pdf)

