library(geobr)
library(sf)
library(here)
library(tidyverse)
library(rmapshaper)
library(RColorBrewer)
library(MetBrewer)
library(showtext)
library(ggrepel)
library(ggtext)

font_add("Gill Sans", "GillSans.ttc")
font_add("Helvetica", "Helvetica.ttc")
showtext_auto()
showtext_opts(dpi = 300)

theme_vini <- theme(
  
  text = element_text(family = "Helvetica", size = 10, colour = "gray15"),
  plot.title = element_text(size = 14),
  plot.subtitle = element_text(size = 8, colour = "gray30"),
  plot.caption = element_text(size = 7, colour = "gray30"),
  
  
  legend.title.align = 0.5,
  legend.title = element_text(size = 8, colour = "gray15"),
  
  legend.position = "bottom",
  panel.background = element_rect(fill = "white"),
  
  panel.grid.major.x = element_line(colour = "gray75", size = 0.5, linetype = 2),
  axis.ticks = element_blank()
)


theme_map <- theme_void() + 
  theme(
    # Adjust plot margins
    plot.margin = margin(t = 0.5, b = 0.5, unit = "in"),
    
    # Text, title and subtitle
    
    text = element_text(family = "Gill Sans", size = 10, colour = "gray10"),
    strip.text = element_text(size = 14, hjust = 0.5),
    plot.title = element_text(
      face = "bold", size = 16, hjust = 0.5, colour = "black"
    ),
    plot.subtitle = element_text(size = 8, hjust = 0.5, colour = "gray20"),
    plot.caption = element_text(size = 6),
    # Legend
    legend.title.align = 0.5,
    legend.margin = margin(0, unit = "pt"),
    legend.box.margin = margin(0, unit = "pt"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = NA, fill = NA),
    
    # Background
    panel.background = element_rect(fill = "#fff7bc", color = "#fff7bc"),
    plot.background = element_rect(fill = "#fff7bc", color = "#fff7bc"),
  )

get_jenk_breaks <- function(x, k) {
  
  j <- BAMMtools::getJenksBreaks(x, k = k)
  rank <- findInterval(x, j)
  
  return(rank)
}


munis <- read_municipality()
munis <- mutate(munis, code_muni = as.character(code_muni))
geostate <- read_state()

data <- read_csv(here("data/2022_01/pib_clean.csv"))

pib_share <- data %>%
  filter(ano == 2019) %>%
  select(code_muni, pib) %>%
  mutate(pib_perc = pib / sum(pib)) %>%
  arrange(desc(pib_perc)) %>%
  mutate(pib_acum = cumsum(pib_perc))

limit50 <- pib_share %>%
  mutate(dist = abs(pib_acum - 0.5)) %>%
  slice_min(dist) %>%
  pull(pib)

limit90 <- pib_share %>%
  mutate(dist = abs(pib_acum - 0.9)) %>%
  slice_min(dist) %>%
  pull(pib)

pib50 <- pib_share %>%
  filter(pib >= limit50) %>%
  pull(code_muni)

pib50 <- pib_share %>%
  filter(pib >= limit90) %>%
  pull(code_muni)

library(sf)

points <- munis %>%
  st_make_valid() %>%
  st_centroid()




# Maps --------------------------------------------------------------------

## Map 1 -------------------------------------------------------------------

# Library
library(cartography)
library(sp)

# Upload data attached with the package.
data(nuts2006)

# Now we have a geospatial object called nuts2.spdf containing the shape of european regions. We can plot it with the plot function.
# summary(nuts2.spdf)

# We also have a dataframe with information concerning every region.
# head(nuts2.df)
# Both object have a first column "id" that makes the link between them.

# Plot Europe
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")
plot(world.spdf, col = "#E3DEBF", border = NA, add = TRUE)
plot(nuts0.spdf, col = "#D1914D", border = "grey80",  add = TRUE)

# Add circles proportional to the total population
propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df,
                 var = "pop2008", symbols = "circle", col = "#920000",
                 legend.pos = "right", legend.title.txt = "Total\npopulation (2008)",
                 legend.style = "c")

# Add titles, legend...
layoutLayer(title = "Countries Population in Europe",
            author = "cartography", sources = "Eurostat, 2008",
            scale = NULL, south = TRUE)
