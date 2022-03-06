library(ggplot2)
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