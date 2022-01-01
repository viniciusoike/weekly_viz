library(tidyverse)
library(cowplot)
library(tmap)
library(sf)
library(showtext)
library(here)
showtext::showtext_auto()
showtext_opts(dpi = 300)

url <- "https://www.nossasaopaulo.org.br/wp-content/uploads/2021/08/Dados_abertos_Mapa_da_Desigualdade_2020.xls"
httr::GET(url, write_disk(here("data/mapa_desigualdade_2020.xls")))

# Disponível em https://transparencia.metrosp.com.br/dataset/pesquisa-origem-e-destino/resource/4362eaa3-c0aa-410a-a32b-37355c091075
data <- readxl::read_excel(here("data/mapa_desigualdade_2020.xls"), range = "A3:AV99")

# Clean names for 
subdata <- data %>%
  janitor::clean_names() %>%
  select(
    name_district = distrito, income = renda_media_familiar_mensal
    ) %>%
  mutate(
    name_original = name_district,
    name_district = iconv(name_district, from = "UTF-8", to = "ASCII//TRANSLIT"),
    group = ntile(income, 6)
    )

dstr <- st_read(here("data/shapes/distritos_sp.gpkg"))

# Fix geometries and select only ids
dstr <- dstr %>%
  st_make_valid() %>%
  select(code_district, name_district) %>%
  mutate(name_district = iconv(name_district, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Spatial join
dstr <- left_join(dstr, subdata, by = "name_district")

# Compute average family income
df_aux <- subdata %>%
  group_by(group) %>%
  summarise(avg = mean(income))

# Create some artifical jitter in x for visualization
subdata <- subdata %>%
  mutate(x_jitter = runif(nrow(.), min = 0.99, max = 1.01))
# Auxiliar data.frame to place text labels
df_label <- subdata %>%
  left_join(df_aux) %>%
  # Finds most "representative" district in each income group
  mutate(diff = abs(income - avg)) %>%
  group_by(group) %>%
  slice_min(diff, n = 1) %>%
  ungroup() %>%
  mutate(x = rep(c(0.98, 1.02), length.out = 6),
         xseg = x + rep(c(0.0025, -0.0025)))

# Map ---------------------------------------------------------------------

m <- tm_shape(dstr) +
  tm_fill(
    col = "income",
    style = "jenks",
    n = 7,
    title = "Renda Média Familiar",
    legend.hist = TRUE,
    palette = "viridis",
    legend.format = list(fun = function(x) format(round(x, -2), big.mark = "."))
  ) +
  tm_borders(col = "gray90", lwd = 1.5) +
  tm_layout(frame = F)


# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_hline(
    yintercept = subdata %>% pull(income) %>% mean(),
    linetype = 2,
    size = 1) +
  geom_text(
    data = df_label,
    aes(x = x,
        y = income,
        label = name_original),
    size = 2,
    family = "Montserrat"
  ) +
  geom_segment(
    data = df_label,
    aes(x = xseg, xend = x_jitter, y = income, yend = income),
    size = 1
  ) +
  geom_point(data = subdata, aes(x = x_jitter, y = income),
             size = 3,
             alpha = 0.5,
             colour = "navyblue") +
  geom_label(
    data = tibble(
      x = 1.02,
      y = 6000,
      label = sprintf("Renda média = R$%s",
                      format(subdata %>%
                               pull(income) %>%
                               mean() %>%
                               round(-2),
                             big.mark = "."))),
    aes(x = x, y = y, label = label),
    family = "Montserrat",
    size = 3
    ) +
  scale_x_continuous(limits = c(0.975, 1.025)) +
  scale_y_continuous(
    breaks = 3:9 * 10^3,
    labels = scales::number_format(big.mark = ".")
  ) +
  labs(
    title = "Renda Média Familiar por Distrito",
    caption = "Fonte: Mapa da Desigualdade 2020 / POD (2017) atualizada pelo IPC-SP",
    subtitle = "Destaque para distritos selecionados",
    x = NULL,
    y = "Renda Média Familia (R$)") + 
  coord_flip() +
  theme_half_open(font_size = 12, font_family = "Montserrat") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  background_grid()

tmap_save(m, here("mapa_renda.png"))
save_plot(plot = p, filename = here("grafico_renda.png"), dpi = 300)