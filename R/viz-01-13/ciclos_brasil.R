library(tidyverse)
library(GetBCBData)
library(MetBrewer)
library(showtext)
library(here)

showtext_auto()
showtext_opts(dpi = 300)
font_add_google("Roboto", "Roboto")

theme_vini <- theme_minimal() +
  theme(
    text = element_text(family = "Roboto", size = 8, colour = "gray15"),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8, colour = "gray30"),
    plot.caption = element_text(size = 7, colour = "gray30"),
    legend.title.align = 0.5,
    legend.title = element_text(size = 8, colour = "gray15"),
    
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"),
    legend.box.margin = margin(0),
    legend.margin = margin(0)
  )


code_series <- 22105:22115
code_series <- code_series[-8]
series <- gbcbd_get_series(code_series, first.date = as.Date("1995-01-01"))

dictionary <- tibble(
  id.num = code_series,
  series_name = c("agropecuaria", "industria", "servicos", "valor_adicionado",
                  "pib_precos_mercado", "consumo", "governo", "fbkf",
                  "exportacao", "importacao")
)

series <- left_join(series, dictionary, by = "id.num")
series <- arrange(series, ref.date)


ggplot(series, aes(x = ref.date, y = value)) +
  geom_line() +
  facet_wrap(~series_name)

series <- series %>%
  mutate(
    recession = case_when(
      ref.date >= as.Date("1995-01-01") & ref.date <= as.Date("1995-07-01") ~ "1995Q2-Q3",
      ref.date >= as.Date("1997-10-01") & ref.date <= as.Date("1999-01-01") ~ "1998Q1-1999Q1",
      ref.date >= as.Date("2001-01-01") & ref.date <= as.Date("2001-10-01") ~ "2001Q2-Q4",
      ref.date >= as.Date("2008-07-01") & ref.date <= as.Date("2009-01-01") ~ "2008Q4-2009Q1",
      ref.date >= as.Date("2014-01-01") & ref.date <= as.Date("2016-10-01") ~ "2014Q2-2016Q4",
      ref.date >= as.Date("2019-04-01") ~ "2020Q1-",
      TRUE ~ ""
    )
  )

pib <- filter(series, series_name == "pib_precos_mercado")

df <- series %>%
  filter(series_name == "pib_precos_mercado", recession != "") %>%
  group_by(recession) %>%
  mutate(new_index = value / first(value))

df <- df %>%
  group_by(recession) %>%
  mutate(period = row_number() - 1)




p1 <- ggplot(data = df, aes(x = period, y = new_index * 100)) +
  geom_hline(yintercept = 100) +
  geom_line(aes(colour = recession)) +
  geom_point(aes(colour = recession)) +
  geom_text(
    data = data.frame(x = 1.75,
                      y = 92,
                      label = "A recessão do Covid\nfoi a maior queda relativa\ndo PIB na história recente."),
    aes(x = x, y = y, label = label),
    family = "Roboto",
    size = 2,
    colour = met.brewer("Cross", n = 5)[5]
  ) +
  geom_text(
    data = data.frame(x = 9, y = 95, label = "A recessão do 2º governo\nDilma foi a mais longa no\n período registrado"),
    aes(x = x, y = y, label = label),
    family = "Roboto",
    size = 2,
    colour = met.brewer("Cross", n = 5)[4]
  ) +
  scale_colour_manual(
    name = "Início/fim\nda recessão",
    values = met.brewer("Cross", n = 5) 
  ) +
  labs(
    title = "Ciclos de Recessão no Brasil (1995-2021)",
    x = "Trimestres após início da recessão",
    y = "Índice normalizado",
    subtitle = "Índice normalizado ao valor do PIB (a preços de mercado) imediatamente anterior ao início da recessão.\nA datação das recessões segue o CODACE (FGV).",
    caption = "Fonte: IBGE e CODACE. Cores: MetBrewer (Cross). Autor: @viniciusoike"
  ) +
  scale_y_continuous(breaks = seq(80, 100, 1)) +
  scale_x_continuous(breaks = seq(0, 20, 1)) +
  theme_vini



recession_start <- c("1997-10-01", "2001-01-01", "2008-07-01")

pib <- series %>%
  filter(series_name == "pib_precos_mercado") %>%
  arrange(ref.date)

ls <- list()

for (i in seq_along(recession_start)) {
  
  start <- lubridate::ymd(recession_start[i])
  
  df <- pib %>%
    filter(ref.date >= start) %>%
    mutate(new_index = value / first(value))
  
  end <- df %>%
    mutate(ind = ifelse(new_index > 1.005, 1, 0)) %>%
    filter(ind == 1) %>%
    slice(1) %>%
    pull(ref.date)
  
  out <- df %>%
    filter(ref.date <= lubridate::ymd(end))
  
  ls[[i]] <- out
  
}

cycles <- bind_rows(ls)

cycles <- cycles %>%
  mutate(recession = ifelse(recession == "", NA, recession)) %>%
  fill(recession, .direction = "down")

pib14 <- pib %>%
  filter(ref.date >= as.Date("2014-01-01")) %>%
  mutate(new_index = value / first(value),
         recession = "2014Q2-2016Q4") 

cycles <- rbind(cycles, pib14)

cycles <- cycles %>%
  group_by(recession) %>%
  mutate(period = row_number() - 1)


p2 <- ggplot(data = cycles, aes(x = period, y = new_index * 100)) +
  geom_hline(yintercept = 100) +
  geom_line(aes(colour = recession)) +
  geom_point(aes(colour = recession)) +
  geom_text(
    data = data.frame(x = 13, y = 101, label = "A economia brasileira levou 2 anos para\nse recuperar totalmente da Crise de 1998."),
    aes(x = x, y = y, label = label),
    family = "Roboto",
    size = 2,
    colour = met.brewer("Cross", n = 4)[1]
  ) +
  geom_text(
    data = data.frame(x = 14, y = 90, label = "Mesmo após 7 anos, a economia\n continua abaixo do nível pré-crise de 2014."),
    aes(x = x, y = y, label = label),
    family = "Roboto",
    size = 2,
    colour = met.brewer("Cross", n = 4)[4]
  ) +
  scale_colour_manual(
    name = "Início/fim\nda recessão",
    values = met.brewer("Cross", n = 4) 
  ) +
  labs(
    title = "Recessão e Recuperação (1996-2021)",
    x = "Trimestres após início da recessão",
    y = "Índice normalizado",
    subtitle = "Índice normalizado ao valor do PIB (a preços de mercado) imediatamente anterior ao início da recessão.\nA datação das recessões segue o CODACE (FGV).",
    caption = "Fonte: IBGE e CODACE. Cores: MetBrewer (Cross). Autor: @viniciusoike"
  ) +
  scale_y_continuous(breaks = seq(80, 102, 1),
                     limits = c(86, 102)) +
  scale_x_continuous(breaks = seq(0, 40, 1)) +
  theme_vini



cowplot::save_plot(here("graphics", "2022_01", "recessores_brasil.png"), p1, dpi = 300)
cowplot::save_plot(here("graphics", "2022_01", "queda_e_recuperacao_brasil.png"), p2, dpi = 300)

ggsave(here("graphics", "2022_01", "recessores_brasil_cairo.png"), p1, type = "cairo",
       height = 3.71, width = 3.71 * 1.618)
ggsave(here("graphics", "2022_01", "queda_e_recuperacao_brasil_cairo.png"), p2, dpi = 300, type = "cairo",
       height = 3.71, width = 3.71 * 1.618, units = "in")