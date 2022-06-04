library(tidyverse)
library(here)
library(showtext)
library(RcppRoll)
library(cowplot)
library(RColorBrewer)
library(readxl)
font_add("Arial", "Arial.ttf")
showtext_opts(dpi = 300)
showtext_auto()
source(here("R/theme.R"))
library(forecast)
library(lubridate)
library(tidycovid19)

hwtrend <- function(data) {
  
  start <- c(min(year(data$date)), min(month(data$date)))
  series <- data$value
  
  x <- ts(series, start = start, frequency = 12)
  
  model <- HoltWinters(x,seasonal = "multiplicative")
  detrend <- model$fitted[, "level"]
  
  df <- tibble(
    date = zoo::as.Date(zoo::as.yearmon(time(detrend))),
    detrend = zoo::coredata(detrend)
  )
  
  left_join(data, df, by = "date")
  
}


# varejo e industria ------------------------------------------------------

dflabel <- tibble(
  id.num = c(28473, 28503),
  name_simplified = c("sales_index", "industry_index"),
  name_label = c("Varejo", "Indústria")
)

series <- GetBCBData::gbcbd_get_series(dflabel$id.num, first.date = as.Date("2010-01-01"))

df <- series |> 
  left_join(dflabel) |> 
  rename(date = ref.date)

base_index <- df |> 
  mutate(year = lubridate::year(date)) |> 
  filter(year == 2019) |> 
  group_by(name_simplified) |> 
  summarise(base = mean(value))

df <- df |> 
  left_join(base_index, by = "name_simplified") |> 
  mutate(index = value / base * 100)

plot_prod <- ggplot(df, aes(x = date, y = index, color = name_label)) +
  geom_line(size = 1) +
  geom_point(aes(shape = name_label)) +
  #geom_vline(xintercept = as.Date("2020-03-01")) +
  scale_color_brewer(name = "", type = "qual", palette = 6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  guides(shape = "none") +
  labs(y = "Índice (100 = média 2019)",
       x = NULL) +
  theme_cowplot(font_family = "Arial") +
  background_grid() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  )

save_plot("plot_5.png", plot_prod)


# preco energia -----------------------------------------------------------

code <- 29039

series <- GetBCBData::gbcbd_get_series(29039, first.date = as.Date("2010-01-01"))

df <- series |> 
  rename(date = ref.date) |> 
  mutate(year = lubridate::year(date))

base_index <- df |> 
  filter(year == 2019) |>
  summarise(base = mean(value))

df <- df |> 
  cbind(base_index) |> 
  mutate(index = value / base * 100)

plot_preco_energia <- ggplot(df, aes(x = date, y = index)) +
  geom_line(size = 1) +
  scale_color_brewer(name = "", type = "qual", palette = 6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(50, 200, 25)) +
  guides(shape = "none") +
  labs(y = "Índice (100 = média 2019)",
       x = NULL) +
  theme_cowplot(font_family = "Arial") +
  background_grid() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  )

save_plot("plot_6.png", plot_preco_energia)

search <- ipeadatar::search_series("energia")

price <- read_excel("data/2022_06/energy_prices.xlsx")

price <-  price |> 
  mutate(date = as.Date(paste0(date, ".01"), format = "%Y.%m.%d"),
         year = lubridate::year(date)) |> 
  filter(year >= 2015)

price |> 
  group_by(year, name) |> 
  summarise(chg = last(value) / first(value) - 1) |> 
  pivot_wider(id_cols = year, names_from = name, values_from = chg)

plot_price <-  price |> 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(size = 1) +
  geom_point(aes(shape = name)) +
  scale_color_brewer(
    name = "", type = "qual", palette = 6,
    labels = c("Comercial", "Industrial", "Residencial")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(200, 800, 100)) +
  guides(shape = "none") +
  labs(y = "Tarifa média por MWh (R$)",
       x = NULL) +
  theme_cowplot(font_family = "Arial") +
  background_grid() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  )

save_plot("plot_7.png", plot_price)


# consumo energia ---------------------------------------------------------



dflabel <- tibble(
  id.num = c(1402, 1403, 1404, 1406, 24364),
  name_simplified = c("comercial", "residencial", "industrial", "total", "ibc"),
  name_label = c("Comercial", "Residencial", "Industrial", "Total", "IBC-Br")
)

series <- GetBCBData::gbcbd_get_series(dflabel$id.num, first.date = as.Date("2010-01-01"))

df <- series |> 
  left_join(dflabel) |> 
  rename(date = ref.date) |> 
  select(date, name_label, value) |> 
  group_by(name_label) |> 
  mutate(ma6 = roll_meanr(value, n = 1)) |> 
  filter(date >= as.Date("2010-01-01"))

df <- bind_rows(lapply(split(df, df$name_label), hwtrend))

dfenergia <- filter(df, !name_label %in% c("IBC-Br", "Total"))

p1 <- ggplot(filter(dfenergia, date >= as.Date("2015-01-01")), aes(x = date, y = ma6, color = name_label)) +
  geom_line(size = 1) +
  geom_point(aes(shape = name_label)) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  scale_color_brewer(name = "", type = "qual", palette = 6) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  guides(shape = "none") +
  labs(y = "GWh",
       x = NULL) +
  theme_cowplot() +
  background_grid() +
  theme(
    legend.position = "bottom"
  )

p2 <- ggplot(filter(dfenergia, date >= as.Date("2015-01-01")),
             aes(x = date, y = detrend, color = name_label)) +
  geom_line(size = 1) +
  geom_point(aes(shape = name_label)) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  scale_color_brewer(name = "", type = "qual", palette = 6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  guides(shape = "none") +
  labs(y = "GWh",
       x = NULL) +
  theme_cowplot() +
  background_grid() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white")
  )

plot_yoy <- df |> 
  filter(name_label == "Total") |> 
  mutate(yoychg = value / lag(value, 12) - 1) |> 
  filter(date >= as.Date("2020-01-01")) |> 
  ggplot(aes(x = date, y = yoychg)) +
  geom_col(fill = "#377EB8") +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "% (var. contra mês do ano anteior)") +
  scale_x_date(date_breaks = "1 month", date_label = "%Y-%m") +
  scale_y_continuous(breaks = seq(-10, 15, 5) / 100, labels = scales::label_percent()) +
  theme_cowplot(font_family = "Arial", font_size = 12) +
  background_grid() +
  theme(axis.text.x = element_text(angle = 90))

save_plot("plot_9.png", plot_yoy)

# df |> 
#   filter(name_label == "Total") |> 
#   mutate(chg = value / lag(value) - 1,
#          acum12m = roll_prodr(1 + chg, n = 12) - 1,
#          year = lubridate::year(date)) |> 
#   filter(year >= 2020) |>
# 
# df |> 
#   filter(name_label == "Total") |> 
#   mutate(year = lubridate::year(date)) |> 
#   group_by(year) |> 
#   summarise(total = sum(value)) |> 
#   ungroup() |> 
#   mutate(chg = total / lag(total) - 1)


tbl <- df |> 
  filter(name_label %in% c("IBC-Br", "Total")) |> 
  mutate(detrend_value = ifelse(name_label == "IBC-Br", value, detrend),
         name_simplified = ifelse(name_label == "IBC-Br", "ibc", "total")) |> 
  na.omit() |> 
  pivot_wider(id_cols = date, names_from = name_simplified, values_from = detrend_value)

pandemia <- seq(as.Date("2020-03-01"), as.Date("2020-12-01"), by = "month")
dilma <- seq(as.Date("2014-06-01"), as.Date("2015-12-01"), by = "month")

tbl |> 
  #mutate(across(ibc:total, ~ as.numeric(scale(.x)))) |> 
  mutate(year = lubridate::year(date)) |> 
  mutate(dummy = ifelse(year <= 2014, 1, 0)) |> 
  filter(!date %in% c(pandemia, dilma), ibc > -1) |> 
  #filter(year >= 2016, ibc > - 1) |> 
  ggplot(aes(x = ibc, y = total, color = as.factor(dummy))) +
  geom_point() +
  geom_smooth(method = "lm")

ibc <- ts(scale(tbl$ibc), frequency = 12)
ee <- ts(scale(tbl$total), frequency = 12)
dum <- ts(c(rep(0, 40), rep(1, 24), rep(0, length(ibc) - 64)), frequency = 12)

model.var <- VAR(cbind(ibc, ee), p = 4, exogen = dum)

library(strucchange)

summary(lm(ee ~ ibc))

ocus <- strucchange::efp(ibc ~ ee, type = "OLS-CUSUM")
re <- strucchange::efp(ibc ~ ee, type = "RE")
  
filter(`IBC-Br` > 130) |> 
  ggplot(aes(x = `IBC-Br`, y = log(Total))) +
  geom_point() +
  geom_smooth()
  

series |> 
  left_join(dflabel) |> 
  rename(date = ref.date) |> 
  filter(name_simplified %in% c("total", "ibc")) |> 
  select(date, name_label, value) |> 
  pivot_wider(id_cols = date, names_from = name_label, value_from = value)

  

save_plot("plot_1.png", p1)
save_plot("plot_2.png", p2)


# indicators --------------------------------------------------------------

library(readxl)

ind <- read_excel("data/2022_06/indicators_energy_companies.xlsx", sheet = 1,
                  col_names = c("code_company", "indicator", paste0("v", 2018:2021)))
dict <- read_excel("data/2022_06/indicators_energy_companies.xlsx", sheet = 2)

ind <- ind |> 
  mutate(code_company = as.numeric(code_company)) |> 
  filter(!is.na(code_company)) |> 
  left_join(dict, by = "code_company") |> 
  pivot_longer(cols = -c(code_company, key, indicator), names_to = "year") |> 
  mutate(year = as.numeric(str_remove(year, "v"))) |> 
  select(year, code_company, key, indicator, value)

# ind <- ind |> 
#   left_join(dict, by = "code_company") |> 
#   pivot_longer(cols = -c(year, code_company, key), names_to = "indicator") 

ind |> 
  group_by(year, indicator) |>
  summarise(media = mean(value, na.rm = TRUE),
            dp = sd(value, na.rm = TRUE)) |> 
  pivot_wider(id_cols = c(indicator), names_from = year, values_from = c(media, dp))
  
ind |> 
  group_by(indicator) |> 
  summarise(media = mean(value, na.rm = TRUE),
            maximo = max(value, na.rm = TRUE),
            minimo = min(value, na.rm = TRUE),
            dp = sd(value, na.rm = TRUE),
            q25 = quantile(value, .25, na.rm = TRUE),
            q50 = quantile(value, .50, na.rm = TRUE),
            q75 = quantile(value, .75, na.rm = TRUE))

# b3 ----------------------------------------------------------------------

b3series <- read_excel("data/2022_06/iee_summary.xlsx", sheet = 1)

plot_series <- b3series |> 
  mutate(date = as.Date(paste(year, str_pad(month, 2, "left", "0"), "01", sep = "-"))) |> 
  ggplot(aes(x = date, y = iee)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = NULL,
       y = "Índice") +
  theme_cowplot() +
  background_grid()

b3year <- read_excel("data/2022_06/iee_summary.xlsx", sheet = 2)  

library(data.table)
ipca <- setDT(ipca)
ipca <- GetBCBData::gbcbd_get_series(433, first.date = as.Date("2018-01-01"))

ipca[, value := value / 100]
ipca[ref.date >= as.Date("2019-01-01"), acum := cumprod(1 + value)]
ipca[, correction := 1 / acum]

mv <- read_excel("data/2022_06/iee_summary.xlsx", sheet = 3)

mv <- mv |> 
  mutate(date = ymd(yearmon)) |> 
  arrange(date) |> 
  left_join(ipca, by = c("date" = "ref.date")) |> 
  mutate(market_value_brl_ipca = market_value_brl * correction)

mv <- mv |> 
  select(date, market_value_brl, market_value_brl_ipca) |> 
  pivot_longer(-date) |> 
  mutate(name = ifelse(str_detect(name, "ipca"), "Corrigido IPCA", "Original"),
         value = value / 10e9)

plot_mv <- ggplot(mv, aes(x = date, y = value, color = name)) +
  geom_line(size = 1) +
  geom_point(aes(shape = name)) +
  scale_y_continuous(breaks = seq(20, 40, 1)) +
  labs(x = NULL,
       y = "R$ (Bilhões)") +
  scale_color_brewer(name = "", type = "qual", palette = 6) +
  guides(shape = "none") +
  theme_cowplot() +
  background_grid() +
  theme(legend.position = "bottom")

save_plot("plot_3.png", plot_series)
save_plot("plot_4.png", plot_mv)



# google mobility ---------------------------------------------------------

google <- download_google_cmr_data()

google_brasil <- google |> 
  filter(iso3c == "BRA") |> 
  select(date, retail_recreation:residential) |> 
  pivot_longer(cols = -date) |> 
  filter(name != "transit_stations") |> 
  group_by(name) |> 
  mutate(ma30 = RcppRoll::roll_meanr(value, n = 30))

plot_google <- ggplot(na.omit(google_brasil), aes(x = date, y = ma30, color = name)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0) +
  scale_color_brewer(
    name = "", type = "qual", palette = 6,
    labels = c("Mercado/farmácia", "Parques", "Casa", "Comércio/Varejo", "Trabalho")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = seq(-60, 60, 10)) +
  guides(shape = "none", color = guide_legend(nrow = 1)) +
  labs(y = "Índice (100 = 2020/02)",
       x = NULL) +
  theme_cowplot(font_family = "Arial", font_size = 12) +
  background_grid() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  )

save_plot("plot_8.png", plot_google)
