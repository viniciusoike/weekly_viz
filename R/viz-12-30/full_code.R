# Furtos e Roubos em SP


# 0. Preamble -------------------------------------------------------------
library(ghibli)
library(tidyverse)
library(showtext)
library(here)
library(RcppRoll)
library(imputeTS)

showtext_opts(dpi = 300)
showtext_auto()

colors <- ghibli_palettes$TotoroMedium

#colors <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")

#font_add_google(name = "Roboto", family = "Roboto")

theme_vini <- theme_minimal() +
  theme(
    legend.position = "bottom",
    
    text = element_text(family = "Helvetica", size = 8),
    plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 90),
    
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )


# 1. Data -----------------------------------------------------------------

pop <- read_csv2(here("data/2021_12/seade_populacao_municipio.csv"),
                 locale = locale(encoding = "UTF-8"))

pop <- pop %>%
  filter(cod_ibge == 3550308) %>%
  select(ano, populacao)


data <- read_csv(here("data/2021_12/ssp_sao_paulo.csv"))

data <- data %>%
  rename(
    total_roubo = total_de_roubo_outros
  ) %>%
  mutate(
    date = as.Date(paste(ano, mes, "01", sep = "-")),
    homicidio_culposo = homicidio_culposo_outros + homicidio_culposo_por_acidente_de_transito,
    total_furto = furto_de_veiculo + furto_outros,
    letalidade_violenta = homicidio_doloso + latrocinio + lesao_corporal_seguida_de_morte
  ) %>%
  left_join(pop, by = "ano") %>%
  arrange(date)

taxa <- data %>%
  mutate(ano = as.character(ano),
         mes = as.character(mes)) %>%
  mutate(across(where(is.numeric), ~ .x / populacao * 10^5))

# Compute number of NAs in each case
taxa %>%
  select(date, contains("furto"), contains("roubo")) %>%
  map_dbl(~sum(is.na(.x)))

taxa %>%
  select(date, contains("furto"), contains("roubo")) %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~name)

series <- taxa %>%
  select(date, contains("furto"), contains("roubo")) %>%
  pivot_longer(cols = -date)

series_imputed <- parallel::mclapply(split(series, series$name), function(df) {
  
  # Extract series as numeric vector and convert to ts
  
  first_date <- min(df$date)
  first_mes <- lubridate::month(first_date)
  first_ano <- lubridate::year(first_date)
  
  y <- ts(df$value, start = c(first_ano, first_mes), frequency = 12)
  
  # Impute missing values using kalman filter
  imp <- imputeTS::na_kalman(y, model = "StructTS")
  # Estimate STL model and extract trend
  model.stl <- forecast::mstl(imp)
  trend.stl <- forecast::trendcycle(model.stl)
  
  # Try using X-13 ARIMA to evaluate seasonality
  model.seas <- try(seasonal::seas(imp))
  # If sucessful, extract trend, if not reuse series
  if (class(model.seas) == "try-error") {
    deseas <- imp
  } else {
    deseas <- forecast::seasadj(model.seas)
  }
  
  # Tidy output into single data.frame
  out <- data.frame(
    date = df$date,
    value = df$value,
    imputed = as.numeric(imp),
    trend = trend.stl,
    seas = as.numeric(deseas)
  )
})
# Rowbind all series
xseries <- bind_rows(series_imputed, .id = "name")
# Compute 6-month moving averages across all series
xseries <- xseries %>%
  group_by(name) %>%
  mutate(mm = roll_meanr(seas, 6)) %>%
  ungroup()

# 2. Plots ----------------------------------------------------------------

ggplot(xseries, aes(x = date, y = seas)) +
  geom_line() +
  facet_wrap(~name, scales = "free")


# Furto total e Roubo total

df_aux <- xseries %>%
  filter(name %in% c("total_furto", "total_roubo")) %>%
  select(date, name, trend, imputed)

p1 <- ggplot() +
  geom_line(data = filter(df_aux, name == "total_furto"),
            aes(x = date, y = imputed),
            colour = colors[1]) +
  geom_line(data = filter(df_aux, name == "total_furto"),
            aes(x = date, y = trend),
            colour = colors[6], size = 1) +
  labs(
    x = NULL,
    y = "Casos por Cem Mil Habitantes",
    title = "Taxa de Furtos"
  ) +
  scale_y_continuous(
    breaks = seq(80, 220, 20),
    labels = format(seq(80, 220, 20), big.mark = ".")
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_vini

p2 <- ggplot(data = filter(df_aux, name == "total_roubo"),
       aes(x = date)) +
  geom_line(aes(y = imputed),
            colour = colors[1]) +
  geom_line(aes(y = trend),
            colour = colors[6], size = 1) +
  labs(
    x = NULL,
    y = "Casos por Cem Mil Habitantes",
    title = "Taxa de Roubos"
  ) +
  scale_y_continuous(
    breaks = seq(40, 140, 20),
    labels = format(seq(40, 140, 20), big.mark = ".")
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_vini


library(patchwork)

p1 / p2

aseries <- xseries %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(name, year) %>%
  summarise(total_ano = mean(seas, na.rm = T)) %>%
  ungroup() %>%
  mutate(
    name_label = factor(
      name,
      labels = c("Furto Veiculo", "Furto Outros", "Roubo a Banco",
                 "Roubo de Carga", "Roubo de Veiculo",
                 "Roubo Outros", "Furto Total", "Roubo Total")
    )
  )

ggplot(filter(aseries, str_detect(name, "furto")),
       aes(x = year, y = total_ano)) +
  geom_col(fill = colors[3]) +
  labs(title = "Furtos em SP",
       x = NULL,
       y = "Taxa Média por Mil Habitantes") +
  facet_wrap(~name_label) +
  theme_vini

ggplot(filter(aseries, str_detect(name, "roubo"), year >= 2017),
       aes(x = year, y = total_ano)) +
  geom_col(fill = colors[3]) +
  labs(title = "Roubo em SP",
       x = NULL,
       y = "Taxa Média por Mil Habitantes") +
  facet_wrap(~name_label) +
  theme_vini

tfur <- ts(xseries[xseries$name == "total_furto", ]$imputed, frequency = 12)
trou <- ts(xseries[xseries$name == "total_roubo", ]$imputed, frequency = 12)

library(urca)


library(strucchange)

strucchange::efp()

# Model

tsfurto <- df_aux %>%
  filter(name == "total_furto") %>%
  select(date, imputed) %>%
  mutate(covid = ifelse(date >= as.Date("2020-02-01") & date <= as.Date("2020-05-01"),
                        1,
                        0))
tsroubo <- df_aux %>%
  filter(name == "total_roubo") %>%
  select(date, imputed)

model.furto <- stsm_estimate(
  y = tsfurto[, c("date", "imputed")],
  exo = tsfurto[, c("date", "covid")],
  freq = 12,
  verbose = TRUE
  )

model.furto$coef

stsm_forecast(model.furto, tsfurto[, c("date", "imputed")],
              n.ahead = 12,
              exo = rep(0, 12))

anom <- stsm_detect_anomalies(model.furto, tsfurto)
anom[anomaly == TRUE]



kal.furto <- stsm_filter(model.furto, tsfurto[, c("date", "imputed")], plot = TRUE)

hist(scale(kal.furto$remainder))
acf(kal.furto$remainder)

arma <- forecast::auto.arima(tsfurto$imputed)

forecast::tsdisplay(residuals(arma))


model.furto$
stsm_forecast(model.furto, y = tsfurto, n.ahead = 12, plot = TRUE)

stbreaks <- stsm_detect_breaks(model.furto, y = tsfurto, plot = TRUE)

stsm_detect_breaks(model.furto, y = tsfurto, components = "trend", plot = TRUE)

stsm_detect_anomalies(model.furto, y = tsfurto, plot = TRUE)
stsm_

stsm_fc = merge(stsm_fc, 
                stsm_detect_breaks(stsm, y = ts[, c("date", "y"), with = FALSE], plot = TRUE, show_progress = TRUE), 
                by = "date", all = TRUE)


ggplot() +
  geom_line(data = df_aux, aes(x = date, y = imputed)) +
  facet_wrap(~name, nrow = 2) +
  theme_vini

  
    group_by(name) %>%
  pivot_longer(-date)
  pivot_wider(id_cols = "date", names_from = "name", values_from = c("trend", "imputed")) %>%
  pivot_longer(cols = -"date") %>%
  mutate()

ggplot(filter(xseries, name %in% c("total_furto", "total_roubo")),
       aes(x = date, y = trend, colour = )) +
  geom_line() +
  scale_colour_manual(
    name = "",
    values = colors,
    labels = c("Furto", "Roubo")
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_vini

library(autostsm)

autostsm::
