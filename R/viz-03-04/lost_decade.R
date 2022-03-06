library(tidyverse)
library(here)

source(here("R/theme.R"))

longpib <- read_csv2(here("data/2022_03/ipea_pib_anual.csv"))

longpib <- longpib %>%
  rename(year = Data,
         value = `PIB - preços de mercado - variação real anual - (% a.a.) - Instituto Brasileiro de Geografia e Estatística, Sistema de Contas Nacionais (IBGE/SCN Anual) - SCN10_PIBG10`) %>%
  select(year, value)

ggplot(longpib, aes(x = year, y = value)) +
  geom_line()

qpib <- GetBCBData::gbcbd_get_single_series(22109, first.date = as.Date("1995-01-01"))

ggplot(qpib, aes(x = ref.date, y = value)) +
  geom_line(size = 1) +
  theme_vini

library(ipeadatar)

search <- ipeadatar::search_series("PIB real")

search
View(search)

qpib <- ipeadatar::ipeadata("PAN4_PIBPMG4")

apib <- ipeadata("PAN_PIBPMG")

apib10 <- apib %>%
  mutate(year = lubridate::year(date),
         highlight = factor(if_else(value > 0, 1, 0))) %>%
  filter(year >= 2011, year <= 2020)


ggplot(apib10, aes(x = year, y = value, fill = highlight)) +
  geom_col() +
  scale_x_continuous(breaks=2011:2020) +
  theme_vini



url <- "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx"
file <- basename(url)
download.file(url, file, mode = "wb")
mad <- readxl::read_excel(file, sheet = 3)
pop <- readxl::read_excel(file, sheet = 5, skip = 3, col_names = F)
file.remove(file)

mad <- maddison::maddison

mad <- as.data.table(mad)
View(mad[year == 1960, ][order(-rgdpnapc)])
mad[countrycode == "KOR" & year >= 1948 & year <= 1960, ]
source("0. Theme and Functions.R")
library(data.table)
library(sysfonts)
library(showtext)

library(ipeadatar)
library(maddison)


pib <- read.csv("04-2021 Decada Perdida/pib_real_trim.csv")
pib <- data.frame(
  date = seq(as.Date("1990-01-01"), as.Date("2020-10-01"), by = "quarter"),
  value = pib[, 1]
)

grafico_linha(pib,
              x = date,
              y = value / 10^6,
              y_axis_breaks = seq(0, 3, 0.5),
              ylab = "Trilhões de R$ (ajustado IGPDI)",
              title = "PIB Real (trimestral)",
              subtitle = "Nível atual é similar ao observado no 1o trim de 2010.",
              caption = "Fonte: PIB (IBGE), IGP-DI (FGV)") +
  geom_segment(data = data.frame(x0 = as.Date("2010-01-01"),
                                 x1 = as.Date("2020-12-01"),
                                 y = pibpc[pibpc$date == as.Date("2010-01-01"), 2]),
               aes(x = x0, xend = x1, y = y / 10^6, yend = y / 10^6),
               linetype = 2,
               size = 1,
               colour = cores2$qual4[2]) +
  theme(
    panel.grid.major.x = element_line(linetype = 2, colour = "gray70")
  )

pib <- GetBCBData::gbcbd_get_series(7326, first.date = as.Date("1995-01-01"))
pib$cresc <- factor(ifelse(pib$value > 0, 0, 1))
pib$ano <- lubridate::year(pib$ref.date)
grafico_coluna(pib,
               x=ano,
               y=value,
               variable=cresc,
               pal = 4,
               title = "Crescimento PIB real") +
  scale_y_continuous(limits = c(-4.2, 8), breaks = seq(-4, 8, 2)) +
  scale_x_continuous(breaks = 1995:2020) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

pib$decade <- findInterval(pib$ano, c(2000, 2010, 2020), rightmost.closed = T)
colnames()
resumo <- data.frame(
  decade = c("1995-2000", "2001-2010", "2011-2020"),
  valor = tapply(pib$value, pib$decade, mean)
)
names(resumo) <- c("Período", "Crescimento Médio")

igp <- GetBCBData::gbcbd_get_series(190, first.date = as.Date("1995-01-01"))
library(xts)
igp <- xts(igp$value, order.by = igp$ref.date)
igp <- apply.quarterly(igp, function(x) cumprod(1 + x/100))[, 3]
plot(igp)

# PAN4_PIBPMG4 - PIB real trimestral
# SCN10_PIBP10 - PIB - preços de mercado (preços 2010)	anual
# PAN_PIBCAP - Pib per capita anual
# WDI_PIBPPCCAPBRA - PIB per capita PPC

code_ipea <- c("PAN4_PIBPMG4", "SCN10_PIBP10", "PAN_PIBCAP", "WDI_PIBPPCCAPBRA")
series_ipea <- parallel::mclapply(code_ipea, ipeadatar::ipeadata)
names(series_ipea) <- c("pib_real", "pib_precos_2010", "pibpc", "pibpc_ppc")

series_ipea <- data.table::rbindlist(series_ipea, .id = "name_series")

pibpc <- as.data.table(series_ipea$pibpc_ppc)
pibpc[, chg := value / shift(value, 1)]

grafico_linha(pibpc, x = date, y = value) +
  
  
  pibreal <- as.data.table(series_ipea[[1]])
pibreal[, chg := value / shift(value, 1)]
pibreal[, mm4 := filter(value, filter = rep(1/4, 4), method = "convolution", sides = 1)]

grafico_linha(pibreal[date >= as.Date("1970-01-01")], x = date, y = mm4,
              zero = F)

pibreal[date >= as.Date("2010-01-01"), acum := cumprod(1 + value / 100)]

grafico_linha(pibreal, x = date, y = acum)


## Automatically use showtext to render text
maddison <- as.data.table(maddison)
maddison <- unique(maddison[, c(1, 11, 12)])
mad <- as.data.table(mad)
mad <- merge(mad, maddison, by = "countrycode")
bra <- mad[countrycode == "BRA" & year >= 1900, ]

anos80 <- data.frame(
  x = 1980,
  xend = 1990,
  y = 4000,
  yend = Inf,
  pib = bra[year %in% 1981:1990, mean(gdppc)]
)

anos10 <- data.frame(
  x = 2010,
  xend = 2020,
  y = 4000,
  yend = Inf,
  pib = bra[year %in% 2011:2020, mean(gdppc)]
)


grafico_linha(bra[year >= 1970, ],
              x = year,
              y = gdppc,
              colour = cores2$qual4[1],
              zero = F,
              title = "Crescimento PIB per capita real 1970-2020",
              subtitle = "Áreas sombreadas indicam as décadas de 1980 e 2010 respectivamente.",
              ylab = "(R$ 2011)") +
  scale_y_continuous(
    breaks = seq(4000, 16000, 2000),
    labels = format(seq(4000, 16000, 2000), big.mark = "."),
    limits = c(4000, 16000)
  ) +
  geom_rect(data = anos80,
            aes(xmin = x, xmax = xend, ymin = y, ymax = yend),
            fill = "#2a9d8f",
            alpha = 0.3) +
  geom_segment(data = anos80,
               aes(x = x, xend = xend, y = pib, yend = pib),
               linetype = 2,
               size = 1,
               colour = cores2$qual4[2]) +
  geom_rect(data = anos10,
            aes(xmin = x, xmax = xend, ymin = y, ymax = yend),
            fill = "#2a9d8f",
            alpha = 0.3) +
  geom_segment(data = anos10,
               aes(x = x, xend = xend, y = pib, yend = pib),
               linetype = 2,
               size = 1,
               colour = cores2$qual4[2]) +
  theme(text = element_text(family = "Montserrat"))

compare <- c("BRA", "CHL", "MEX", "ARG", "TUR", "IDN")
mad70 <- mad[year >= 1970 & countrycode %in% compare]

ggplot(data = mad70, aes(x = year, y = gdppc, colour = countrycode)) +
  geom_line(size = 1) +
  theme_vini



grafico_linha(mad70,
              x = year,
              y = gdppc,
              variable = countrycode,
              zero = F,
              title = "Crescimento PIB per capita real 1970-2020",
              subtitle = "Áreas sombreadas indicam as décadas de 1980 e 2010 respectivamente.",
              ylab = "(R$ 2011)") +
  scale_y_continuous(
    breaks = seq(4000, 16000, 2000),
    labels = format(seq(4000, 16000, 2000), big.mark = "."),
    limits = c(4000, 16000)
  ) +
  geom_rect(data = anos80,
            aes(xmin = x, xmax = xend, ymin = y, ymax = yend),
            fill = "#2a9d8f",
            alpha = 0.3) +
  geom_segment(data = anos80,
               aes(x = x, xend = xend, y = pib, yend = pib),
               linetype = 2,
               size = 1,
               colour = cores2$qual4[2]) +
  geom_rect(data = anos10,
            aes(xmin = x, xmax = xend, ymin = y, ymax = yend),
            fill = "#2a9d8f",
            alpha = 0.3) +
  geom_segment(data = anos10,
               aes(x = x, xend = xend, y = pib, yend = pib),
               linetype = 2,
               size = 1,
               colour = cores2$qual4[2]) +
  theme(text = element_text(family = "Montserrat"))

dec <- seq(1900, 2020, 10)
bra[, decade := dec[findInterval(year, dec)]]
bra[, growth := (gdppc / shift(gdppc) - 1) * 100]

bradec <- bra[, .(
  crescimento_medio = mean(growth, na.rm = T),
  pibpc_inicial = first(gdppc),
  pibpc_final = last(gdppc),
  crescimento_decada = (last(gdppc) / first(gdppc) - 1) * 100),
  by = decade]

bradec <- bradec[, lapply(.SD, round, digits = 2), by = decade]

nomes <- c("Década", "Crescimento Anual Médio", "PIBpc Início da Década",
           "PIBpc Final da Década", "Crescimento Década")
setnames(bradec, names(bradec), nomes)
bradec

emerging <- c("South America", "South-Eastern Asia", "Eastern Europe", "Caribbean")
developm <- c("Western Europe", "Australia and New Zealand", "Northern Europe")

mad[region %in% emerging, subregion := "Emergente"]
mad[region %in% developm | countrycode %in% c("USA", "CAN", "JPN"), subregion := "Developed"]
mad[countrycode == "BRA", subregion := "Brasil"]

dec <- seq(1960, 2020, 10)
mad60 <- mad[year >= 1960]
mad60[, decade := dec[findInterval(year, dec)]]
#all(mad[, .N, c("countrycode")]$N == 49)
mad60[, growth := (gdppc / shift(gdppc) - 1) * 100, by = "countrycode"]
mad_dec <- mad60[,
                 .(growth_medio = mean(growth)),
                 by = c("countrycode", "decade")
][!is.na(growth_medio)]
mad_dec <- merge(mad_dec,
                 unique(mad[, c("countrycode","region","subregion"), with = F]),
                 by = "countrycode")

dtcompare <- mad_dec[region %in% emerging[1:3],
                     .(growth_medio = mean(growth_medio)),
                     by = c("decade", "region")]
dtcompare <- rbind(dtcompare,
                   bra[, .(growth_medio = mean(growth)), by = "decade"],
                   fill = T)
dtcompare[is.na(region), region := "Brasil"]

grafico_coluna(dtcompare[decade >= 1970],
               x = decade,
               y = growth_medio,
               variable = region,
               y_axis_breaks = seq(-2, 7, 1),
               title = "Crescimento Médio por Década (Emergentes)",
               scale_label = c("Brasil", "Leste Europeu", "Sudeste Asiático",
                               "América do Sul"))

mad70 <- mad[year >= 1969 & region %in% c(emerging, developm)]
mad70[countrycode == "BRA", region := "Brasil"]
mad70[, growth := (gdppc / shift(gdppc) - 1) * 100, by = "countrycode"]
mad70 <- mad70[order(year)][order(region)]
mad70 <- mad70[!is.na(growth) & abs(growth) <= 10,
               .(growth = mean(growth)),
               by = c("year", "region")]
mad70[, acumgrowth := cumprod(1 + growth/100), by = "region"]

ggplot() +
  geom_line(data = mad70[region != "Brasil"],
            aes(x = year, y = growth, group = region),
            size = 1,
            colour = "gray50",
            alpha = 0.5) +
  geom_line(data = mad70[region == "Brasil"],
            aes(x = year, y = growth),
            size = 1.5,
            colour = cores2$qual4[1]) +
  theme_vini

ggplot() +
  geom_line(data = mad70,
            aes(x = year, y = acumgrowth, colour = region),
            size = 1)# +
geom_line(data = mad70[region == "Brasil"],
          aes(x = year, y = acumgrowth),
          size = 1.5,
          colour = cores2$qual4[1]) +
  theme_vini

pibpc_usa <- mad[countrycode == "USA" & year >= 1900]



grafico_linha(mad,
              x = year,
              y = growth,
              variable = highlight)
