# Preamble ----------------------------------------------------------------
# Libraries
library(tidyverse)
library(here)

# Function to convert "231,21" to 231.21
as_numeric_comma <- function (x) {
  
  if (is.numeric(x)) {return(x)} 
  
  x <- stringr::str_remove_all(x, pattern = "\\.")
  x <- stringr::str_replace(x, pattern = ",", replacement = ".")
  x <- as.numeric(x)
  return(x)
  
}

# Set directory
dir <- here("data/itbi")

# Download and Import -----------------------------------------------------

# Urls from dados abertos
urls <- c(
  "https://ckan.pbh.gov.br/dataset/fdb1a8b6-1ef5-4084-bda5-e9503d00d5c5/resource/c98e75c0-fe00-4b0c-98c9-a30e45ee76dd/download/relatorioitbi2009_julho2021.csv",
  "https://ckan.pbh.gov.br/dataset/fdb1a8b6-1ef5-4084-bda5-e9503d00d5c5/resource/1536ccbe-7d65-43dd-9628-bdb96f14f731/download/relatorioitbiagosto2021.csv",
  "https://ckan.pbh.gov.br/dataset/fdb1a8b6-1ef5-4084-bda5-e9503d00d5c5/resource/9a04f4c0-9ca0-434b-b405-fcae8154e6d5/download/relatorioitbisetembro2021.csv",
  "https://ckan.pbh.gov.br/dataset/fdb1a8b6-1ef5-4084-bda5-e9503d00d5c5/resource/d1712a0b-6dec-48e3-8f01-a14599b86942/download/relatorioitbioutubro2021.csv",
  "https://ckan.pbh.gov.br/dataset/fdb1a8b6-1ef5-4084-bda5-e9503d00d5c5/resource/d3aeb4fb-f0a0-4cbe-81ee-5596609e4367/download/relatorioitbinovembro2021.csv",
  "https://ckan.pbh.gov.br/dataset/fdb1a8b6-1ef5-4084-bda5-e9503d00d5c5/resource/933ca26b-5497-4b3e-8567-135105ac8c11/download/relatorioitbidezembro2021.csv")

# Get filename from url
n <- str_remove(str_extract(urls, pattern = "itbi.+"), "itbi")
# Download and save in loop
for (i in seq_along(urls)) {
  
  name_file <- n[i]
  download.file(urls[i], destfile = here(dir, name_file), mode = "wb")
  
}
# Import all files using map
name_file <- list.files(dir)
path_file <- sapply(name_file, \(x) here(dir, x))
data <- map(path_file, data.table::fread)
# Stack all tables by row (some column names inconsistencies)
data <- bind_rows(map(data, janitor::clean_names))


# Clean and export --------------------------------------------------------

data <- data %>%
  # Merge columns that have the same information
  mutate(
    house_address = if_else(
      is.na(endereco_completo),
      endereco,
      endereco_completo),
    house_construction_year = if_else(
      is.na(ano_construcao_unidade),
      ano_de_construcao_unidade,
      ano_construcao_unidade),
    itbi_zone_use = if_else(
      is.na(descri_uo_tipo_ocupacao_unidade),
      descricao_tipo_ocupacao_unidade,
      descri_uo_tipo_ocupacao_unidade)) %>%
  # Rename columns
  rename(
    ts_date = data_inclusao_transacao,
    itbi_zone = zona_uso_itbi,
    itbi_name_neighborhood = bairro,
    itbi_tax_value = valor_imposto_cobrado_sumarizado,
    itbi_house_value = valor_base_calculo,
    house_land_area = area_terreno_total,
    house_unit_area = area_construida_adquirida,
    house_total_area = area_adquirida_unidades_somadas,
    house_construction_type = padrao_acabamento_unidade,
    itbi_fraction = fracao_ideal_adquirida
  ) %>%
  # Convert to numeric (swap comma for dot)
  mutate(
    across(.cols = matches("(area)|(value)|(itbi_fraction)"),
           .fns = as_numeric_comma)) %>%
  # Get zipcode, compute value/m2 and house age
  mutate(
    house_zipcode = str_extract(house_address, pattern = "[0-9]{5}-[0-9]{3}"),
    house_city = "Belo Horizonte",
    house_state = "Minas Gerais",
    itbi_house_value_land_area = ifelse(house_land_area > 0, itbi_house_value / house_land_area, NA),
    itbi_house_value_total_area = ifelse(house_total_area > 0, itbi_house_value / house_total_area, NA),
    ts_date = as.Date(ts_date, format = "%d/%m/%Y"),
    ts_year = lubridate::year(ts_date),
    house_age_at_transaction = as.numeric(lubridate::year(lubridate::round_date(ts_date, "year"))) - house_construction_year
  ) %>%
  # Rearrange column order
  select(
    ts_date, ts_year, house_construction_year, house_age_at_transaction,
    itbi_name_neighborhood, itbi_zone,
    itbi_house_value, itbi_tax_value, itbi_fraction,
    itbi_house_value_land_area, itbi_house_value_total_area,
    house_land_area, house_unit_area, house_total_area,
    house_construction_type, house_state, house_city, house_zipcode, house_address)

# Import updated CPI data to convert all prices to current market values
ipca <- GetBCBData::gbcbd_get_series(433, first.date = min(data$ts_date, na.rm = T))
# Dplyr is not very good for this
ipca <- ipca %>%
  mutate(
    acum = cumprod(1 + value / 100),
    ipca_rev = last(acum) / acum,
    ipca_correction = lag(ipca_rev, 1),
    ipca_correction = ifelse(is.na(ipca_correction), last(acum), ipca_correction)) %>%
  select(ts_round_date = ref.date, ipca_correction)
# Join with original data and create *_cpi variables
data <- data %>%
  mutate(ts_round_date = lubridate::round_date(ts_date, unit = "month")) %>%
  left_join(ipca) %>%
  mutate(
    itbi_house_value_cpi = itbi_house_value * ipca_correction,
    itbi_tax_value_cpi = itbi_tax_value * ipca_correction,
    itbi_house_value_land_area_cpi = itbi_house_value_land_area * ipca_correction,
    itbi_house_value_total_area_cpi = itbi_house_value_total_area * ipca_correction
  ) %>%
  select(-ts_round_date)

# Export
data.table::fwrite(data, here(dir, "itbi_clean.csv"))