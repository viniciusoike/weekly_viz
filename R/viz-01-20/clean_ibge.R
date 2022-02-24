library(tidyverse)
library(here)

data01 <- readxl::read_excel(here("data/2022_01/pib_municipios_2002_2009.xls"))
data02 <- readxl::read_excel(here("data/2022_01/pib_municipios_2010_2019.xls"))

data <- bind_rows(list(data01, data02))
data <- janitor::clean_names(data)

data <- data %>%
  select(
    ano,
    code_state = codigo_da_unidade_da_federacao,
    name_state = nome_da_unidade_da_federacao,
    code_muni = codigo_do_municipio,
    name_muni = nome_do_municipio,
    name_rmetro = regiao_metropolitana,
    nome_da_mesorregiao,
    va_agro = valor_adicionado_bruto_da_agropecuaria_a_precos_correntes_r_1_000,
    va_indus = valor_adicionado_bruto_da_industria_a_precos_correntes_r_1_000,
    va_servicos = valor_adicionado_bruto_dos_servicos_a_precos_correntes_exceto_administracao_defesa_educacao_e_saude_publicas_e_seguridade_social_r_1_000,
    va_adm_pub = valor_adicionado_bruto_da_administracao_defesa_educacao_e_saude_publicas_e_seguridade_social_a_precos_correntes_r_1_000,
    va_total = valor_adicionado_bruto_total_a_precos_correntes_r_1_000,
    ativ_1 = atividade_com_maior_valor_adicionado_bruto,
    ativ_2 = atividade_com_segundo_maior_valor_adicionado_bruto,
    pib = produto_interno_bruto_a_precos_correntes_r_1_000,
    pib_pc = produto_interno_bruto_per_capita_a_precos_correntes_r_1_00,
    impostos = impostos_liquidos_de_subsidios_sobre_produtos_a_precos_correntes_r_1_000
  )

write_csv(data, "data/2022_01/pib_clean.csv")