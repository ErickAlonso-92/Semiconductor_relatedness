install.packages("stringi")
library(stringi)

#Define o diretório que será usado
setwd("C:\\Temp\\Downloads\\tabelas2022-20240722T114704Z-001\\tabelas2022")
getwd()

#Monta as bases de dados
patentes = fread(file = "./dados/badepiv9_ptn_deposito.csv", 
                sep = ",",
                dec = ".",
                header = TRUE,
                stringsAsFactors = FALSE,
                encoding = "UTF-8")

depositante = fread(file = "./dados/badepiv9_ptn_depositante.csv", 
                    sep = ",",
                    dec = ".",
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    encoding = "UTF-8")

depositante2 = fread(file = "./dados/badepiv9_ptn_depositante_v2.csv", 
                    sep = ",",
                    dec = ".",
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    encoding = "UTF-8")

codigo_ipc = fread(file = "./dados/badepiv9_ptn_ipc_campo_tec.csv", 
                   sep = ",",
                   dec = ".",
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   encoding = "UTF-8")

inventor = fread(file = "./dados/badepiv9_ptn_inventor.csv", 
                 sep = ",",
                 dec = ".",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8")

#Verifica os nomes das colunas e renomea algumas delas
colnames(patentes)
colnames(inventor)
colnames(depositante2)
colnames(codigo_ipc)

patentes <- patentes %>%
  rename(CD_PAIS_DEPOSITO = CD_PAIS_PFPJ)

inventor <- inventor %>%
  rename(NO_ORDEM_INVENTOR = no_ordem)

inventor <- inventor %>%
  rename(NO_PEDIDO = no_pedido)

inventor <- inventor %>%
  rename(nm_inventor = nm_complet_pfpj)

inventor <- inventor %>%
  rename(cd_pais_inventor = cd_pais_pfpj)

inventor <- inventor %>%
  rename(cd_uf_inventor = cd_uf_pfpj)

inventor <- inventor %>%
  rename(nm_cidade_inventor = nm_cidade_pfpj)

inventor <- inventor %>%
  rename(cd_ibge_inventor = cd_ibge_cidade)

inventor <- inventor %>%
  rename(no_cep_inventor = no_cep_pfpj)

depositante2 <- depositante2 %>%
  rename(NO_ORDEM_DEPOSITANTE = no_ordem)

depositante2 <- depositante2 %>%
  rename(NO_PEDIDO = no_pedido)

depositante2 <- depositante2 %>%
  rename(cd_tipo_dep = cd_tipo_pfpj)

depositante2 <- depositante2 %>%
  rename(nm_depositante = nm_complet_pfpj)

depositante2 <- depositante2 %>%
  rename(cd_pais_depositante = cd_pais_pfpj)

depositante2 <- depositante2 %>%
  rename(cd_uf_depositante = cd_uf_pfpj)

depositante2 <- depositante2 %>%
  rename(nm_cidade_depositante = nm_cidade_pfpj)

depositante2 <- depositante2 %>%
  rename(cd_ibge_depositante = cd_ibge_cidade)

depositante2 <- depositante2 %>%
  rename(no_cep_depositante = no_cep_pfpj)

#Eliminar as bases extras existentes
rm(br_depositante, br_inventors, codigo_ipc_filtered, codigo_ipc_grouped, combined_data, combined_data2, country_contributions_by_period, country_contributions_by_period2, country_contributions_by_period3, depositante_contributions, depositante_contributions_br, depositante_contributions_by_period, depositante_counts, depositante_counts_br, inventor_contributions, inventor_contributions_br, inventor_counts, inventor_counts_br)
rm(resultados, semicondutor_patente1, semicondutor_patente_depositante_filtrado, semicondutor_patente_depositante_selecionado, semicondutor_patente2, semicondutor_patente3, semicondutor_patentecompleta, semicondutor_patentecompleta_selecionada, semicondutor_patentecompleta_selecionada_periodo, state_contributions_by_period, contribuicao_inventores, contribuicao_por_estado_periodo, contribuicao_por_pais_periodo)
rm(contribuicao_por_pais_periodo2, contribuicao_por_tipo_depositante, contribuicao_por_tipo_depositante_br, semicondutor_patente_depositante_br, semicondutor_patente_inventor_br, semicondutor_patente_inventor_filtrado, total_contributions_by_country, total_contributions_by_state, total_depositante_por_patente, total_inventores_por_patente)
rm(agricultura_verde_patente, aspectos_regulatorios_patente, biocombustivel_patente, celula_combustivel_patente, combustivel_desperdicio_patente, conservacao_patente, contagem_br_periodo_tipo, contagem_pais_periodo, contagem_pais_periodo_br, contagem_pais_periodo_depositante, contagem_pais_periodo_depositante_br, contagem_pais_periodo_depositante2, contagem_pais_periodo_depositante2br)
rm(contagem_pais_periodo_tipo, controle_poluicao_patente, energia_hidro_patente, eolica_patente, geotermico_patente, frequencia_pais_inventor, gestao_residuo_patente, heating_pump_patente, nuclear_patente, semicondutor_base_inventor, semicondutor_patente_depositante_br, semicondutor_patente_inventor_br, semicondutor_patente_inventor_filtrado)
rm(solar_patente, tecnologia_verde_patente, total_inventores_por_patente, veiculo_eletrico_patente)


#Separa o conjunto de dados de patentes de interesse (SEMICONDUTORES)
semicondutor_patente1 <- codigo_ipc %>%  
  filter(grepl("^H01L", CD_CLASSIF))

semicondutor_patente2 <- codigo_ipc %>%  
  filter(grepl("^8$", CAMPO_TEC))

#Junta com os dados de outras colunas
combined_data <- semicondutor_patente2 %>%
  left_join(inventor, by = "NO_PEDIDO")

combined_data <- combined_data %>%
  left_join(patentes, by = "NO_PEDIDO")

#Salva a base num arquivo CSV
write_csv(combined_data, "combined_data.csv")

#Captura os códigos IPC descartados pelo filtro anterior
codigo_ipc_filtered <- codigo_ipc %>%
  filter(!str_detect(CD_CLASSIF, "^H01L"))

codigo_ipc_grouped <- codigo_ipc_filtered %>%
  group_by(NO_PEDIDO) %>%
  summarise(CD_OUTRA_CLASSIF = paste(unique(CD_CLASSIF), collapse = ","))

combined_data2 <- combined_data %>%
  left_join(codigo_ipc_grouped, by = "NO_PEDIDO")

write_csv(combined_data2, "combined_data2.csv")

#Padronizar os dados
colnames(combined_data2)

capitalize_first_letter <- function(x) {
  sapply(x, function(y) {
    if (is.character(y)) {
      y <- tolower(y)
      words <- strsplit(y, " ")[[1]]
      words <- sapply(words, function(word) {
        paste(toupper(substring(word, 1, 1)), substring(word, 2), sep = "")
      })
      paste(words, collapse = " ")
    } else {
      y
    }
  })
}

dados_completo <- dados_completo %>%
  mutate(across(c("nm_inventor", "nm_cidade_inventor"), ~capitalize_first_letter(.)))

#Salva a base num arquivo CSV
write_csv(dados_completo, "dados_completo.csv")
