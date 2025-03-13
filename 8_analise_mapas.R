library(sf)
library(ggplot2)
library(dplyr)

# Carregando o Shapefile
shapefile_path <- "C:/Users/erick/Downloads/Shapefile-20241230T145057Z-001/Shapefile/BR_Mesorregioes_2022.shp"
meso_map <- st_read(shapefile_path)

# Conferindo os nomes das mesorregiões no shapefile
unique(meso_map$NM_MESO)

str(meso_map)

# Selecionar o período desejado
periodo_selecionado <- "Periodo_1"  # Escolha o período (ex.: "Periodo_1", "Periodo_2", etc.)

matriz_h01_binaria <- rca_brasil[[periodo_selecionado]]$binaria %>%
  dplyr::select(nm_meso, H01)

# Associando os dados
meso_map_dados <- meso_map %>%
  left_join(matriz_h01_binaria, by = c("NM_MESO" = "nm_meso"))

# Criando o mapa com título dinâmico
ggplot(data = meso_map_dados) +
  geom_sf(aes(fill = H01), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90", name = "H01") +
  labs(title = paste("Período: 1997-2021"),
       caption = "Fonte: Dados Processados") +
  theme_minimal()


# Criar uma base consolidada para todos os períodos
base_consolidada <- NULL

# Iterar por todos os períodos
periodos <- paste0("Periodo_", 1:6)

for (periodo in periodos) {
  # Selecionar a matriz binária do período atual
  matriz_h01_binaria <- rca_brasil[[periodo]]$binaria %>%
    dplyr::select(nm_meso, H01)
  
  # Adicionar a informação do período
  matriz_h01_binaria <- matriz_h01_binaria %>%
    mutate(periodo = periodo)
  
  # Combinar com a base consolidada
  base_consolidada <- bind_rows(base_consolidada, matriz_h01_binaria)
}

# Transformar a base consolidada para formato wide
base_wide <- base_consolidada %>%
  tidyr::pivot_wider(
    names_from = periodo,  # Criar colunas baseadas no período
    values_from = H01      # Valores serão os da coluna H01
  )

# Salvar a base consolidada em CSV
write.csv(base_wide, "base_wide.csv", row.names = FALSE)







###################### GRÁFICO DE DADOS GERAIS ########################################

# Conferindo os nomes das mesorregiões no shapefile
unique(meso_map$NM_MESO)

# Conferindo os nomes das mesorregiões na base de inventores
unique(contagem_BR$nm_meso)

# Padronizar os nomes das mesorregiões (se necessário)
contagem_BR$nm_meso <- tolower(contagem_BR$nm_meso)  # Deixar em minúsculo
meso_map$NM_MESO <- tolower(meso_map$NM_MESO)        # Deixar em minúsculo

# Associando os dados de inventores ao shapefile
meso_map_dados <- meso_map %>%
  left_join(contagem_BR, by = c("NM_MESO" = "nm_meso"))

# Criando o gráfico com a quantidade de inventores por mesorregião
ggplot(data = meso_map_dados) +
  geom_sf(aes(fill = n), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90", name = "Nº de Inventores") +
  labs(title = "Distribuição de Inventores nas Mesorregiões Brasileiras",
       caption = "Fonte: Dados Processados") +
  theme_minimal()

############################### MAPAS DAS MESORREGIÕES ESPECIALIZADAS ###############################

str(rca_binario_brasil_long)

# Filtrar a base para manter apenas as observações onde CD_CLASSIF_3 == "H01"
rca_filtrada_long <- rca_binario_brasil_long %>%
  filter(CD_CLASSIF_3 == "H01")

# Transformar para formato wide
rca_wide <- rca_filtrada_long %>%
  pivot_wider(names_from = periodo, values_from = RCA_binaria, names_prefix = "periodo_")

# Adicionar uma coluna com a contagem de períodos onde RCA_binaria = 1
rca_wide2 <- rca_wide %>%
  mutate(contagem_RCA = rowSums(dplyr::select(., starts_with("periodo_")) == 1, na.rm = TRUE))

str(rca_wide2)

# Adicionar uma coluna com a contagem de NAs por linha
rca_wide2 <- rca_wide2 %>%
  mutate(contagem_NA = rowSums(is.na(dplyr::select(., starts_with("periodo_")))))

str(rca_wide2)

# Criar a variável categórica com base nas contagens
rca_wide2 <- rca_wide2 %>%
  mutate(categoria_meso = case_when(
    contagem_RCA >= 4 ~ "alta_RCA",  # Azul escuro
    contagem_RCA == 0  ~ "alta_NA",   # Cinza claro
    contagem_RCA >= 1 & contagem_RCA < 4  ~ "outros"      # Azul claro
  ))

# Associando os dados ao shapefile
meso_map_dados <- meso_map %>%
  left_join(rca_wide2, by = c("NM_MESO" = "nm_meso"))

# Definir a paleta de cores personalizada
cores_mapa <- c("alta_RCA" = "darkblue", "alta_NA" = "lightgray", "outros" = "lightblue")

# Criar o mapa
ggplot(data = meso_map_dados) +
  geom_sf(aes(fill = categoria_meso), color = "white") +
  scale_fill_manual(values = cores_mapa, name = "Classificação") +
  labs(title = "Mapa das Mesorregiões - H01 (1997-2021)",
       caption = "Fonte: Dados Processados") +
  theme_minimal()