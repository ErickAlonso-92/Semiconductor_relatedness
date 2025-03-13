dados_regionais = fread(file = "./dados/Dados_regionais.csv", 
                                     sep = ",",
                                     dec = ".",
                                     header = TRUE,
                                     stringsAsFactors = FALSE,
                                     encoding = "UTF-8")


### RESOLVENDO OS PROBLEMAS DOS DADOS DO PIB AGREGADO ######
# Selecione apenas as colunas de PIB da base de dados_regionais
dados_completos <- dados_regionais %>%
  dplyr::select(starts_with("pib_"))

# Adicione colunas vazias para os anos 1997 e 1998
dados_completos <- dados_completos %>%
  mutate(pib_1997 = NA, pib_1998 = NA) %>%
  dplyr::select(sort(names(.)))  # Ordena as colunas por ano

# Substituir vírgulas por pontos e converter para numérico em todas as colunas de PIB
dados_completos <- dados_completos %>%
  mutate(across(starts_with("pib_"), ~ as.numeric(gsub(",", ".", .))))

#Criando os dados para os anos de 1997 e 1998, inferindo a taxa de crescimento de 1996 e 1999 
taxa_crescimento <- (dados_completos$pib_1999 / dados_completos$pib_1996)^(1/3) - 1
dados_completos$pib_1997 <- dados_completos$pib_1996 * (1 + taxa_crescimento)
dados_completos$pib_1998 <- dados_completos$pib_1997 * (1 + taxa_crescimento)

# Remova as colunas de PIB antigas de dados_regionais
dados_regionais <- dados_regionais %>%
  dplyr::select(-starts_with("pib_"))

# Adicione as colunas de PIB atualizadas de dados_completos em dados_regionais
dados_regionais <- bind_cols(dados_regionais, dados_completos)


##### RESOLVENDO OS PROBLEMAS DOS DADOS DE ÁREA ###########

# Substituir vírgulas por pontos e converter para numérico em todas as colunas de PIB
dados_regionais <- dados_regionais %>%
  mutate(across(starts_with("area"), ~ as.numeric(gsub(",", ".", .))))

##### RESOLVENDO OS PROBLEMAS DOS DADOS DO PRODUTO INDUSTRIAL ###########

# Selecione apenas as colunas de PIB industrial da base de dados_regionais
dados_completos2 <- dados_regionais %>%
  dplyr::select(starts_with("prod_"))

# Adicione colunas vazias para os anos 1997 e 1998
dados_completos2 <- dados_completos2 %>%
  mutate(prod_ind_1997 = NA, prod_ind_1998 = NA) %>%
  dplyr::select(sort(names(.)))  # Ordena as colunas por ano

# Substituir vírgulas por pontos e converter para numérico em todas as colunas de PIB
dados_completos2 <- dados_completos2 %>%
  mutate(across(starts_with("prod_"), ~ as.numeric(gsub(",", ".", .))))

#Criando os dados para os anos de 1997 e 1998, inferindo a taxa de crescimento de 1996 e 1999 
taxa_crescimento <- (dados_completos2$prod_ind_1999 / dados_completos2$prod_ind_1996)^(1/3) - 1
dados_completos2$prod_ind_1997 <- dados_completos2$prod_ind_1996 * (1 + taxa_crescimento)
dados_completos2$prod_ind_1998 <- dados_completos2$prod_ind_1997 * (1 + taxa_crescimento)

# Remova as colunas de PIB antigas de dados_regionais
dados_regionais <- dados_regionais %>%
  dplyr::select(-starts_with("prod_"))

# Adicione as colunas de PIB atualizadas de dados_completos em dados_regionais
dados_regionais <- bind_cols(dados_regionais, dados_completos2)


##### CALCULANDO A DENSIDADE DEMOGRÁFICA ######################

# Primeiro, crie uma função para calcular a densidade demográfica
calcular_densidade <- function(pop, area) {
  return(pop / area)
}

# Calcule a densidade para cada ano
for (ano in 1997:2021) {
  coluna_pop <- paste0("pop_", ano)  # Nome da coluna de população
  coluna_densidade <- paste0("densidade_", ano)  # Nome da coluna de densidade
  
  # Calcule a densidade e adicione como nova coluna
  dados_regionais[[coluna_densidade]] <- calcular_densidade(dados_regionais[[coluna_pop]], dados_regionais$area)
}


############## CALCULANDO O PIB PER CAPITA ##########################

# Calcular o PIB per capita para cada ano de 1997 a 2021
for (ano in 1997:2021) {
  coluna_pib <- paste0("pib_", ano)      # Nome da coluna de PIB
  coluna_pop <- paste0("pop_", ano)      # Nome da coluna de população
  coluna_pib_per_capita <- paste0("pib_per_capita_", ano)  # Nome da coluna de PIB per capita
  
  # Calcule o PIB per capita e adicione como nova coluna
  dados_regionais[[coluna_pib_per_capita]] <- dados_regionais[[coluna_pib]] / dados_regionais[[coluna_pop]]
}

write.csv2(dados_regionais, "dados_regionais2.csv", row.names = FALSE)










#################################### ÚLTIMOS DETALHES ###################################

write.csv2(base_modelo_final, "base_modelo_final.csv", row.names = FALSE)

# Utilizando o operador `%>%` explicitamente do `dplyr`
base_modelo_final <- base_modelo_final %>%
  dplyr::select(1:180)

base_modelo_final = fread(file = "./dados/base_modelo_final.csv", 
                          sep = ",",
                          dec = ".",
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          encoding = "UTF-8")

readLines("./dados/base_modelo_final.csv", n = 5)


# Substituir vírgulas por pontos e converter para numérico
cols_to_convert <- c("pib", "prodind", "densidade", "pibpercapita") # Liste as colunas a serem convertidas

base_modelo_final[, (cols_to_convert) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_to_convert]

