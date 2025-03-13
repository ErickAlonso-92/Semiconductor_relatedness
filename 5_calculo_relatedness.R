matriz_relatedness <- relatedness(matriz_coocorrencia_ipc)

# Carregar as bases
# patentes <- read.csv("caminho_para_base_patentes.csv")
# dados_consolidados2 <- read.csv("caminho_para_base_dados_consolidados2.csv")

# Fazer o merge para adicionar as colunas
semicondutor_inventor <- merge(dados_consolidados2, 
                             patentes[, c("NO_PEDIDO", "DT_ENTRADA_INPI", "DT_DEPOSITO")], 
                             by = "NO_PEDIDO", 
                             all.x = TRUE)

str(semicondutor_inventor)

# Verificar a base resultante
head(dados_consolidados2)

#Monta as bases de dados
semicondutor_inventor = fread(file = "./dados/semicondutor_inventor.csv", 
                         sep = ",",
                         dec = ".",
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8")

# Converter a coluna DT_DEPOSITO para o formato de data correto
semicondutor_inventor <- semicondutor_inventor %>%
  mutate(DT_DEPOSITO = as.Date(DT_DEPOSITO, format = "%d/%m/%Y"))

semicondutor_inventor <- semicondutor_inventor %>%
  mutate(DT_ENTRADA_INPI = as.Date(DT_ENTRADA_INPI, format = "%d/%m/%Y"))

# Definir os períodos de tempo correspondentes aos mandatos dos presidentes
semicondutor_inventor <- semicondutor_inventor %>%
  mutate(ano_deposito = case_when(
    DT_DEPOSITO >= as.Date("1900-01-01") & DT_DEPOSITO <= as.Date("1996-12-31") ~ "1996",
    DT_DEPOSITO >= as.Date("1997-01-01") & DT_DEPOSITO <= as.Date("1997-12-31") ~ "1997",
    DT_DEPOSITO >= as.Date("1998-01-01") & DT_DEPOSITO <= as.Date("1998-12-31") ~ "1998",
    DT_DEPOSITO >= as.Date("1999-01-01") & DT_DEPOSITO <= as.Date("1999-12-31") ~ "1999",
    DT_DEPOSITO >= as.Date("2000-01-01") & DT_DEPOSITO <= as.Date("2000-12-31") ~ "2000",
    DT_DEPOSITO >= as.Date("2001-01-01") & DT_DEPOSITO <= as.Date("2001-12-31") ~ "2001",
    DT_DEPOSITO >= as.Date("2002-01-01") & DT_DEPOSITO <= as.Date("2002-12-31") ~ "2002",
    DT_DEPOSITO >= as.Date("2003-01-01") & DT_DEPOSITO <= as.Date("2003-12-31") ~ "2003",
    DT_DEPOSITO >= as.Date("2004-01-01") & DT_DEPOSITO <= as.Date("2004-12-31") ~ "2004",
    DT_DEPOSITO >= as.Date("2005-01-01") & DT_DEPOSITO <= as.Date("2005-12-31") ~ "2005",
    DT_DEPOSITO >= as.Date("2006-01-01") & DT_DEPOSITO <= as.Date("2006-12-31") ~ "2006",
    DT_DEPOSITO >= as.Date("2007-01-01") & DT_DEPOSITO <= as.Date("2007-12-31") ~ "2007",
    DT_DEPOSITO >= as.Date("2008-01-01") & DT_DEPOSITO <= as.Date("2008-12-31") ~ "2008",
    DT_DEPOSITO >= as.Date("2009-01-01") & DT_DEPOSITO <= as.Date("2009-12-31") ~ "2009",
    DT_DEPOSITO >= as.Date("2010-01-01") & DT_DEPOSITO <= as.Date("2010-12-31") ~ "2010",
    DT_DEPOSITO >= as.Date("2011-01-01") & DT_DEPOSITO <= as.Date("2011-12-31") ~ "2011",
    DT_DEPOSITO >= as.Date("2012-01-01") & DT_DEPOSITO <= as.Date("2012-12-31") ~ "2012",
    DT_DEPOSITO >= as.Date("2013-01-01") & DT_DEPOSITO <= as.Date("2013-12-31") ~ "2013",
    DT_DEPOSITO >= as.Date("2014-01-01") & DT_DEPOSITO <= as.Date("2014-12-31") ~ "2014",
    DT_DEPOSITO >= as.Date("2015-01-01") & DT_DEPOSITO <= as.Date("2015-12-31") ~ "2015",
    DT_DEPOSITO >= as.Date("2016-01-01") & DT_DEPOSITO <= as.Date("2016-12-31") ~ "2016",
    DT_DEPOSITO >= as.Date("2017-01-01") & DT_DEPOSITO <= as.Date("2017-12-31") ~ "2017",
    DT_DEPOSITO >= as.Date("2018-01-01") & DT_DEPOSITO <= as.Date("2018-12-31") ~ "2018",
    DT_DEPOSITO >= as.Date("2019-01-01") & DT_DEPOSITO <= as.Date("2019-12-31") ~ "2019",
    DT_DEPOSITO >= as.Date("2020-01-01") & DT_DEPOSITO <= as.Date("2020-12-31") ~ "2020",
    DT_DEPOSITO >= as.Date("2021-01-01") & DT_DEPOSITO <= as.Date("2021-12-31") ~ "2021",
    DT_DEPOSITO >= as.Date("2022-01-01") & DT_DEPOSITO <= as.Date("2022-12-31") ~ "2021"
  ))

semicondutor_inventor <- semicondutor_inventor %>%
  mutate(ano_entrada = case_when(
    DT_ENTRADA_INPI >= as.Date("1900-01-01") & DT_ENTRADA_INPI <= as.Date("1996-12-31") ~ "1996",
    DT_ENTRADA_INPI >= as.Date("1997-01-01") & DT_ENTRADA_INPI <= as.Date("1997-12-31") ~ "1997",
    DT_ENTRADA_INPI >= as.Date("1998-01-01") & DT_ENTRADA_INPI <= as.Date("1998-12-31") ~ "1998",
    DT_ENTRADA_INPI >= as.Date("1999-01-01") & DT_ENTRADA_INPI <= as.Date("1999-12-31") ~ "1999",
    DT_ENTRADA_INPI >= as.Date("2000-01-01") & DT_ENTRADA_INPI <= as.Date("2000-12-31") ~ "2000",
    DT_ENTRADA_INPI >= as.Date("2001-01-01") & DT_ENTRADA_INPI <= as.Date("2001-12-31") ~ "2001",
    DT_ENTRADA_INPI >= as.Date("2002-01-01") & DT_ENTRADA_INPI <= as.Date("2002-12-31") ~ "2002",
    DT_ENTRADA_INPI >= as.Date("2003-01-01") & DT_ENTRADA_INPI <= as.Date("2003-12-31") ~ "2003",
    DT_ENTRADA_INPI >= as.Date("2004-01-01") & DT_ENTRADA_INPI <= as.Date("2004-12-31") ~ "2004",
    DT_ENTRADA_INPI >= as.Date("2005-01-01") & DT_ENTRADA_INPI <= as.Date("2005-12-31") ~ "2005",
    DT_ENTRADA_INPI >= as.Date("2006-01-01") & DT_ENTRADA_INPI <= as.Date("2006-12-31") ~ "2006",
    DT_ENTRADA_INPI >= as.Date("2007-01-01") & DT_ENTRADA_INPI <= as.Date("2007-12-31") ~ "2007",
    DT_ENTRADA_INPI >= as.Date("2008-01-01") & DT_ENTRADA_INPI <= as.Date("2008-12-31") ~ "2008",
    DT_ENTRADA_INPI >= as.Date("2009-01-01") & DT_ENTRADA_INPI <= as.Date("2009-12-31") ~ "2009",
    DT_ENTRADA_INPI >= as.Date("2010-01-01") & DT_ENTRADA_INPI <= as.Date("2010-12-31") ~ "2010",
    DT_ENTRADA_INPI >= as.Date("2011-01-01") & DT_ENTRADA_INPI <= as.Date("2011-12-31") ~ "2011",
    DT_ENTRADA_INPI >= as.Date("2012-01-01") & DT_ENTRADA_INPI <= as.Date("2012-12-31") ~ "2012",
    DT_ENTRADA_INPI >= as.Date("2013-01-01") & DT_ENTRADA_INPI <= as.Date("2013-12-31") ~ "2013",
    DT_ENTRADA_INPI >= as.Date("2014-01-01") & DT_ENTRADA_INPI <= as.Date("2014-12-31") ~ "2014",
    DT_ENTRADA_INPI >= as.Date("2015-01-01") & DT_ENTRADA_INPI <= as.Date("2015-12-31") ~ "2015",
    DT_ENTRADA_INPI >= as.Date("2016-01-01") & DT_ENTRADA_INPI <= as.Date("2016-12-31") ~ "2016",
    DT_ENTRADA_INPI >= as.Date("2017-01-01") & DT_ENTRADA_INPI <= as.Date("2017-12-31") ~ "2017",
    DT_ENTRADA_INPI >= as.Date("2018-01-01") & DT_ENTRADA_INPI <= as.Date("2018-12-31") ~ "2018",
    DT_ENTRADA_INPI >= as.Date("2019-01-01") & DT_ENTRADA_INPI <= as.Date("2019-12-31") ~ "2019",
    DT_ENTRADA_INPI >= as.Date("2020-01-01") & DT_ENTRADA_INPI <= as.Date("2020-12-31") ~ "2020",
    DT_ENTRADA_INPI >= as.Date("2021-01-01") & DT_ENTRADA_INPI <= as.Date("2021-12-31") ~ "2021",
    DT_ENTRADA_INPI >= as.Date("2022-01-01") & DT_ENTRADA_INPI <= as.Date("2022-12-31") ~ "2022"
  ))


########################################### AJEITANDO A BASE #########################################

#  Converta a base de dados para o formato data.table, caso ainda não esteja
setDT(semicondutor_inventor)

# 2. Filtrar apenas as patentes com inventores no Brasil
library(dplyr)
patentes_brasil <- semicondutor_inventor %>%
  filter(cd_pais_inventor == "BR")
patentes_total <- semicondutor_inventor

patentes_brasil <- as.data.table(patentes_brasil)
patentes_total <- as.data.table(patentes_total)

# 3. Selecionar os números de pedido únicos
patentes_unicas_br <- unique(patentes_brasil[, .(NO_PEDIDO)])
patentes_unicas_total <- unique(patentes_total[, .(NO_PEDIDO)])

# 4. Realizar a junção para adicionar as classes tecnológicas
patentes_brasil_completo <- merge(patentes_unicas_br, patentes_brasil[, .(NO_PEDIDO, CD_CLASSIF)], by = "NO_PEDIDO", all.x = TRUE)
patentes_total_completo <- merge(patentes_unicas_total, patentes_total[, .(NO_PEDIDO, CD_CLASSIF)], by = "NO_PEDIDO", all.x = TRUE)

# 5. Obter as informações dos inventores relacionados a cada pedido de patente
# Seleciona colunas NO_PEDIDO, NO_ORDEM_INVENTOR, nm_inventor e remove duplicatas
inventores_br <- patentes_brasil[, .(NO_PEDIDO, NO_ORDEM_INVENTOR, nm_inventor)]
inventores_total <- patentes_total[, .(NO_PEDIDO, NO_ORDEM_INVENTOR, nm_inventor)]
inventores_br <- unique(inventores_br)
inventores_total <-  unique(inventores_total)

# 6. Expandir as combinações de patentes, classes e inventores usando merge com all.x = TRUE
# Isso gera todas as combinações possíveis de classes tecnológicas e inventores por patente

dados_brasil_completos <- merge(patentes_brasil_completo, inventores_br, by = "NO_PEDIDO", allow.cartesian = TRUE)
dados_total_completos <- merge(patentes_total_completo, inventores_total, by = "NO_PEDIDO", allow.cartesian = TRUE)

# 7. Juntar os dados restantes da base semicondutor_inventor com dados_completos
dados_brasil_finais <- merge(dados_brasil_completos, semicondutor_inventor, 
                      by = c("NO_PEDIDO", "CD_CLASSIF", "NO_ORDEM_INVENTOR"), 
                      all.x = TRUE)
dados_total_finais <- merge(dados_total_completos, semicondutor_inventor, 
                             by = c("NO_PEDIDO", "CD_CLASSIF", "NO_ORDEM_INVENTOR"), 
                             all.x = TRUE)

str(dados_brasil_finais)

library(dplyr)

# Contagem das ocorrências de cada combinação de nm_meso e periodo
contagem <- dados_brasil_finais %>%
  group_by(nm_meso, periodo) %>%
  summarise(
    contagem_total = n(),  # Contagem total
    contagem_H01 = sum(CD_CLASSIF_3 == "H01")  # Contagem onde CD_CLASSIF_3 é igual a "H01"
  )

# Exibindo o resultado
print(contagem)

# Salvar a contagem em um arquivo CSV
write.csv(contagem, "contagem_meso_periodo.csv", row.names = FALSE)

##################### RELATEDNESS ############################################################

# Criar a nova coluna 'CD_CLASSIF_3' com os primeiros 4 caracteres de 'CD_CLASSIF'
dados_brasil_finais$CD_CLASSIF_3 <- substr(dados_brasil_finais$CD_CLASSIF, 1, 3)
dados_total_finais$CD_CLASSIF_3 <- substr(dados_total_finais$CD_CLASSIF, 1, 3)

# Verificar o resultado
head(dados_brasil_finais)
head(dados_total_finais)

################### DIVIDIR POR PERÍODO #######################################################

str(dados_brasil_finais)
str(dados_total_finais)

dados_brasil_finais <- dados_brasil_finais %>%
  mutate(periodo = case_when(
    ano_entrada >= 1997 & ano_entrada <= 2001 ~ "1",
    ano_entrada >= 2002 & ano_entrada <= 2005 ~ "2",
    ano_entrada >= 2006 & ano_entrada <= 2009 ~ "3",
    ano_entrada >= 2010 & ano_entrada <= 2013 ~ "4",
    ano_entrada >= 2014 & ano_entrada <= 2017 ~ "5",
    ano_entrada >= 2018 & ano_entrada <= 2021 ~ "6"
  ))

# Contando a quantidade de observações por intervalo de tempo
observacoes_por_intervalo <- dados_brasil_finais %>%
  group_by(periodo) %>%
  summarise(quantidade = n())

dados_total_finais <- dados_total_finais %>%
  mutate(periodo = case_when(
    ano_entrada >= 1997 & ano_entrada <= 2001 ~ "1",
    ano_entrada >= 2002 & ano_entrada <= 2005 ~ "2",
    ano_entrada >= 2006 & ano_entrada <= 2009 ~ "3",
    ano_entrada >= 2010 & ano_entrada <= 2013 ~ "4",
    ano_entrada >= 2014 & ano_entrada <= 2017 ~ "5",
    ano_entrada >= 2018 & ano_entrada <= 2021 ~ "6"
  ))

# Contando a quantidade de observações por intervalo de tempo
observacoes_por_intervalo2 <- dados_total_finais %>%
  group_by(periodo) %>%
  summarise(quantidade = n())

# Removendo NAs da coluna periodo em dados_brasil_finais
dados_brasil_finais <- dados_brasil_finais %>%
  filter(!is.na(periodo))

# Contando novamente as observações por intervalo de tempo
observacoes_por_intervalo <- dados_brasil_finais %>%
  group_by(periodo) %>%
  summarise(quantidade = n())

# Removendo NAs da coluna periodo em dados_total_finais
dados_total_finais <- dados_total_finais %>%
  filter(!is.na(periodo))

# Contando novamente as observações por intervalo de tempo
observacoes_por_intervalo2 <- dados_total_finais %>%
  group_by(periodo) %>%
  summarise(quantidade = n())


###################### CRIAR AS MATRIZES DE REGIÃO E TECNOLOGIA ################################

# Função para construir a matriz para cada período
criar_matriz <- function(base_dados) {
  # Filtrar por período e criar uma lista de matrizes
  matrizes_por_periodo <- base_dados %>%
    group_split(periodo) %>%  # Dividir os dados por período
    lapply(function(df) {
      # Criar matriz para o período
      df %>%
        group_by(nm_meso, CD_CLASSIF_3) %>%
        summarise(count = n(), .groups = "drop") %>%  # Contar número de patentes
        pivot_wider(
          names_from = CD_CLASSIF_3,  # Colunas: Classes tecnológicas
          values_from = count,       # Valores: Contagem de patentes
          values_fill = 0            # Substituir NA por 0
        )
    })
  
  # Retornar lista de matrizes
  names(matrizes_por_periodo) <- paste0("Periodo_", 1:length(matrizes_por_periodo))
  return(matrizes_por_periodo)
}

# Construir matrizes para cada base
matrizes_brasil <- criar_matriz(dados_brasil_finais)
matrizes_total <- criar_matriz(dados_total_finais)

# Exemplo: Visualizar a matriz do período 1 para "dados_brasil_finais"
print(matrizes_brasil[["Periodo_2"]])

# Função para calcular RCA contínua e binária com limitações
calcular_rca_completa <- function(base_dados) {
  # Lista para armazenar as matrizes de RCA por período
  lista_rca <- list()
  
  # Dividir a base por períodos
  base_por_periodo <- base_dados %>%
    group_split(periodo)
  
  # Iterar sobre cada período
  for (i in seq_along(base_por_periodo)) {
    df_periodo <- base_por_periodo[[i]]
    
    # Calcular RCA contínua
    matriz_long <- df_periodo %>%
      group_by(nm_meso, CD_CLASSIF_3) %>%
      summarise(patentes = n(), .groups = "drop") %>%
      group_by(CD_CLASSIF_3) %>%
      mutate(
        total_classe = sum(patentes, na.rm = TRUE),  # Total de patentes na classe tecnológica
        atende_limite_classe = total_classe >= 10   # Verificar limite da classe
      ) %>%
      group_by(nm_meso) %>%
      mutate(
        total_meso = sum(patentes, na.rm = TRUE),  # Total de patentes na mesorregião
        atende_limite_meso = total_meso >= 5      # Verificar limite da região
      ) %>%
      ungroup() %>%
      mutate(
        total_geral = sum(patentes, na.rm = TRUE), # Total geral de patentes
        RCA = (patentes / total_meso) / (total_classe / total_geral)  # Cálculo da RCA contínua
      )
    
    # Calcular RCA binária com condições detalhadas
    matriz_long <- matriz_long %>%
      mutate(
        RCA_binaria = case_when(
          RCA > 1 & atende_limite_classe & atende_limite_meso ~ 1,  # RCA > 1 e atende os limites
          RCA <= 1 & atende_limite_classe & atende_limite_meso ~ 0, # RCA <= 1 e atende os limites
          !atende_limite_classe | !atende_limite_meso ~ NA_real_    # Não atende os limites
        )
      )
    
    # Converter para formato matriz (contínua)
    matriz_rca_continua <- matriz_long %>%
      dplyr::select(nm_meso, CD_CLASSIF_3, RCA) %>%
      pivot_wider(names_from = CD_CLASSIF_3, values_from = RCA, values_fill = 0)
    
    # Converter para formato matriz (binária)
    matriz_rca_binaria <- matriz_long %>%
      dplyr::select(nm_meso, CD_CLASSIF_3, RCA_binaria) %>%
      pivot_wider(names_from = CD_CLASSIF_3, values_from = RCA_binaria, values_fill = 0)
    
    # Salvar as matrizes para o período
    lista_rca[[paste0("Periodo_", i)]] <- list(
      continua = matriz_rca_continua,
      binaria = matriz_rca_binaria
    )
  }
  
  return(lista_rca)
}

# Aplicar a função para as bases de dados
rca_brasil <- calcular_rca_completa(dados_brasil_finais)
rca_total <- calcular_rca_completa(dados_total_finais)

# Exemplo: Visualizar RCA binária do Período 4 para "dados_brasil_finais"
print(rca_brasil[["Periodo_1"]]$binaria)

# Função para salvar RCA contínuo e binário em arquivos CSV por período
salvar_rca_csv <- function(rca_lista, diretorio = ".") {
  # Criar o diretório, se não existir
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
    message("Diretório criado: ", diretorio)
  }
  
  # Iterar sobre cada período na lista de RCA
  for (periodo in names(rca_lista)) {
    # Obter os dados de RCA contínuo e binário
    rca_continua <- rca_lista[[periodo]]$continua
    rca_binaria <- rca_lista[[periodo]]$binaria
    
    # Salvar RCA contínuo
    nome_arquivo_continua <- file.path(diretorio, paste0("RCA_continua_", periodo, ".csv"))
    write.csv2(rca_continua, file = nome_arquivo_continua, row.names = FALSE, fileEncoding = "UTF-8")
    message("Arquivo salvo: ", nome_arquivo_continua)
    
    # Salvar RCA binário
    nome_arquivo_binaria <- file.path(diretorio, paste0("RCA_binaria_", periodo, ".csv"))
    write.csv2(rca_binaria, file = nome_arquivo_binaria, row.names = FALSE, fileEncoding = "UTF-8")
    message("Arquivo salvo: ", nome_arquivo_binaria)
  }
}

# Executar a função
salvar_rca_csv(rca_lista = rca_brasil, diretorio = "dados_rca_brasil")


###################### CRIAR AS MATRIZES DE PROXIMIDADE ################################

# Função para criar matrizes de coocorrência por período
criar_coocorrencia_por_periodo <- function(base_dados) {
  # Dividir os dados por período
  dados_por_periodo <- base_dados %>%
    group_split(periodo)
  
  # Lista para armazenar matrizes de coocorrência
  matrizes_coocorrencia <- list()
  
  # Iterar sobre os períodos
  for (i in seq_along(dados_por_periodo)) {
    # Dados do período
    dados_periodo <- dados_por_periodo[[i]]
    
    # Criar matriz binária
    matriz_binaria <- dados_periodo %>%
      group_by(nm_meso, CD_CLASSIF_3) %>%
      summarise(presente = 1, .groups = "drop") %>%
      pivot_wider(names_from = CD_CLASSIF_3, values_from = presente, values_fill = 0)
    
    # Converter para matriz numérica
    matriz <- as.matrix(matriz_binaria[,-1])  # Excluir coluna "nm_meso"
    rownames(matriz) <- matriz_binaria$nm_meso  # Usar "nm_meso" como nome das linhas
    
    # Produto matricial: matriz de coocorrência
    matriz_coocorrencia <- t(matriz) %*% matriz
    
    # Salvar no resultado
    matrizes_coocorrencia[[paste0("Periodo_", unique(dados_periodo$periodo))]] <- matriz_coocorrencia
  }
  
  return(matrizes_coocorrencia)
}

# Aplicar a função para os dados
matrizes_coocorrencia_brasil <- criar_coocorrencia_por_periodo(dados_brasil_finais)
matrizes_coocorrencia_total <- criar_coocorrencia_por_periodo(dados_total_finais)

# Exemplo: Visualizar a matriz de coocorrência para o Período 1 (dados do Brasil)
print(matrizes_coocorrencia_brasil[["Periodo_1"]])

# Exemplo: Visualizar a matriz de coocorrência para o Período 2 (dados totais)
print(matrizes_coocorrencia_total[["Periodo_2"]])



############################ DENSIDADE DE RELATEDNESS #############################


calcular_densidade_binaria <- function(RCA_binario, Matriz_proximidade) {
  densidade <- list()
  
  # Verificar períodos comuns
  periodos_comuns <- intersect(names(RCA_binario), names(Matriz_proximidade))
  if (length(periodos_comuns) == 0) {
    stop("Não há períodos comuns entre RCA_binario e Matriz_proximidade.")
  }
  
  # Iterar pelos períodos comuns
  for (periodo in periodos_comuns) {
    cat("Processando período:", periodo, "\n")
    
    RCA <- as.matrix(RCA_binario[[periodo]]$binaria[, -1])
    regioes <- dplyr::pull(RCA_binario[[periodo]]$binaria, 1)
    Prox <- Matriz_proximidade[[periodo]]
    
    tecnologias_comuns <- intersect(colnames(RCA), colnames(Prox))
    Prox_ajustada <- Prox[tecnologias_comuns, tecnologias_comuns, drop = FALSE]
    
    if (ncol(RCA) != nrow(Prox_ajustada)) {
      stop(paste("Dimensões inconsistentes no período:", periodo))
    }
    
    numerador <- RCA %*% Prox_ajustada
    denominador <- colSums(Prox_ajustada, na.rm = TRUE)
    denominador[denominador == 0] <- NA
    matriz_densidade <- sweep(numerador, 2, denominador, FUN = "/")
    matriz_densidade[is.nan(matriz_densidade) | is.infinite(matriz_densidade)] <- 0
    
    rownames(matriz_densidade) <- regioes
    densidade[[periodo]] <- matriz_densidade
  }
  
  return(densidade)
}

# Aplicar a função com RCA binário
densidade_binaria_brasil <- calcular_densidade_binaria(rca_brasil, matrizes_coocorrencia_brasil)
densidade_binaria_total <- calcular_densidade_binaria(rca_total, proximidade_total)



################################### AJUSTANDO PARA INCORPORAR NA BASE ####################################

# Função corrigida para salvar arquivos CSV compatíveis com Google Sheets
salvar_densidades_csv_google <- function(densidade_lista, dados_finais, nome_base, diretorio = ".") {
  # Obter todas as mesorregiões e classes tecnológicas
  todas_mesorregioes <- unique(dados_finais$nm_meso)
  todas_classes <- unique(dados_finais$CD_CLASSIF_3)
  
  for (periodo in names(densidade_lista)) {
    # Expandir a matriz de densidade para incluir todas as combinações
    densidade <- as.data.frame(densidade_lista[[periodo]])
    densidade$nm_meso <- rownames(densidade)  # Adicionar nomes das mesorregiões
    densidade_long <- reshape2::melt(densidade, id.vars = "nm_meso", variable.name = "CD_CLASSIF_3", value.name = "densidade")
    
    # Combinar com todas as mesorregiões e classes
    completo <- expand.grid(nm_meso = todas_mesorregioes, CD_CLASSIF_3 = todas_classes)
    completo <- merge(completo, densidade_long, by = c("nm_meso", "CD_CLASSIF_3"), all.x = TRUE)
    
    # Substituir NAs por zero
    completo$densidade[is.na(completo$densidade)] <- 0
    
    # Salvar em CSV diretamente com write.csv2 (compatível com separador ; e decimal .)
    nome_arquivo <- file.path(diretorio, paste0(nome_base, "_", periodo, ".csv"))
    write.csv2(completo, file = nome_arquivo, row.names = FALSE, fileEncoding = "UTF-8")
    
    message("Arquivo salvo: ", nome_arquivo)
  }
}

# Aplicar a função para salvar as densidades no formato ajustado
salvar_densidades_csv_google(
  densidade_lista = densidade_binaria_brasil_ajustada,
  dados_finais = dados_brasil_finais,
  nome_base = "densidade_brasil"
)

salvar_densidade_csv <- function(densidade_lista, diretorio = ".", prefixo = "densidade") {
  # Criar o diretório, se não existir
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
    message("Diretório criado: ", diretorio)
  }
  
  # Iterar sobre cada período na lista de densidade
  for (periodo in names(densidade_lista)) {
    # Obter a matriz de densidade para o período
    matriz_densidade <- densidade_lista[[periodo]]
    
    # Adicionar as tecnologias como nomes das colunas
    tecnologias <- colnames(matriz_densidade)
    matriz_densidade <- as.data.frame(matriz_densidade)
    matriz_densidade$nm_meso <- rownames(matriz_densidade)  # Adicionar regiões como a primeira coluna
    
    # Reorganizar a matriz para o formato correto
    matriz_densidade <- matriz_densidade[, c("nm_meso", tecnologias)]
    
    # Salvar a matriz em um arquivo CSV
    nome_arquivo <- file.path(diretorio, paste0(prefixo, "_", periodo, ".csv"))
    write.csv2(matriz_densidade, file = nome_arquivo, row.names = FALSE, fileEncoding = "UTF-8")
    message("Arquivo salvo: ", nome_arquivo)
  }
}

# Salvar as matrizes de densidade ajustadas para o Brasil
salvar_densidade_csv(
  densidade_lista = densidade_binaria_brasil_ajustada, 
  diretorio = "densidades_brasil", 
  prefixo = "densidade_binaria_brasil"
)


###################### ENTRADA, SAIDA E PERSISTENCIA ###############################


calcular_dinamicas <- function(lista_rca) {
  resultados <- list()
  
  for (i in 2:length(lista_rca)) {  # Começar no segundo período
    periodo_anterior <- lista_rca[[i - 1]]$binaria
    periodo_atual <- lista_rca[[i]]$binaria
    
    # Garantir alinhamento das tecnologias e regiões
    tecnologias <- intersect(colnames(periodo_anterior)[-1], colnames(periodo_atual)[-1])
    regioes <- intersect(periodo_anterior$nm_meso, periodo_atual$nm_meso)
    
    rca_ant <- periodo_anterior[match(regioes, periodo_anterior$nm_meso), tecnologias]
    rca_atual <- periodo_atual[match(regioes, periodo_atual$nm_meso), tecnologias]
    
    # Entrada: 0 -> 1 (com condições adicionais)
    entrada <- ifelse(
      rca_ant == 0 & rca_atual == 1, 1,  # Entrada válida (0 -> 1)
      ifelse(
        rca_ant == 0 & rca_atual == 0, 0,  # Permanece 0 (0 -> 0)
        NA                                 # Retorna NA em outros casos
      )
    )
    
    # Saída: 1 -> 0 (com condições adicionais)
    saida <- ifelse(
      rca_ant == 1 & rca_atual == 0, 1,  # Saída válida (1 -> 0)
      ifelse(
        rca_ant == 1 & rca_atual == 1, 0,  # Permanece 1 (1 -> 1)
        NA                                 # Retorna NA em outros casos
      )
    )
    
    # Persistência: 1 -> 1 (com condições adicionais)
    persistencia <- ifelse(
      rca_ant == 1 & rca_atual == 1, 1,  # Persistência válida (1 -> 1)
      ifelse(
        rca_ant == 0 & rca_atual == 0, 0,  # Permanece 0 (0 -> 0)
        NA                                 # Retorna NA em outros casos
      )
    )
    
    # Criar data frame com os resultados
    resultados[[paste0("Periodo_", i - 1, "_para_", i)]] <- data.frame(
      nm_meso = rep(regioes, length(tecnologias)),
      tecnologia = rep(tecnologias, each = length(regioes)),
      entrada = as.vector(entrada),
      saida = as.vector(saida),
      persistencia = as.vector(persistencia)
    )
  }
  
  return(resultados)
}

# Aplicar a função nas matrizes RCA
dinamicas_brasil <- calcular_dinamicas(rca_brasil)

# Visualizar os resultados para o primeiro período de transição
print(dinamicas_brasil[["Periodo_2_para_3"]])


########################## SALVAR ARQUIVOS ###############################

# Diretório onde os arquivos CSV serão salvos
diretorio <- "./resultados_dinamicas"

# Criar o diretório, caso não exista
if (!dir.exists(diretorio)) {
  dir.create(diretorio)
}

# Loop para salvar cada elemento da lista dinamicas_brasil
for (nome_periodo in names(dinamicas_brasil)) {
  # Extrair o data frame correspondente
  df <- dinamicas_brasil[[nome_periodo]]
  
  # Definir o nome do arquivo com base no período
  nome_arquivo <- paste0(diretorio, "/", nome_periodo, ".csv")
  
  # Salvar o data frame em CSV
  write.csv(df, file = nome_arquivo, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Mensagem de confirmação
  message("Arquivo salvo: ", nome_arquivo)
}

