library(stringr)

# Lê o arquivo como texto bruto para corrigir manualmente
arquivo_raw <- readLines("./dados/dados_regionais3.csv", encoding = "UTF-8")

# Substitui vírgulas por pontos para o separador decimal
arquivo_corrigido <- gsub(",", ".", arquivo_raw)

# Escreve um arquivo temporário corrigido
writeLines(arquivo_corrigido, "./dados/dados_regionais_corrigido.csv")

# Lê o arquivo corrigido
base_modelo_final2 <- fread(file = "./dados/dados_regionais_corrigido.csv", 
                            sep = ".",  # Agora, somente vírgulas delimitam campos
                            dec = ",",  # Ponto como separador decimal
                            header = TRUE,
                            stringsAsFactors = FALSE,
                            encoding = "UTF-8")

base_modelo_final2 <- base_modelo_final2 %>%
  rename(nm_meso = Mesorregioes)

# Lê o arquivo como texto bruto para corrigir manualmente
arquivo_raw <- readLines("./dados/dados_regionais4.csv", encoding = "UTF-8")

# Substitui vírgulas por pontos para o separador decimal
arquivo_corrigido <- gsub(",", ".", arquivo_raw)

# Escreve um arquivo temporário corrigido
writeLines(arquivo_corrigido, "./dados/dados_regionais_corrigido2.csv")

#Monta as bases de dados
base_modelo_final3 = fread(file = "./dados/dados_regionais_corrigido2.csv", 
                           sep = ".",  # Agora, somente vírgulas delimitam campos
                           dec = ",",  # Ponto como separador decimal
                           header = TRUE,
                           stringsAsFactors = FALSE,
                           encoding = "UTF-8")

base_modelo_final2 <- base_modelo_final2 %>%
  rename(nm_meso = Mesorregioes)

base_modelo_final3 <- base_modelo_final3 %>%
  rename(nm_meso = Mesorregioes)

library(dplyr)

# Renomear a coluna densidade para densdem
base_modelo_final <- base_modelo_final %>%
  rename(densdem = densidade)

library(dplyr)
library(tidyr)

# Aplicar as três funções na coluna "nm_meso"
base_modelo_final3 <- base_modelo_final3 %>%
  mutate(
    nm_meso = nm_meso %>%
      convert_to_lowercase() %>%   # Converte para minúsculas
      remove_accentuation() %>%    # Remove acentuação
      remove_all_spaces()          # Remove todos os espaços
  )

# Verificar o resultado
head(base_modelo_final3$nm_meso)

# Preparar a base de densidade no formato longo
densidade_long <- data.frame()

for (periodo in names(densidade_binaria_brasil_ajustada)) {
  # Extrair a matriz de densidade para o período
  matriz <- as.data.frame(densidade_binaria_brasil_ajustada[[periodo]])
  matriz$nm_meso <- rownames(matriz)  # Adicionar a coluna de mesorregião
  
  # Transformar para o formato longo
  matriz_long <- matriz %>%
    pivot_longer(
      cols = -nm_meso,  # Todas as colunas, exceto 'nm_meso'
      names_to = "CD_CLASSIF_3",  # Nome da tecnologia
      values_to = "densrelat"  # Valor da densidade
    )
  
  # Adicionar a coluna de período
  matriz_long$periodo <- as.numeric(gsub("Periodo_", "", periodo))  # Extrair número do período
  
  # Combinar com a base final
  densidade_long <- bind_rows(densidade_long, matriz_long)
}

# Obter as tecnologias únicas da densidade de relatedness
tecnologias_unicas <- unique(densidade_long$CD_CLASSIF_3)

# Expandir a base para todas as combinações de nm_meso, periodo e tecnologia
base_expandida <- base_modelo_final3 %>%
  distinct(nm_meso, periodo) %>%  # Manter apenas combinações únicas de região e período
  tidyr::expand(nm_meso, periodo, CD_CLASSIF_3 = tecnologias_unicas)  # Criar todas as combinações

# Integrar as variáveis originais à base expandida
base_expandida <- base_expandida %>%
  left_join(base_modelo_final3, by = c("nm_meso", "periodo"))

# Integrar a densidade de relatedness à base expandida
base_final <- base_expandida %>%
  left_join(densidade_long, by = c("nm_meso", "periodo", "CD_CLASSIF_3"))

# Verificar a estrutura final
str(base_final)

# Conferir os primeiros registros
head(base_final)

library(dplyr)
library(tidyr)

# 1. Unir todas as dinâmicas em um único data frame
dinamicas_long <- bind_rows(
  lapply(names(dinamicas_brasil), function(periodo_nome) {
    df <- dinamicas_brasil[[periodo_nome]]
    
    # Extrair o período final da transição
    periodo_final <- as.numeric(gsub(".*_para_", "", periodo_nome))
    
    # Atribuir a transição ao período final
    df$periodo <- periodo_final
    return(df)
  }),
  .id = "transicao"  # Adiciona um identificador da transição
)

# Visualizar a base combinada de dinâmicas
head(dinamicas_long)

# 2. Ajustar a coluna tecnologia para coincidir com base_final
dinamicas_long <- dinamicas_long %>%
  rename(CD_CLASSIF_3 = tecnologia) %>%
  dplyr::select(nm_meso, periodo, CD_CLASSIF_3, entrada, saida, persistencia)

# 3. Integrar com a base_final usando left_join
base_final_com_dinamicas <- base_final %>%
  left_join(dinamicas_long, by = c("nm_meso", "periodo", "CD_CLASSIF_3"))

# 4. Visualizar a base resultante
str(base_final_com_dinamicas)
head(base_final_com_dinamicas)

# Criar a variável cd_uf com os dois primeiros dígitos de 'codigo'
base_final_com_dinamicas <- base_final_com_dinamicas %>%
  mutate(cd_uf = substr(as.character(codigo), 1, 2))

# Verificar a estrutura da base após a criação da variável
str(base_final_com_dinamicas)

base_final2 <- base_final_com_dinamicas

str(base_final2)

# Selecionar apenas as colunas desejadas
nova_base <- base_final2 %>%
  dplyr::select(nm_meso, CD_CLASSIF_3, periodo, densrelat)

# Exibir a estrutura da nova base
str(nova_base)

# Converter RCA contínua de `rca_brasil` para formato long
library(dplyr)
library(tidyr)

# Converter RCA continuo de `rca_brasil` para formato long
rca_brasil_long <- bind_rows(
  lapply(names(rca_brasil), function(periodo) {
    rca_brasil[[periodo]]$continua %>%
      mutate(periodo = as.numeric(sub("Periodo_", "", periodo))) %>%
      pivot_longer(-c(nm_meso, periodo), names_to = "CD_CLASSIF_3", values_to = "RCA_continua")
  })
)

# Unir os dados de RCA contínua à nova base
nova_base_com_rca <- nova_base %>%
  left_join(rca_brasil_long, by = c("nm_meso", "CD_CLASSIF_3", "periodo"))

# Converter RCA binário de `rca_brasil` para formato long
rca_binario_brasil_long <- bind_rows(
  lapply(names(rca_brasil), function(periodo) {
    rca_brasil[[periodo]]$binaria %>%
      mutate(periodo = as.numeric(sub("Periodo_", "", periodo))) %>%
      pivot_longer(-c(nm_meso, periodo), names_to = "CD_CLASSIF_3", values_to = "RCA_binaria")
  })
)

# Reorganizar as matrizes em formato consolidado
tabela_consolidada <- bind_rows(
  lapply(names(matrizes_brasil), function(periodo) {
    matrizes_brasil[[periodo]] %>%
      mutate(periodo = periodo) %>%
      pivot_longer(-c(nm_meso, periodo), names_to = "CD_CLASSIF_3", values_to = "numero_patentes")
  })
)

# Visualizar a tabela consolidada
head(tabela_consolidada)
str(tabela_consolidada)

# Adicionar a coluna numero_patentes à nova_base
nova_base4 <- nova_base3 %>%
  left_join(tabela_consolidada, by = c("nm_meso", "CD_CLASSIF_3", "periodo"))

# Remover o prefixo "Periodo_" e converter para numérico
tabela_consolidada <- tabela_consolidada %>%
  mutate(periodo = as.numeric(sub("Periodo_", "", periodo)))

library(dplyr)

# Criar a nova coluna com o somatório total de patentes por período
tabela_consolidada <- tabela_consolidada %>%
  group_by(periodo) %>%
  mutate(total_patentes_pais = sum(numero_patentes, na.rm = TRUE)) %>%
  ungroup()

tabela_consolidada <- tabela_consolidada %>%
  group_by(CD_CLASSIF_3, periodo) %>%  # Agrupar por tecnologia e período
  mutate(total_tecnologia = sum(numero_patentes, na.rm = TRUE)) %>%  # Somar as patentes em todas as regiões
  ungroup()  # Remover o agrupamento após a operação

# Adicionar a coluna numero_patentes à nova_base
nova_base4 <- nova_base3 %>%
  left_join(tabela_consolidada, by = c("nm_meso", "CD_CLASSIF_3", "periodo"))

nova_base5 <- nova_base4 %>%
  mutate(volume_h01 = ifelse(CD_CLASSIF_3 == "H01", numero_patentes, 0))

library(dplyr)

tecnologias_grupo <- c("H02", "H05", "G01", "H04", "F21", "B60", 
                       "C08", "G06", "G05", "G02", "F16", "B82", "C01", 
                       "A61", "G08")

nova_base6 <- nova_base5 %>%
  mutate(volume_tecnologias_relacionadas = ifelse(CD_CLASSIF_3 %in% tecnologias_grupo, numero_patentes, 0))

str(nova_base6)

# Adicionar a coluna numero_patentes à nova_base
nova_base5 <- nova_base4 %>%
  left_join(base_final2, by = c("nm_meso", "CD_CLASSIF_3", "periodo"))

str(base_final2)

# Adicionar as variáveis de nova_base6 na base_final2
base_final2 <- base_final2 %>%
  left_join(
    nova_base6 %>% 
      dplyr::select(nm_meso, periodo, CD_CLASSIF_3, volume_h01, volume_tecnologias_relacionadas, RCA_binaria),
    by = c("nm_meso", "periodo", "CD_CLASSIF_3")
  )

library(dplyr)

base_final2 <- base_final2 %>%
  rename(RTA_binaria = RCA_binaria)

base_final2 <- base_final2 %>%
  dplyr::select(-volume_total_tecnologia.x, -volume_total_tecnologia.y)

str(base_final2)
str(tabela_consolidada)

# Criar o volume total de tecnologias por região e período
volume_tecnologias <- tabela_consolidada %>%
  group_by(nm_meso, periodo) %>%
  summarise(volume_total_tecnologia = sum(numero_patentes, na.rm = TRUE), .groups = "drop")

# Extrair o número do texto e converter para numérico
volume_tecnologias <- volume_tecnologias %>%
  mutate(periodo = str_extract(periodo, "\\d+") %>% as.numeric())

str(volume_tecnologias)

base_final2 <- base_final2 %>%
  mutate(periodo = as.character(periodo))

volume_tecnologias <- volume_tecnologias %>%
  mutate(periodo = as.character(periodo))

base_final2 <- base_final2 %>%
  left_join(volume_tecnologias, by = c("nm_meso", "periodo"))


########################## CRIAR AS VARIÁVEIS DEFASADAS ##################################

library(slider)

# Calcular defasagens com `slider::slide()`
base_final2 <- base_final2 %>%
  group_by(nm_meso, CD_CLASSIF_3) %>%
  mutate(
    lag_densrelat = slide(densrelat, ~ .x[1], .before = 1),
    lag_densdem = slide(densdem, ~ .x[1], .before = 1),
    lag_pibpercapita = slide(pibpercapita, ~ .x[1], .before = 1),
    lag_populacao = slide(populacao, ~ .x[1], .before = 1),
    lag_pib = slide(pib, ~ .x[1], .before = 1),
    lag_prodind = slide(prodind, ~ .x[1], .before = 1),
    lag_trab = slide(trab, ~ .x[1], .before = 1),
    lag_uni = slide(uni, ~ .x[1], .before = 1),
    lag_volume_h01 = slide(volume_h01, ~ .x[1], .before = 1),
    lag_volume_tecnologias_relacionadas = slide(volume_tecnologias_relacionadas, ~ .x[1], .before = 1),
    lag_RTA_binaria = slide(RTA_binaria, ~ .x[1], .before = 1),
    lag_volume__tecnologias_totais = slide(volume_total_tecnologia, ~ .x[1], .before = 1)
  ) %>%
  ungroup()

# Verificar resultado
head(base_final2)

# Diagnóstico: Verificar a estrutura dentro de cada grupo
base_final2 %>%
  group_by(nm_meso, CD_CLASSIF_3) %>%
  arrange(periodo, .by_group = TRUE) %>%
  dplyr::select(nm_meso, CD_CLASSIF_3, periodo, densdem, lag_densdem) %>%
  print(n = 20)

# Filtrar os dados para excluir período 1
base_final3 <- base_final2 %>%
  filter(periodo > 1)

################################# RODAR OS MODELOS #################################

base_final3 <- base_final3 %>%
  mutate(
    lag_densrelat = as.numeric(unlist(lag_densrelat)),
    lag_densdem = as.numeric(unlist(lag_densdem)),
    lag_pibpercapita = as.numeric(unlist(lag_pibpercapita)),
    lag_prodind = as.numeric(unlist(lag_prodind)),
    lag_pib = as.numeric(unlist(lag_pib)),
    lag_populacao = as.numeric(unlist(lag_populacao)),
    lag_trab = as.numeric(unlist(lag_trab)),
    lag_uni = as.numeric(unlist(lag_uni)),
    lag_volume_h01 = as.numeric(unlist(lag_volume_h01)),
    lag_volume_tecnologias_relacionadas = as.numeric(unlist(lag_volume_tecnologias_relacionadas)),
    lag_RTA_binaria = as.numeric(unlist(lag_RTA_binaria)),
    lag_volume_tecnologias_totais = as.numeric(unlist(lag_volume__tecnologias_totais))
  )


base_final3 <- base_final3 %>%
  mutate(
    log_lag_densrelat = log(lag_densrelat + 1),
    log_lag_densdem = log(lag_densdem + 1),
    log_lag_pibpercapita = log(lag_pibpercapita + 1),
    log_lag_populacao = log(lag_populacao + 1),
    log_lag_pib = log(lag_pib + 1),
    log_lag_prodind = log(lag_prodind + 1),
    log_lag_trab = log(lag_trab + 1),
    log_lag_uni = log(lag_uni + 1),
    log_lag_volume_h01 = log(lag_volume_h01 + 1),
    log_lag_volume_tecnologias_relacionadas = log(lag_volume_tecnologias_relacionadas + 1),
    log_lag_RTA_binaria = log(lag_RTA_binaria + 1),
    log_lag_volume_tecnologias_totais = log(lag_volume_tecnologias_totais + 1)
  )

# Converter Variáveis Categóricas em Fatores:
base_final3 <- base_final3 %>%
  mutate(
    nm_meso = as.factor(nm_meso),
    CD_CLASSIF_3 = as.factor(CD_CLASSIF_3),
    periodo = as.factor(periodo),
    cd_uf = as.factor(cd_uf)
  )

# Filtrar os dados para excluir outras tecnologias
base_final4 <- base_final3 %>%
  dplyr::filter(CD_CLASSIF_3 %in% c("H01", "H02", "H05", "G01", "H04", "F21", "B60", 
                                    "C08", "G06", "G05", "G02", "F16", "B82", "C01", 
                                    "A61", "G08"))

base_final3_RTA1 <- base_final3 %>%
  dplyr::filter(RTA_binaria == 1)

base_final3_RTA0 <- base_final3 %>%
  dplyr::filter(RTA_binaria == 0)

base_final4_RTA1 <- base_final4 %>%
  dplyr::filter(RTA_binaria == 1)

base_final4_RTA0 <- base_final4 %>%
  dplyr::filter(RTA_binaria == 0)


# Selecionar variáveis específicas e garantir que são numéricas
base_selecionada <- base_final3 %>%
  dplyr::select(densrelat, pibpercapita, densdem, prodind, trab, uni) %>%
  mutate(across(everything(), as.numeric))  # Converter todas as colunas para numérico, se necessário

# Verificar a classe das variáveis
sapply(base_selecionada, class)

# Calcular a matriz de correlação
matriz_correlacao <- cor(base_selecionada, use = "complete.obs")

# Exibir a matriz no console
print(matriz_correlacao)

# Visualizar a matriz com corrplot
corrplot(matriz_correlacao, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black")

############## ENTRADA

library(fixest)
modelo_probit_fe_entrada1 <- feglm(
  entrada ~ lag_densrelat + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada1)

modelo_probit_fe_entrada2 <- feglm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada2)

modelo_probit_fe_entrada3 <- feglm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + log_lag_prodind + log_lag_volume_h01 | cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada3)

modelo_probit_fe_entrada4 <- feglm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + log_lag_trab + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada4)

modelo_probit_fe_entrada5 <- feglm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + log_lag_uni + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada5)

modelo_probit_entrada4 <- glm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + 
    log_lag_trab,
  family = binomial(link = "probit"), 
  data = base_final4
)

# Resumo do modelo
summary(modelo_probit_entrada4)

library(margins)
efeitos_marginais <- margins(modelo_probit_entrada4)
summary(efeitos_marginais)

#TESTE 1
modelo_probit_fe_entrada2 <- feglm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada2)

modelo_probit_fe_entrada4 <- feglm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + log_lag_trab + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada4)

#TESTE 2
modelo_probit_fe_entrada2 <- feglm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "logit"), 
  data = base_final3_RTA0,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada2)

modelo_probit_fe_entrada2 <- feglm(
  entrada ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "logit"), 
  data = base_final3_RTA1,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_entrada2)
















############## SAIDA

library(fixest)
modelo_probit_fe_saida1 <- feglm(
  saida ~ lag_densrelat | cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_saida1)

modelo_probit_fe_saida2 <- feglm(
  saida ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_saida2)

modelo_probit_fe_saida3 <- feglm(
  saida ~ lag_densrelat + log_lag_densdem + log_lag_prodind| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_saida3)

modelo_probit_fe_saida4 <- feglm(
  saida ~ lag_densrelat + log_lag_prodind + log_lag_trab| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_saida4)

modelo_probit_fe_saida5 <- feglm(
  saida ~ lag_densrelat + log_lag_prodind + log_lag_trab + log_lag_uni| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_saida5)

modelo_probit_saida4 <- glm(
  saida ~ lag_densrelat + log_lag_prodind + log_lag_trab,
  family = binomial(link = "probit"), 
  data = base_final4
)

# Resumo do modelo
summary(modelo_probit_saida4)

library(margins)
efeitos_marginais <- margins(modelo_probit_saida4)
summary(efeitos_marginais)


#TESTE 1
modelo_probit_fe_saida2 <- feglm(
  saida ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_saida2)

modelo_probit_fe_saida4 <- feglm(
  saida ~ lag_densrelat + log_lag_prodind + log_lag_trab + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_saida4)










############## PERSISTENCIA

library(fixest)
modelo_probit_fe_persistencia1 <- feglm(
  persistencia ~ lag_densrelat | cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_persistencia1)

modelo_probit_fe_persistencia2 <- feglm(
  persistencia ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_persistencia2)

modelo_probit_fe_persistencia3 <- feglm(
  persistencia ~ lag_densrelat + log_lag_prodind| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_persistencia3)

modelo_probit_fe_persistencia4 <- feglm(
  persistencia ~ lag_densrelat + log_lag_trab| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_persistencia4)

modelo_probit_fe_persistencia5 <- feglm(
  persistencia ~ lag_densrelat + log_lag_uni| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_persistencia5)

modelo_probit_persistencia1 <- glm(
  persistencia ~ lag_densrelat,
  family = binomial(link = "probit"), 
  data = base_final4
)

# Resumo do modelo
summary(modelo_probit_persistencia1)

library(margins)
efeitos_marginais <- margins(modelo_probit_persistencia1)
summary(efeitos_marginais)

#TESTE 1
modelo_probit_fe_persistencia2 <- feglm(
  persistencia ~ lag_densrelat + log_lag_densdem + log_lag_pibpercapita + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_persistencia2)

modelo_probit_fe_persistencia1 <- feglm(
  persistencia ~ lag_densrelat + log_lag_volume_h01| cd_uf + periodo + CD_CLASSIF_3,
  family = binomial(link = "probit"), 
  data = base_final4,
  cluster = ~ cd_uf + periodo + CD_CLASSIF_3
)

# Resumo do modelo
summary(modelo_probit_fe_persistencia1)













################################## COMPARANDO OS MODELOS ################################

library(fixest)

# Tabela comparativa de métricas
etable(modelo_mpl_entrada, modelo_probit_fe_entrada, tex = FALSE)
etable(modelo_mpl_saida, modelo_probit_fe_saida, tex = FALSE)
etable(modelo_mpl_persistencia, modelo_probit_fe_persistencia, tex = FALSE)


write_csv(base_final3, "base_final3.csv")







############################ CRIANDO TABELAS DAS VARIÁVEIS #############################

str(base_final2)

# Filtrar as observações onde CD_CLASSIF_3 é "H01" e selecionar as colunas nm_meso, periodo e densrelat
nova_base <- base_final2 %>%
  filter(CD_CLASSIF_3 == "H01") %>%
  dplyr::select(nm_meso, periodo, densrelat)

# Transformar de formato long para wide
nova_base_wide <- nova_base %>%
  pivot_wider(names_from = periodo, values_from = densrelat, names_prefix = "periodo_")

# Visualizar a nova base em formato wide
head(nova_base_wide)

# Salvar em um arquivo CSV
write.csv2(nova_base_wide, "nova_base_wide.csv", row.names = FALSE)

# Filtrar as observações onde CD_CLASSIF_3 é "H01" e selecionar as colunas nm_meso, periodo e outras
nova_base3 <- base_final4 %>%
  dplyr::select(nm_meso, periodo, entrada, saida, persistencia)

str(nova_base2)

# Contagem de 1, 0 e NA nas variáveis entrada, saida e persistencia
contagem <- nova_base3 %>%
  summarise(
    entrada_1 = sum(entrada == 1, na.rm = TRUE),
    entrada_0 = sum(entrada == 0, na.rm = TRUE),
    entrada_NA = sum(is.na(entrada)),
    
    saida_1 = sum(saida == 1, na.rm = TRUE),
    saida_0 = sum(saida == 0, na.rm = TRUE),
    saida_NA = sum(is.na(saida)),
    
    persistencia_1 = sum(persistencia == 1, na.rm = TRUE),
    persistencia_0 = sum(persistencia == 0, na.rm = TRUE),
    persistencia_NA = sum(is.na(persistencia))
  )

# Exibir o resultado
print(contagem)


