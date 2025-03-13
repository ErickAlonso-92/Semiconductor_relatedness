semicondutor_patentecompleta = fread(file = "./dados/dados_completo.csv", 
                 sep = ",",
                 dec = ".",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8")

# Converter a coluna DT_DEPOSITO para o formato de data correto
semicondutor_patentecompleta <- semicondutor_patentecompleta %>%
  mutate(DT_DEPOSITO = as.Date(DT_DEPOSITO, format = "%d/%m/%Y"))

semicondutor_patentecompleta <- semicondutor_patentecompleta %>%
  mutate(DT_ENTRADA_INPI = as.Date(DT_ENTRADA_INPI, format = "%d/%m/%Y"))


# Definir os períodos de tempo correspondentes aos mandatos dos presidentes
semicondutor_patentecompleta <- semicondutor_patentecompleta %>%
  mutate(ano_deposito = case_when(
    DT_DEPOSITO >= as.Date("1900-01-01") & DT_DEPOSITO <= as.Date("1998-12-31") ~ "Antes de 1998",
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
    DT_DEPOSITO >= as.Date("2022-01-01") & DT_DEPOSITO <= as.Date("2022-12-31") ~ "2022"
  ))

semicondutor_patentecompleta <- semicondutor_patentecompleta %>%
  mutate(ano_entrada = case_when(
    DT_ENTRADA_INPI >= as.Date("1900-01-01") & DT_ENTRADA_INPI <= as.Date("1998-12-31") ~ "Antes de 1998",
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

# Criando a base de Inventor
semicondutor_base_inventor <- semicondutor_patentecompleta %>%
  select(NO_PEDIDO, NO_ORDEM_INVENTOR, nm_inventor, cd_pais_inventor, CD_CLASSIF, cd_uf_inventor, nm_cidade_inventor, cd_ibge_inventor, no_cep_inventor, DT_DEPOSITO, DT_ENTRADA_INPI, ano_deposito, ano_entrada)

#Juntando os dados do código de classificação de patente de semicondutores numa coluna só
semicondutor_base_inventor <- semicondutor_patentecompleta %>%
  group_by(NO_PEDIDO, NO_ORDEM_INVENTOR, nm_complet_inventor, cd_pais_inventor, CD_OUTRA_CLASSIF, cd_uf_inventor, nm_cidade_inventor, cd_ibge_inventor, no_cep_inventor, DT_DEPOSITO, DT_ENTRADA_INPI, ano_deposito, ano_entrada) %>%
  summarize(CD_CLASSIF = paste(unique(CD_CLASSIF), collapse = ", ")) %>%
  ungroup()

# Filtrar e eliminar duplicatas considerando a combinação de NO_PEDIDO e NO_ORDEM_INVENTOR
semicondutor_base_inventor_filtrado <- semicondutor_base_inventor %>%
  group_by(NO_PEDIDO, NO_ORDEM_INVENTOR) %>%
  summarise(across(everything(), first), .groups = 'drop')

write_csv(semicondutor_base_inventor, "semicondutor_inventor.csv")


# Contribuições
########################################################################################################################################################
########################################################################################################################################################
#inventor####################

# Selecione as colunas específicas
semicondutor_patentecompleta_selecionada <- semicondutor_patentecompleta %>%
  select(NO_PEDIDO, NO_ORDEM_INVENTOR, cd_pais_inventor, cd_uf_inventor, nm_cidade_inventor, nm_complet_inventor, cd_ibge_cidade_inventor, no_cep_inventor, DT_DEPOSITO, DT_ENTRADA_INPI, ano_deposito, ano_entrada)

# Filtrar e eliminar duplicatas considerando a combinação de NO_PEDIDO e NO_ORDEM_INVENTOR
semicondutor_patente_inventor_filtrado <- semicondutor_patentecompleta_selecionada %>%
  group_by(NO_PEDIDO, NO_ORDEM_INVENTOR) %>%
  summarise(across(everything(), first), .groups = 'drop')

# Contar o número total de inventores por patente
total_inventores_por_patente <- semicondutor_patente_inventor_filtrado %>%
  group_by(NO_PEDIDO) %>%
  summarise(total_inventores = n(), .groups = 'drop')  # Total de inventores por patente

# Juntar a contagem de inventores à base original
semicondutor_patente_inventor_filtrado <- semicondutor_patente_inventor_filtrado %>%
  left_join(total_inventores_por_patente, by = "NO_PEDIDO")

# Calcular a contribuição percentual de cada inventor
semicondutor_patente_inventor_filtrado <- semicondutor_patente_inventor_filtrado %>%
  mutate(contribuicao_inventor = 1 / total_inventores * 100)  # Cada inventor contribui igualmente

contribuicao_por_pais_periodo <- semicondutor_patente_inventor_filtrado %>%
  group_by(cd_pais_inventor, period) %>%
  summarise(total_contribuicao = sum(contribuicao_inventor), .groups = 'drop')  # Somar contribuições

######Jeito não fracionado

contagem_pais_periodo <- semicondutor_patente_inventor_filtrado %>%
  group_by(cd_pais_inventor, period) %>%
  summarise(contagem = n(), .groups = 'drop')

# # # # # # # # # # # # # # BRASIL # # # # # # # # # # # # # # # # # # # 

# Filtrar para incluir apenas inventores do Brasil
semicondutor_patente_inventor_br <- semicondutor_patente_inventor_filtrado %>%
  filter(cd_pais_inventor == "BR")

# Somar as contribuições por estado e período
contribuicao_por_estado_periodo <- semicondutor_patente_inventor_br %>%
  group_by(cd_uf_inventor, period) %>%
  summarise(total_contribuicao = sum(contribuicao_inventor), .groups = 'drop')  # Somar contribuições

######Jeito não fracionado

contagem_pais_periodo_br <- semicondutor_patente_inventor_br %>%
  group_by(cd_uf_inventor, period) %>%
  summarise(contagem = n(), .groups = 'drop')


########################################################################################################################################################
########################################################################################################################################################
#depositante####################

# Selecionar colunas específicas
semicondutor_patente_depositante_selecionado <- semicondutor_patentecompleta %>%
  select(NO_PEDIDO, NO_ORDEM_DEPOSITANTE, cd_tipo_depositante,
         nm_complet_depositante, cd_pais_depositante,
         cd_uf_depositante, nm_cidade_depositante,
         cd_ibge_cidade_depositante, no_cep_depositante,
         DT_DEPOSITO, period)

# Filtrar e eliminar duplicatas considerando a combinação de NO_PEDIDO e NO_ORDEM_DEPOSITANTE
semicondutor_patente_depositante_filtrado <- semicondutor_patente_depositante_selecionado %>%
  group_by(NO_PEDIDO, NO_ORDEM_DEPOSITANTE) %>%
  summarise(across(everything(), first), .groups = 'drop')

# Contar o número total de depositantes por patente
total_depositante_por_patente <- semicondutor_patente_depositante_filtrado %>%
  group_by(NO_PEDIDO) %>%
  summarise(total_depositante = n(), .groups = 'drop')  # Total de inventores por patente

# Juntar a contagem de depositantes à base original
semicondutor_patente_depositante_filtrado <- semicondutor_patente_depositante_filtrado %>%
  left_join(total_depositante_por_patente, by = "NO_PEDIDO")

# Calcular a contribuição percentual de cada depositante
semicondutor_patente_depositante_filtrado <- semicondutor_patente_depositante_filtrado %>%
  mutate(contribuicao_depositante = 1 / total_depositante * 100)  # Cada depositante contribui igualmente

contribuicao_por_pais_periodo2 <- semicondutor_patente_depositante_filtrado %>%
  group_by(cd_pais_depositante, period) %>%
  summarise(total_contribuicao = sum(contribuicao_depositante), .groups = 'drop')  # Somar contribuições

contribuicao_por_tipo_depositante <- semicondutor_patente_depositante_filtrado %>%
  group_by(cd_tipo_depositante, period) %>%
  summarise(total_contribuicao = sum(contribuicao_depositante), .groups = 'drop')  # Somar contribuições

######Jeito não fracionado

contagem_pais_periodo_depositante <- semicondutor_patente_depositante_filtrado %>%
  group_by(cd_pais_depositante, period) %>%
  summarise(contagem = n(), .groups = 'drop')

contagem_pais_periodo_tipo <- semicondutor_patente_depositante_filtrado %>%
  group_by(cd_tipo_depositante, period) %>%
  summarise(contagem = n(), .groups = 'drop')

contagem_pais_periodo_depositante2 <- semicondutor_patente_depositante_filtrado %>%
  group_by(nm_complet_depositante, period) %>%
  summarise(contagem = n(), .groups = 'drop')

# # # # # # # # # # # # # # BRASIL # # # # # # # # # # # # # # # # # # # 

# Filtrar para incluir apenas inventores do Brasil
semicondutor_patente_depositante_br <- semicondutor_patente_depositante_filtrado %>%
  filter(cd_pais_depositante == "BR")

# Somar as contribuições por estado e período
contribuicao_por_estado_periodo <- semicondutor_patente_depositante_br %>%
  group_by(cd_uf_depositante, period) %>%
  summarise(total_contribuicao = sum(contribuicao_depositante), .groups = 'drop')  # Somar contribuições

contribuicao_por_tipo_depositante_br <- semicondutor_patente_depositante_br  %>%
  group_by(cd_tipo_depositante, period) %>%
  summarise(total_contribuicao = sum(contribuicao_depositante), .groups = 'drop') 

######Jeito não fracionado

contagem_pais_periodo_depositante_br <- semicondutor_patente_depositante_br %>%
  group_by(cd_uf_depositante, period) %>%
  summarise(contagem = n(), .groups = 'drop')

contagem_br_periodo_tipo <- semicondutor_patente_depositante_br %>%
  group_by(cd_tipo_depositante, period) %>%
  summarise(contagem = n(), .groups = 'drop')

contagem_pais_periodo_depositante2br <- semicondutor_patente_depositante_br %>%
  group_by(nm_complet_depositante, period) %>%
  summarise(contagem = n(), .groups = 'drop')




####################################################################################################


semicondutor_inventor = fread(file = "./dados/semicondutor_inventor.csv", 
                                     sep = ",",
                                     dec = ".",
                                     header = TRUE,
                                     stringsAsFactors = FALSE,
                                     encoding = "UTF-8")

#Fazer a contagem de cidades
semicondutor_inventor_contagem <- semicondutor_inventor %>%
  mutate(cidade_estado = paste(nm_cidade_inventor, cd_uf_inventor, sep = ", "))

contagem_cidades <- semicondutor_inventor_contagem %>%
  group_by(cidade_estado) %>%
  summarise(contagem = n(), .groups = 'drop')

write_csv(contagem_cidades, "contagem_cidades.csv")

#Fazer a contagem de classificações
# Passo 1: Excluir quaisquer possíveis espaços ou caracteres não desejados das colunas de classificação
semicondutor_inventor <- semicondutor_inventor %>%
  mutate(
    CD_CLASSIF = trimws(CD_CLASSIF),
    CD_OUTRA_CLASSIF = trimws(CD_OUTRA_CLASSIF)
  )

# Passo 2: Separar classificações múltiplas em linhas diferentes
contagem_classificacoes <- semicondutor_inventor %>%
  # Seleciona as colunas relevantes
  select(CD_CLASSIF, CD_OUTRA_CLASSIF) %>%
  # Transforma para formato longo
  pivot_longer(
    cols = everything(), 
    names_to = "tipo_classif", 
    values_to = "classificacao"
  ) %>%
  # Separa classificações múltiplas em linhas diferentes
  separate_rows(classificacao, sep = ",\\s*") %>%
  # Remove possíveis espaços adicionais
  mutate(classificacao = trimws(classificacao)) %>%
  # Conta a frequência de cada classificação
  group_by(classificacao) %>%
  summarise(contagem = n(), .groups = 'drop') %>%
  arrange(desc(contagem))

write.csv(contagem_classificacoes, file = "contagem_classificacoes.csv", row.names = FALSE)

#Fazer a contagem de depósitos de patentes
contagem_depositos <- semicondutor_inventor %>%
  # Selecionar a coluna de interesse
  select(NO_PEDIDO) %>%
  # Remover duplicatas
  distinct(NO_PEDIDO) %>%
  # Contar o número de depósitos únicos
  summarise(contagem_depositos = n())

write.csv(contagem_depositos, file = "contagem_depositos.csv", row.names = FALSE)

#Fazer a contagem de inventor e quantos são brasileiros
nova_base <- semicondutor_inventor %>%
  # Agrupar por número do pedido
  group_by(NO_PEDIDO) %>%
  # Contar o total de inventores por depósito de patente
  summarise(
    TOTAL_INVENTOR = n_distinct(NO_ORDEM_INVENTOR),  # Contar inventores únicos
    BRASILEIRO_INVENTOR = sum(cd_pais_inventor == "BR")  # Contar inventores brasileiros
  ) %>%
  # Ordenar por NO_PEDIDO se desejado (opcional)
  arrange(NO_PEDIDO)

write.csv(nova_base, file = "total_inventores_brasileiros.csv", row.names = FALSE)

# Criar uma nova coluna com os quatro primeiros caracteres da classificação
contagem_classificacoes$classificacao_4 <- substr(contagem_classificacoes$classificacao, 1, 4)

# Agrupar e contar a frequência das classificações pelos primeiros quatro caracteres
contagem_por_4_caracteres <- contagem_classificacoes %>%
  group_by(classificacao_4) %>%
  summarise(contagem_total = sum(contagem))

# Ver o resultado
print(contagem_por_4_caracteres)

write.csv(contagem_por_4_caracteres, "contagem_por_4_caracteres.csv", row.names = FALSE)

