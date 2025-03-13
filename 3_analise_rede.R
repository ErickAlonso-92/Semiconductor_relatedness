library(reshape2)
library(igraph)


################ MATRIZ DE COOCORRÊNCIA PARA ANALISAR A RELAÇÃO ENTRE OS CÓDIGOS IPC ################################

# A) ANÁLISE COM OS TRÊS PRIMEIROS CARACTERES DA CLASSIFICAÇÃO IPC 
##### AJEITANDO A BASE ###############

semicondutor_inventor <- semicondutor_inventor %>%
  mutate(CD_CLASSIF_3 = substr(CD_CLASSIF, 1, 3))

freq_cd_classif <- semicondutor_inventor %>%
  group_by(CD_CLASSIF_3) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(freq_cd_classif)

freq_cd_classif <- semicondutor_inventor %>%
  group_by(CD_CLASSIF_3) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Ajustar para evitar problemas com apenas um código por NO_PEDIDO
pares_codigos <- semicondutor_inventor %>%
  dplyr::select(NO_PEDIDO, CD_CLASSIF_3) %>%
  distinct() %>%  # Remover duplicatas dentro do mesmo pedido
  group_by(NO_PEDIDO) %>%
  filter(n() > 1) %>%  # Considerar apenas pedidos com mais de um código
  summarise(pares = list(combn(CD_CLASSIF_3, 2, simplify = FALSE))) %>%
  unnest(pares) %>%
  ungroup() %>%
  mutate(source = sapply(pares, "[", 1),
         target = sapply(pares, "[", 2)) %>%
  dplyr::select(source, target) %>%
  count(source, target, name = "weight")

# Filtrar as patentes que possuem pelo menos um código H01
patentes_com_h01 <- semicondutor_inventor %>%
  filter(CD_CLASSIF_3 == "H01") %>%
  pull(NO_PEDIDO) %>%  # Extrair os IDs das patentes com H01
  unique()

# Manter todos os códigos associados às patentes identificadas
base_filtrada <- semicondutor_inventor %>%
  filter(NO_PEDIDO %in% patentes_com_h01)

# Verificar o resultado
View(base_filtrada)

# Verificar os códigos tecnológicos mantidos
table(base_filtrada$CD_CLASSIF_3)

# Verificar se H01 está presente em todas as patentes
base_filtrada %>%
  group_by(NO_PEDIDO) %>%
  summarise(possui_h01 = any(CD_CLASSIF_3 == "H01")) %>%
  filter(!possui_h01)

table(base_filtrada$CD_CLASSIF_3)

# Criar pares de códigos tecnológicos por NO_PEDIDO
pares_codigos_h01 <- base_filtrada %>%
  dplyr::select(NO_PEDIDO, CD_CLASSIF_3) %>%
  distinct() %>%
  group_by(NO_PEDIDO) %>%
  filter(n() > 1) %>%  # Garantir que o pedido tenha mais de um código
  summarise(pares = list(combn(CD_CLASSIF_3, 2, simplify = FALSE)), .groups = "drop") %>%
  unnest(pares) %>%
  mutate(source = map_chr(pares, 1),
         target = map_chr(pares, 2)) %>%
  dplyr::select(source, target) %>%
  count(source, target, name = "weight")

# Filtrar patentes com pelo menos um código H01 e inventores brasileiros
patentes_brasileiras_h01 <- semicondutor_inventor %>%
  mutate(CD_CLASSIF_3 = substr(CD_CLASSIF, 1, 3)) %>%
  filter(cd_pais_inventor == "BR", CD_CLASSIF_3 == "H01") %>%
  pull(NO_PEDIDO) %>%  # Extrair os IDs das patentes que atendem aos critérios
  unique()

# Manter todos os códigos associados às patentes brasileiras com H01
base_filtrada_brasileira_h01 <- semicondutor_inventor %>%
  filter(NO_PEDIDO %in% patentes_brasileiras_h01)

# Verificar o resultado
View(base_filtrada_brasileira_h01)

pares_brasileiras_h01 <- base_filtrada_brasileira_h01 %>%
  dplyr::select(NO_PEDIDO, CD_CLASSIF_3) %>%
  distinct() %>%
  group_by(NO_PEDIDO) %>%
  filter(n() > 1) %>%  # Garantir que o pedido tenha mais de um código
  summarise(pares = list(combn(CD_CLASSIF_3, 2, simplify = FALSE)), .groups = "drop") %>%
  unnest(pares) %>%
  mutate(
    source = map_chr(pares, 1),
    target = map_chr(pares, 2)
  ) %>%
  dplyr::select(source, target) %>%
  count(source, target, name = "weight")

# Criar e salvar a rede para pares_codigos
rede_pares_codigos <- graph_from_data_frame(pares_codigos, directed = FALSE)
write_graph(rede_pares_codigos, "pares_codigos.graphml", format = "graphml")

# Criar e salvar a rede para pares_codigos_h01
rede_pares_codigos_h01 <- graph_from_data_frame(pares_codigos_h01, directed = FALSE)
write_graph(rede_pares_codigos_h01, "pares_codigos_h01.graphml", format = "graphml")

# Criar e salvar a rede para pares_brasileiras_h01
rede_pares_brasileiras_h01 <- graph_from_data_frame(pares_brasileiras_h01, directed = FALSE)
write_graph(rede_pares_brasileiras_h01, "pares_brasileiras_h01.graphml", format = "graphml")

################# ANÁLISE DE REDE ######################################################################


# Criar um objeto grafo com a matriz de coocorrência
grafo <- graph_from_adjacency_matrix(matriz_coocorrencia_ipc_total, mode = "undirected", weighted = TRUE)

# Criar o gráfico
g <- ggraph(grafo, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
  geom_node_point(aes(size = degree(grafo)), color = "blue") +
  geom_node_text(aes(label = name), repel = TRUE, max.overlaps = Inf) +
  theme_void()

# Exibir o gráfico
print(g)

# Salvar a imagem (ajuste o tamanho conforme necessário)
ggsave("rede_semicondutores.png", plot = g, width = 10, height = 8, dpi = 300)

write_graph(grafo, file = "grafo_coocorrencia.graphml", format = "graphml")

# Criar o grafo a partir da matriz de co-ocorrência
grafo <- graph_from_adjacency_matrix(matriz_coocorrencia_ipc, mode = "undirected", weighted = TRUE)

# Calcular o grau dos nós
grau_nos <- degree(grafo)

# Definir um threshold para considerar os nós mais relevantes
threshold <- quantile(grau_nos, 0.75)  # Por exemplo, 75% dos nós mais relevantes

# Obter os nomes dos nós relevantes
nos_relevantes <- names(grau_nos[grau_nos >= threshold])

# Criar um data frame com os nomes dos nós e seus graus
df_nos <- data.frame(name = V(grafo)$name, grau = grau_nos)

# Criar o gráfico
g <- ggraph(grafo, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
  geom_node_point(aes(size = grau), color = "blue") +
  geom_node_text(data = df_nos[df_nos$name %in% nos_relevantes, ], 
                 aes(label = name), repel = TRUE) +
  theme_void()

# Exibir o gráfico
print(g)

# Salvar a imagem (opcional)
ggsave("rede_semicondutores_relevantes.png", plot = g, width = 10, height = 8, dpi = 300)

## As Ligações mais relevantes

# Calculando as arestas mais pesadas
E(grafo)$weight <- E(grafo)$weight

# Filtrando as arestas com peso acima de um determinado limiar
threshold <- quantile(E(grafo)$weight, 0.75)
arestas_relevantes <- E(grafo)[E(grafo)$weight >= threshold]

# Criar subgrafo com as arestas mais relevantes
subgrafo <- subgraph.edges(grafo, arestas_relevantes)

# Visualizar o subgrafo com ggraph
g <- ggraph(subgrafo, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
  geom_node_point(aes(size = degree(subgrafo)), color = "blue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Exibir o gráfico
print(g)

# Salvar a imagem (opcional)
ggsave("rede_semicondutores_relevantes.png", plot = g, width = 10, height = 8, dpi = 300)


################## MÉTRICA DE CENTRALIDADE ###########################

# Criar grafo com a matriz de coocorrência brasileira
grafo_brasil <- graph_from_adjacency_matrix(
  matriz_coocorrencia_ipc, 
  mode = "undirected",  # Não direcionado
  weighted = TRUE,      # Considerar pesos
  diag = FALSE          # Remover loops
)

# Criar grafo com a matriz de coocorrência total
grafo_total <- graph_from_adjacency_matrix(
  matriz_coocorrencia_ipc_total, 
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# Adicionar rótulos aos nodos
V(grafo_brasil)$label <- rownames(matriz_coocorrencia_ipc)
V(grafo_total)$label <- rownames(matriz_coocorrencia_ipc_total)

# Exportar grafo brasileiro
write_graph(grafo_brasil, file = "grafo_brasil.graphml", format = "graphml")

# Exportar grafo total
write_graph(grafo_total, file = "grafo_total.graphml", format = "graphml")

####################################################################################################

############################### ANÁLISE DE REDE DOS INVENTORES ############################################

library(igraph)
library(dplyr)
library(purrr)

#Monta as bases de dados
inventores = fread(file = "./dados/semicondutor_inventor_selecionada.csv", 
                 sep = ",",
                 dec = ".",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8")

# Agrupar inventores por patente e criar pares de mesorregiões (para patentes com mais de um inventor)
pares_inventores <- inventores %>%
  group_by(NO_PEDIDO) %>%
  summarise(localizacoes = list(nm_meso)) %>%
  ungroup() %>%
  mutate(pares = map(localizacoes, function(x) {
    if(length(x) > 1) {
      # Criar combinações de pares de mesorregiões para patentes com mais de um inventor
      comb <- combn(x, 2)
      data.frame(localizacao_origem = comb[1,], localizacao_destino = comb[2,])
    } else {
      # Para patentes com apenas um inventor, incluir a mesorregião como um nó isolado
      data.frame(localizacao_origem = x, localizacao_destino = x)
    }
  })) %>%
  select(-localizacoes) %>%
  unnest(cols = pares)

# Contar quantas vezes cada par de mesorregiões aparece junto (peso das arestas)
arestas <- pares_inventores %>%
  count(localizacao_origem, localizacao_destino, name = "peso")

# Ver o resultado das arestas
print(arestas)

# Criar o grafo com base nas arestas, incluindo nós isolados
grafo <- graph_from_data_frame(arestas, directed = FALSE)

# Exportar o grafo para Gephi no formato .graphml
write_graph(grafo, file = "rede_inventores_mesorregioes_com_isolados.graphml", format = "graphml")



#########################################################################################################


str(dados_brasil_finais)
dados_brasil_finais$CD_CLASSIF_4 <- substr(dados_brasil_finais$CD_CLASSIF, 1, 4)
dados_filtrados_H01L_br <- dados_brasil_finais[dados_brasil_finais$CD_CLASSIF_4 == "H01L", ]

dados_total_finais$CD_CLASSIF_4 <- substr(dados_total_finais$CD_CLASSIF, 1, 4)
dados_filtrados_H01L_total <- dados_total_finais[dados_total_finais$CD_CLASSIF_4 == "H01L", ]



library(dplyr)

dados_filtrados_H01L_br <- dados_filtrados_H01L_br %>%
  dplyr::distinct(NO_PEDIDO, nm_inventor.x, .keep_all = TRUE)

dados_filtrados_H01L_total <- dados_filtrados_H01L_total %>%
  dplyr::distinct(NO_PEDIDO, nm_inventor.x, .keep_all = TRUE)

str(dados_filtrados_H01L_br)
# Contagem do número de vezes que cada cd_uf_inventor aparece por período
contagem_uf_por_periodo <- dados_filtrados_H01L_br %>%
  count(cd_uf_inventor, periodo)

# Contagem do número de vezes que cada mesorregião aparece por período
contagem_meso_por_periodo <- dados_filtrados_H01L_br %>%
  count(nm_meso, periodo)

dados_filtrados_H01L_br  <- dados_filtrados_H01L_br  %>%
  mutate(across(c("cd_uf_inventor"), ~convert_to_lowercase(.)))

dados_filtrados_H01L_br <- dados_filtrados_H01L_br %>%
  mutate(across(c("cd_uf_inventor"), ~remove_accentuation(.)))

dados_filtrados_H01L_br <- dados_filtrados_H01L_br %>%
  mutate(across(c("cd_uf_inventor"), ~remove_all_spaces(.)))

# Contagem do número de vezes que cada cd_uf_inventor aparece por período
contagem_pais_por_periodo <- dados_filtrados_H01L_total %>%
  count(cd_pais_inventor, periodo)

str(dados_filtrados_H01L_total)

dados_filtrados_H01L_total[, .N, by = NO_PEDIDO][, .N]

dados_unicos <- unique(dados_filtrados_H01L_total[, .(NO_PEDIDO)])

dados_filtrados <- merge(dados_total_finais, dados_unicos, by = "NO_PEDIDO")

dados_filtrados_semicondutores <- dados_filtrados[dados_filtrados$CD_CLASSIF_4 == "H01L", ]
dados_filtrados_CEARA <- dados_filtrados[dados_filtrados$cd_uf_inventor == "ce", ]
dados_filtrados_SP <- dados_filtrados[dados_filtrados$cd_uf_inventor == "sp", ]
dados_filtrados_SP$CD_CLASSIF_6 <- substr(dados_filtrados_SP$CD_CLASSIF, 1, 7)
dados_filtrados_BR <- dados_filtrados[dados_filtrados$cd_pais_inventor == "BR", ]
dados_filtrados_BR$CD_CLASSIF_6 <- substr(dados_filtrados_BR$CD_CLASSIF, 1, 7)
contagem_CEARA <- dados_filtrados_CEARA %>%
  count(CD_CLASSIF_6)
contagem_SP <- dados_filtrados_SP %>%
  count(CD_CLASSIF_6)
dados_filtrados_BR  <- dados_filtrados_BR  %>%
  mutate(across(c("cd_uf_inventor"), ~convert_to_lowercase(.)))
contagem_BR <- dados_filtrados_BR %>%
  count(CD_CLASSIF_6, cd_uf_inventor)

dados_filtrados_inventores <- merge(
  dados_total_finais[, .(NO_PEDIDO, nm_inventor.x, nm_meso)], 
  dados_unicos, 
  by = "NO_PEDIDO"
)

# Remover duplicatas com base nas colunas NO_PEDIDO e nm_complet_depositante
dados_filtrados_inventores2 <- dados_filtrados_inventores %>%
  distinct(NO_PEDIDO, nm_inventor.x, .keep_all = TRUE)

contagem_BR <- dados_filtrados_inventores2 %>%
  count(nm_meso)

str(contagem_BR)

dados_filtrados_inventores2[, nm_meso := ifelse(is.na(nm_meso), "exterior", nm_meso)]

# Criar pares de nm_meso para cada NO_PEDIDO
ligacoes <- dados_filtrados_inventores2[, {
  if (.N == 1) {
    # Caso com apenas uma nm_meso
    list(origem = nm_meso, destino = nm_meso)
  } else {
    # Caso com mais de uma nm_meso
    pares <- combn(nm_meso, 2, simplify = FALSE)
    list(origem = sapply(pares, `[`, 1), destino = sapply(pares, `[`, 2))
  }
}, by = NO_PEDIDO]

# Contar as ocorrências de cada ligação (independente da ordem)
ligacoes_contadas <- ligacoes[, .N, by = .(origem, destino)]

# Renomear a coluna de contagem para "peso"
setnames(ligacoes_contadas, "N", "peso")

# Visualizar a nova base
ligacoes_contadas

str(meso_map_dados)

# Converter meso_map_dados em data.table (caso não esteja)
meso_map_dados <- as.data.table(meso_map_dados)

# Capturar o primeiro caractere de CD_MESO e criar uma nova coluna regiao na base meso_map_dados
meso_map_dados[, regiao := substr(CD_MESO, 1, 1)]

# Fazer um merge com a base ligacoes_contadas para adicionar a coluna regiao correspondente à origem
ligacoes_contadas <- merge(
  ligacoes_contadas,
  meso_map_dados[, .(NM_MESO, regiao)], 
  by.x = "origem", 
  by.y = "NM_MESO", 
  all.x = TRUE
)

# Renomear a coluna regiao para regiao_origem
setnames(ligacoes_contadas, "regiao", "regiao_origem")

# Fazer outro merge para adicionar a coluna regiao correspondente ao destino
ligacoes_contadas <- merge(
  ligacoes_contadas,
  meso_map_dados[, .(NM_MESO, regiao)], 
  by.x = "destino", 
  by.y = "NM_MESO", 
  all.x = TRUE
)

# Renomear a coluna regiao para regiao_destino
setnames(ligacoes_contadas, "regiao", "regiao_destino")

# Visualizar a base resultante
ligacoes_contadas

ligacoes_contadas[, regiao_destino := ifelse(is.na(regiao_destino), 6, regiao_destino)]
ligacoes_contadas[, regiao_origem := ifelse(is.na(regiao_origem), 6, regiao_origem)]

str(ligacoes_contadas)

# Filtrar as bases para cada região
ligacoes_norte_centro_oeste <- ligacoes_contadas[regiao_origem %in% c("1", "5") | regiao_destino %in% c("1", "5")]
ligacoes_nordeste <- ligacoes_contadas[regiao_origem == "2" | regiao_destino == "2"]
ligacoes_sudeste <- ligacoes_contadas[regiao_origem == "3" | regiao_destino == "3"]
ligacoes_sul <- ligacoes_contadas[regiao_origem == "4" | regiao_destino == "4"]

# Verificar o número de registros em cada base
cat("Número de registros por região:\n")
cat("Norte e Centro-Oeste:", nrow(ligacoes_norte_centro_oeste), "\n")
cat("Nordeste:", nrow(ligacoes_nordeste), "\n")
cat("Sudeste:", nrow(ligacoes_sudeste), "\n")
cat("Sul:", nrow(ligacoes_sul), "\n")

# Função para criar um arquivo GraphML
exportar_para_graphml <- function(base, nome_arquivo) {
  # Criar um objeto de rede
  grafo <- graph_from_data_frame(
    d = base[, .(origem, destino, peso)], # Arestas com peso
    directed = FALSE                      # Grafo não direcionado
  )
  
  # Exportar o grafo em formato GraphML
  write_graph(grafo, nome_arquivo, format = "graphml")
  cat("Arquivo salvo:", nome_arquivo, "\n")
}

# Exportar as bases para GraphML
exportar_para_graphml(ligacoes_norte_centro_oeste, "ligacoes_norte_centro_oeste.graphml")
exportar_para_graphml(ligacoes_nordeste, "ligacoes_nordeste.graphml")
exportar_para_graphml(ligacoes_sudeste, "ligacoes_sudeste.graphml")
exportar_para_graphml(ligacoes_sul, "ligacoes_sul.graphml")

######################################      DEPOSITANTES      #####################################

# Caminho do arquivo
file_path <- "C:/Users/erick/Downloads/drive-download-20241219T144739Z-001/combined_dataV2.csv"

# Carregando a base de dados
combined_dataV2 <- read_csv(file_path)

combined_dataV2 <- combined_dataV2 %>%
  mutate(DT_ENTRADA_INPI = as.Date(DT_ENTRADA_INPI, format = "%d/%m/%Y"))

combined_dataV2 <- combined_dataV2 %>%
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

combined_dataV2 <- combined_dataV2 %>%
  mutate(periodo = case_when(
    ano_entrada >= 1997 & ano_entrada <= 2001 ~ "1",
    ano_entrada >= 2002 & ano_entrada <= 2005 ~ "2",
    ano_entrada >= 2006 & ano_entrada <= 2009 ~ "3",
    ano_entrada >= 2010 & ano_entrada <= 2013 ~ "4",
    ano_entrada >= 2014 & ano_entrada <= 2017 ~ "5",
    ano_entrada >= 2018 & ano_entrada <= 2021 ~ "6"
  ))

# Realizando o join para trazer as informações desejadas
dados_filtrados_depositantes2 <- dados_unicos %>%
  left_join(combined_dataV2 %>% 
              dplyr::select(NO_PEDIDO, nm_complet_depositante, cd_pais_depositante, cd_uf_depositante, cd_tipo_depositante, periodo),
            by = "NO_PEDIDO")

# Contagem do número de vezes que cada cd_uf_inventor aparece por período
contagem_uf_por_periodo2 <- dados_filtrados_depositantes %>%
  count(cd_pais_depositante)

dados_filtrados_depositantes_br <- dados_filtrados_depositantes2[dados_filtrados_depositantes2$cd_pais_depositante == "BR", ]

str(dados_filtrados_depositantes2)

# Remover duplicatas com base nas colunas NO_PEDIDO e nm_complet_depositante
dados_filtrados_depositantes3 <- dados_filtrados_depositantes2 %>%
  distinct(NO_PEDIDO, nm_complet_depositante, .keep_all = TRUE)

# Contagem do número de vezes que cada cd_uf_inventor aparece por período
contagem_uf_por_periodo2 <- dados_filtrados_depositantes3 %>%
  count(cd_tipo_depositante, periodo)

dados_filtrados_depositantes_br <- dados_filtrados_depositantes3[dados_filtrados_depositantes3$cd_pais_depositante == "BR", ]

# Contagem do número de vezes que cada cd_uf_inventor aparece por período
contagem_br_depositante <- dados_filtrados_depositantes_br %>%
  count(cd_tipo_depositante, periodo)

