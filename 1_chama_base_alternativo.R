# Definir o caminho da pasta e os nomes dos arquivos
caminho_pasta <- "C:/Temp/Downloads/tabelas2022-20240821T130113Z-001/tabelas2022/dados"

# Nome dos arquivos
nome_arquivo_ipc_campo_tec <- "badepiv9_ptn_ipc_campo_tec.csv"
nome_arquivo_inventor <- "badepiv9_ptn_inventor.csv"
nome_arquivo_patentes <- "badepiv9_ptn_deposito.csv"  # Nome adaptado para 'patentes'
nome_arquivo_final <- "basefinal.xlsx"

# Caminhos completos para os arquivos
caminho_arquivo_ipc_campo_tec <- file.path(caminho_pasta, nome_arquivo_ipc_campo_tec)
caminho_arquivo_inventor <- file.path(caminho_pasta, nome_arquivo_inventor)
caminho_arquivo_patentes <- file.path(caminho_pasta, nome_arquivo_patentes)
caminho_arquivo_final <- file.path(caminho_pasta, nome_arquivo_final)

# Ler o arquivo CSV para dados IPC campo tec
codigo_ipc <- read.csv(caminho_arquivo_ipc_campo_tec, sep = ",")  # Ajuste o separador se necessário

# Filtrar as linhas onde o CODIGO_IPC está relacionado a semicondutores "H01L" e manter somente a variável NO_PEDIDO
todas_patente <- codigo_ipc %>%
  dplyr::select(NO_PEDIDO) %>%
  distinct()

# Juntar as bases 'codigo_ipc' e as patentes de semicondutores filtradas
dados_combinado <- inner_join(codigo_ipc, todas_patente, by = "NO_PEDIDO")

# Ler o arquivo CSV para dados de inventores
dados_inventor <- read.csv(caminho_arquivo_inventor, sep = ",")  # Ajuste o separador se necessário

dados_inventor <- dados_inventor %>%
  rename(NO_ORDEM_INVENTOR = no_ordem)

dados_inventor <- dados_inventor %>%
  rename(NO_PEDIDO = no_pedido)

dados_inventor <- dados_inventor %>%
  rename(nm_inventor = nm_complet_pfpj)

dados_inventor <- dados_inventor %>%
  rename(cd_pais_inventor = cd_pais_pfpj)

dados_inventor <- dados_inventor %>%
  rename(cd_uf_inventor = cd_uf_pfpj)

dados_inventor <- dados_inventor %>%
  rename(nm_cidade_inventor = nm_cidade_pfpj)

dados_inventor <- dados_inventor %>%
  rename(cd_ibge_inventor = cd_ibge_cidade)

dados_inventor <- dados_inventor %>%
  rename(no_cep_inventor = no_cep_pfpj)

# Juntar com a base de inventores
dados_final <- inner_join(dados_combinado, dados_inventor, by = c("NO_PEDIDO" = "NO_PEDIDO"), relationship = "many-to-many")

dados_final <- dados_final %>%
  filter(cd_pais_inventor == "BR")

# Preenchendo os valores ausentes de `nm_cidade_inventor` baseado no `nm_inventor`
dados_filtrados <- dados_final %>%
  group_by(nm_inventor) %>% # Agrupa por nome do inventor
  mutate(
    nm_cidade_inventor = ifelse(
      is.na(nm_cidade_inventor),
      first(na.omit(nm_cidade_inventor)), # Seleciona o primeiro valor não ausente
      nm_cidade_inventor
    )
  ) %>%
  ungroup()


# Contar valores vazios
num_vazio <- sum(dados_filtrados$nm_cidade_inventor == "", na.rm = TRUE)
cat("Número de valores vazios:", num_vazio, "\n")

#Filtrar observações onde ambas as colunas não estão vazias ou ausentes
dados_final <- dados_final %>%
  filter(!(is.na(nm_cidade_inventor) & (is.na(no_cep_inventor) | no_cep_inventor == "")) &
           !(nm_cidade_inventor == "" & (is.na(no_cep_inventor) | no_cep_inventor == "")))

library(dplyr)

# Filtrar observações que não contêm "/" em ambas as colunas
dados_final <- dados_final %>%
  filter(!grepl("/", nm_cidade_inventor, fixed = TRUE) &
           !grepl("/", no_cep_inventor, fixed = TRUE))

# Função para converter texto em letras minúsculas
convert_to_lowercase <- function(x) {
  if (is.character(x)) {
    return(tolower(x)) # Converte todo o texto para minúsculas
  } else {
    return(x) # Retorna valores não-caracteres inalterados
  }
}

# Aplicar a transformação às colunas "nm_inventor" e "nm_cidade_inventor"
dados_final <- dados_final %>%
  mutate(across(c("nm_inventor", "nm_cidade_inventor", "cd_uf_inventor"), ~convert_to_lowercase(.)))

remove_accentuation <- function(x) {
  if (is.character(x)) {
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    return(x)
  } else {
    return(x) # Retorna valores não-caracteres inalterados
  }
}

dados_final <- dados_final %>%
  mutate(across(c("nm_inventor", "nm_cidade_inventor"), ~remove_accentuation(.)))

# Função para remover todos os espaços de uma string
remove_all_spaces <- function(x) {
  if (is.character(x)) {
    x <- gsub("\\s", "", x) # Remove todos os espaços
    return(x)
  } else {
    return(x) # Retorna valores não-caracteres inalterados
  }
}

# Aplicar a transformação às colunas "nm_inventor" e "nm_cidade_inventor"
dados_final <- dados_final %>%
  mutate(across(c("nm_inventor", "nm_cidade_inventor"), ~remove_all_spaces(.)))


#Monta as bases de dados
divisoes_brasileiras = fread(file = "./dados/DTB_2022_Municipio.csv", 
                 sep = ",",
                 dec = ".",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8")

divisoes_brasileiras  <- divisoes_brasileiras  %>%
  mutate(across(c("nm_muni", "nm_micro", "nm_meso", "nm_rg_intermediaria", "nm_rg_imediata", "nm_uf"), ~convert_to_lowercase(.)))

divisoes_brasileiras <- divisoes_brasileiras %>%
  mutate(across(c("nm_muni", "nm_micro", "nm_meso", "nm_rg_intermediaria", "nm_rg_imediata", "nm_uf"), ~remove_accentuation(.)))

divisoes_brasileiras <- divisoes_brasileiras %>%
  mutate(across(c("nm_muni", "nm_micro", "nm_meso", "nm_rg_intermediaria", "nm_rg_imediata", "nm_uf"), ~remove_all_spaces(.)))

library(dplyr)

dados_final <- dados_final %>%
  filter(nm_cidade_inventor != "") %>%  # Filtrar observações onde "nm_cidade_inventor" não está vazio
  mutate(cidade_estado = paste0(nm_cidade_inventor, cd_uf_inventor))

library(dplyr)

# Criar uma base com as cidades distintas
cidades_distintas <- dados_final %>%
  dplyr::select(cidade_estado) %>%   # Seleciona apenas a coluna "cidade_estado"
  distinct()                  # Remove duplicatas

library(dplyr)

# Transformar a coluna "nm_uf" em letras maiúsculas
divisoes_brasileiras <- divisoes_brasileiras %>%
  mutate(nm_uf = toupper(nm_uf))

library(dplyr)

# Criar a nova coluna "cidade_estado"
divisoes_brasileiras <- divisoes_brasileiras %>%
  mutate(cidade_estado = paste0(nm_muni, nm_uf))

library(dplyr)

# Criar a coluna "correspondencia" na base "cidades_distintas"
cidades_distintas <- cidades_distintas %>%
  mutate(correspondencia = ifelse(cidade_estado %in% divisoes_brasileiras$cidade_estado, 1, 0))

library(dplyr)

# Contar os valores 1 e 0 na coluna "correspondencia"
contagem_correspondencia <- cidades_distintas %>%
  count(correspondencia)

# Exibir os resultados
print(contagem_correspondencia)

library(dplyr)

# Adicionar a coluna "nm_meso" com base na correspondência de "cidade_estado"
cidades_distintas <- cidades_distintas %>%
  dplyr::left_join(divisoes_brasileiras %>% dplyr::select(cidade_estado, nm_meso), by = "cidade_estado")

# Juntar as colunas "correspondencia" e "nm_meso" da base "cidades_distintas" à base "dados_final"
dados_final <- dados_final %>%
  left_join(cidades_distintas %>% dplyr::select(cidade_estado, correspondencia, nm_meso), by = "cidade_estado")

library(dplyr)

# Filtrar observações onde correspondencia == 0 e cd_pais_inventor == "BR"
dados_filtrados <- dados_final %>%
  filter(correspondencia == 0, cd_pais_inventor == "BR")

# Remover as linhas filtradas de "dados_final"
dados_final <- dados_final %>%
  anti_join(dados_filtrados, by = c("NO_PEDIDO", "cd_pais_inventor", "correspondencia"))

library(dplyr)

# Criar uma base com as cidades distintas
patentes_distintas <- dados_filtrados %>%
  dplyr::select(NO_PEDIDO) %>%   # Seleciona apenas a coluna "cidade_estado"
  distinct()  

library(dplyr)

# Criar uma base filtrada removendo os NO_PEDIDO presentes em patentes_distintas e com cd_pais_inventor == "BR"
dados_final <- dados_final %>%
  filter(!(NO_PEDIDO %in% patentes_distintas$NO_PEDIDO & cd_pais_inventor == "BR"))

# Pesquisa por uma cidade específica
cidade_pesquisada <- "Humaita"

resultado <- dados_filtrados %>%
  dplyr::filter(nm_cidade_inventor == cidade_pesquisada) %>%
  dplyr::select(no_cep_inventor)

# Exibir os CEPs encontrados
print(resultado, n=50)

#Monta as bases de dados
info_recuperada = fread(file = "./dados/info_recuperadas.csv", 
                             sep = ",",
                             dec = ".",
                             header = TRUE,
                             stringsAsFactors = FALSE,
                             encoding = "UTF-8")

library(dplyr)

# Criar a base "patentes_distintas" com valores distintos de "no_pedido"
patentes_distintas <- info_recuperada %>%
  distinct(no_pedido)

patentes_distintas <- patentes_distintas %>%
  rename(NO_PEDIDO = no_pedido)

# Juntar as bases 'codigo_ipc' e as patentes de semicondutores filtradas
dados_combinado2 <- inner_join(codigo_ipc, patentes_distintas, by = "NO_PEDIDO")

# Juntar com a base de inventores
dados_final2 <- inner_join(dados_combinado2, dados_inventor, by = c("NO_PEDIDO" = "NO_PEDIDO"), relationship = "many-to-many")

library(dplyr)

# Filtrar somente as patentes brasileiras (cd_pais_inventor == "BR")
dados_final_br <- dados_final2 %>%
  filter(cd_pais_inventor == "BR")

library(dplyr)

info_recuperada2  <- info_recuperada  %>%
  mutate(across(c("inventor"), ~convert_to_lowercase(.)))

info_recuperada2 <- info_recuperada2 %>%
  mutate(across(c("inventor"), ~remove_accentuation(.)))

info_recuperada2 <- info_recuperada2 %>%
  mutate(across(c("inventor"), ~remove_all_spaces(.)))

dados_final_br2  <- dados_final_br  %>%
  mutate(across(c("nm_inventor"), ~convert_to_lowercase(.)))

dados_final_br2 <- dados_final_br2 %>%
  mutate(across(c("nm_inventor"), ~remove_accentuation(.)))

dados_final_br2 <- dados_final_br2 %>%
  mutate(across(c("nm_inventor"), ~remove_all_spaces(.)))

# Juntar a base "dados_final_br" com "info_recuperada" usando "NO_PEDIDO" e "inventor"
dados_final_br3 <- dados_final_br2 %>%
  left_join(info_recuperada2 %>% dplyr::select(no_pedido, inventor, co_cid), 
            by = c("NO_PEDIDO" = "no_pedido", "nm_inventor" = "inventor"))

# Contar o número de NAs na coluna "co_cid"
num_na_co_cid <- sum(is.na(dados_final_br4$co_cid))

# Exibir o resultado
cat("Número de NAs na coluna co_cid:", num_na_co_cid, "\n")

#Filtrar observações onde ambas as colunas não estão vazias ou ausentes
dados_final_br4 <- dados_final_br3 %>%
  filter(!(is.na(nm_cidade_inventor) & (is.na(no_cep_inventor) | no_cep_inventor == "")) &
           !(nm_cidade_inventor == "" & (is.na(no_cep_inventor) | no_cep_inventor == "")))

library(readr)

write_csv(dados_final_br4, "dados_final_br4.csv")

#Monta as bases de dados
dados_final_br5 = fread(file = "./dados/dados_final_br4.csv", 
                        sep = ",",
                        dec = ".",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8")

dados_final_br5 <- dados_final_br5 %>%
  filter(nm_cidade_inventor != "") %>%  # Filtrar observações onde "nm_cidade_inventor" não está vazio
  mutate(cidade_estado = paste0(nm_cidade_inventor, cd_uf_inventor))

dados_final_br5 <- dados_final_br5 %>%
  mutate(correspondencia = 0)

# Adicionar a coluna "nm_meso" na base "dados_final_br5" com base na correspondência de "cd_ibge_inventor" e "cd_muni"
dados_final_br6 <- dados_final_br5 %>%
  left_join(divisoes_brasileiras %>% dplyr::select(cd_muni, nm_meso), 
            by = c("cd_ibge_inventor" = "cd_muni"))

library(dplyr)

# Unir as bases "dados_final_br6" e "dados_final"
dados_consolidados <- bind_rows(dados_final, dados_final_br6)

write_csv(dados_consolidados, "dados_consolidados.csv")

# Filtrar apenas observações onde cd_pais_inventor == "BR"
dados_consolidados_br <- dados_consolidados %>%
  filter(cd_pais_inventor == "BR")

# Remover observações onde cd_pais_inventor == "BR" e nm_meso é NA
dados_consolidados2 <- dados_consolidados %>%
  filter(!(cd_pais_inventor == "BR" & is.na(nm_meso)))

dados_consolidados2_br <- dados_consolidados2 %>%
  filter(cd_pais_inventor == "BR")