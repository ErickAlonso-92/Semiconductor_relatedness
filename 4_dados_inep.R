#Monta as bases de dados
universidade1997 = fread(file = "./dados/INSTITUICAO.csv", 
                 sep = "|",
                 dec = ".",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8")

# Supondo que a base se chama "dados"
contagem_por_cidade <- universidade1997 %>%
  count(CODIGO_IBGE, SG_UF) %>%
  arrange(desc(n)) 

#Salva a base num arquivo CSV
write_csv(universidade1997, "IES1997.csv")

#Salva a base num arquivo CSV
write_csv(contagem_por_cidade, "contagem_por_cidade1997.csv")


# Usando read.csv2 para arquivos com separador ";"
universidade2021 <- read.csv2(file = "./dados/MICRODADOS_CADASTRO_IES_2021.csv", 
                              stringsAsFactors = FALSE,
                              fileEncoding = "latin1")  # Tente também "UTF-8"


# Supondo que a base se chama "dados"
contagem_por_MESO_2021 <- universidade2021 %>%
  count(NO_MESORREGIAO_IES) %>%
  arrange(desc(n)) 
