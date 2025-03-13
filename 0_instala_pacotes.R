#Instala e chama os pacotes para serem utilizados

pacotes <- c("tidyverse","data.table","DataExplorer","report","tidyr","knitr","kableExtra", "dplyr",
             "PerformanceAnalytics", "ggraph", "correlation", "corrplot", "corrr" , "stringr", "geobr",
             "reshape2", "igraph", "purrr", "geobr", "EconGeo", "zoo", "sf", "ggplot2", "tmap",
             "lmtest", "sandwich", "pglm", "margins", "car", "mfx", "fixest", "slider", "stringdist", "stargazer",
             "modelsummary", "sjlabelled", "insight", "marginaleffects", "margins")

if( sum( as.numeric( !pacotes %in% installed.packages() ) ) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply( pacotes, require, character = T ) 
} else {
  sapply( pacotes, require, character = T ) 
}

rm( pacotes )