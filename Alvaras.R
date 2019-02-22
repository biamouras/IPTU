# Bibliotecas   -------------------------------------------------
library(tidyverse)
library(foreign) #read.dbf
library(rvest) 
library(plyr) #revalue function
library(raster) #leitura do shp
library(rgdal) # leitura de shp
library(sf)

# Inicializacao     ----

# determina pasta com os dados
setwd("data")
# pega os nomes dos arquivos na pasta de trabalho com extensao .dbf
files <- list.files("Alvaras", pattern = ".shp$")
filesNames <- str_replace(files, ".shp", "")

# le todos os arquivos dbfs
sf <- lapply(files, function(x){
    y <- paste("Alvaras/", x, sep="")
    shp <- shapefile(y)
    st_as_sf(shp)})
names(sf) <- filesNames

# Rotina para corrigir nomes e empilhar ----

# algumas tabelas nao estavam com a mesma qtde de colunas
# esta parte eh para corrigir isso
sf2 <- sf
sf2$anual2002_2010_pdr_geocode <- 
    sf2$anual2002_2010_pdr_geocode[c(1:3, 5:16, 19:36, 39:40)]
sf2$anual2011_2018_pdr_geocode <-
    sf2$anual2011_2018_pdr_geocode[c(1:3, 5:16, 19:38)]

# criando colunas de lat e long
dfs <- lapply(sf2, function(x){
    y <- x %>% mutate(long = st_coordinates(x)[,1],
                  lat = st_coordinates(x)[,2])
    st_geometry(y) <- NULL
    return(y)
})

dfs2 <- lapply(dfs, function(x){
    names(x) <- c("id", "status", "score", "side", "match_addr",
                  "arc_Street", "ano", "mês", "unidade", "adm_reg", 
                  "alvara", "processo", "descricao", "tipo_constr", 
                  "sql_incra", "categ_uso", "zona_uso", "z_uso_anterior", 
                  "bairro", "area_constr", "proprieta", "area_terr", 
                  "endereco_orig", "num_imov", "data_aprov", "firma_dirigtec", 
                  "respons_tec", "autor_proj", "respons_firm", "num_bloc", 
                  "num_unid", "num_pav", "data_autua", "tipo_log_p", "nome_log", 
                  "long", "lat")
    return(x)
})

# empilhando as tabelas
combined.df <- do.call(rbind, dfs2)

# Corrigindo campos ----

# avaliando quais sao as colunas com "S/I"
y <- apply(combined.df, MARGIN = 2, 
      function(x){sum(str_detect(x,"S/I"), na.rm = T)})
names(y[y>0])

df <- data.frame(lapply(combined.df, function(x){
   ifelse(x=="S/I", NA, x) 
}))[,c(1,36,37,2:35)]

y <- apply(df, MARGIN = 2, 
           function(x){sum(str_detect(x,"S/I"), na.rm = T)})
names(y[y>0])

# Selecionando pontos   ----
# alguns pontos estao com long lat = "-1.79769313486232e+308"
df.off <- df %>% filter(long < -47 | status == "T") %>% 
    mutate(long=0, lat=0)

# ptos ok
df2 <- df %>% filter(long > -47 & status != "T") 

# salvando arquivos
write.csv(df.off, "../results/Alvaras_naoGeocod.csv")

write.csv(df2, "../results/Alvaras_geocod.csv")




