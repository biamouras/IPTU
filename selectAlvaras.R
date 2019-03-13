# Bibliotecas   ----
library(tidyverse)
library(rebus)
library(rlang) # eval expression

# Load data ----

load(file = "rda/Alvaras.rda")
load(file = "rda/IPTU.rda")

# Check long lat    ----

# verificando se tem sql duplicado
df.iptu2 <- df.iptu %>%
    dplyr::select(num_contr, long, lat, nome_lograd_orig, num_imov) 

# left join em alvaras


    left_join(df.iptu2, by = num_contr)

load("rda/Alvaras.rda")

# Select residencial, his e hmp ----

# padroes para selecionar a categoria de uso para
# HIS, se contiver HIS
pat.his <- "H"%R%ANY_CHAR%R%"I"%R%ANY_CHAR%R%"S"
# HMP, se contiver HMP
pat.hmp <- "H"%R%ANY_CHAR%R%"M"%R%ANY_CHAR%R%"P"
# Residencial, se contiver R
pat.res <- or(" R", "^R")

# seleciona residencial, his ou hmp
df.hab <- df.alv %>% 
    mutate(hp = factor(ifelse(str_detect(categ_uso, pat.his), 
                       "HIS", 
                       ifelse(str_detect(categ_uso, pat.hmp), 
                              "HMP",
                              ifelse(str_detect(categ_uso, pat.res), 
                                     "Residencial", 
                                     NA))))) %>%
    dplyr::select(id:lat, match_addr:unidade, descricao:sql_incra, 
                  categ_uso:z_uso_anterior, area_constr, area_terr:data_aprov, 
                  hp) %>%
    filter(!is.na(hp),
           ano %in% 2002:2013)

save(df.hab, file = "rda/AlvarasHab.rda")
# % de num_unid sem num_unid - RODAR UMA VEZ
df.hab %>% group_by(ano) %>% summarise(perc = mean(is.na(num_unid)))

# Select nova edificacao, execucao e com habite-se  ----

df.hab.nv <- df.hab %>% 
    filter(str_detect(descricao, "NOVA"))
write_csv(df.hab.nv, path="results/Alvaras_habNova.csv")

df.hab.ex <- df.hab.nv %>% 
    filter(str_detect(descricao, "EXECUCAO"))
save(df.hab.ex, file = "rda/AlvarasExecucao.rda")

df.conc <- df.hab %>%
    filter(str_detect(descricao, "CERTIFICADO DE CONCLUSAO"))
write_csv(df.hab.conc, path="results/Alvaras_habConc.csv")

# 
