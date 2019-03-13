# Libraries   ----
library(tidyverse)

# Load data ----
load("rda/AlvarasExecucao.rda")

# Select only aprovacao ----

df.ap <- df.hab %>% filter(str_detect(descricao, "APROVACAO DE EDIFICACAO NOVA"))

test <- df.ap %>% 
    mutate(cod = ifelse(str_detect(descricao, "REVALIDACAO"), 16,
                        ifelse(str_detect(descricao, "RECONSIDERACAO"), 8,
                               ifelse(str_detect(descricao, "MODIFICATIVO"), 4,
                                      ifelse(str_detect(descricao, "APOSTILAMENTO"), 2, 1
                                          ))))) %>%
    group_by(sql_incra) %>%
    summarise(sum = sum(cod)) %>%
    group_by(sum) %>%
    summarise(ct = n())

# Select only execucao ----

df.ex <- df.hab %>% filter(str_detect(descricao, "DE EXECUCAO DE EDIFICACAO NOVA"))

test <- df.ex %>% 
    mutate(cod = ifelse(str_detect(descricao, "REVALIDACAO"), 16,
                        ifelse(str_detect(descricao, "RECONSIDERACAO"), 8,
                               ifelse(str_detect(descricao, "MODIFICATIVO"), 4,
                                      ifelse(str_detect(descricao, "APOSTILAMENTO"), 2, 1
                                      ))))) %>%
    group_by(sql_incra) %>%
    summarise(sum = sum(cod)) %>%
    group_by(sum) %>%
    summarise(ct = n())

# Select aprovacao e execucao ----

df.apex <- df.hab %>% filter(str_detect(descricao, " E EXECUCAO DE EDIFICACAO NOVA"))

test <- df.apex %>% 
    mutate(cod = ifelse(str_detect(descricao, "REVALIDACAO"), 16,
                        ifelse(str_detect(descricao, "RECONSIDERACAO"), 8,
                               ifelse(str_detect(descricao, "MODIFICATIVO"), 4,
                                      ifelse(str_detect(descricao, "APOSTILAMENTO"), 2, 1
                                      ))))) %>%
    group_by(sql_incra) %>%
    summarise(sum = sum(cod)) %>%
    group_by(sum) %>%
    summarise(ct = n())

# Select novas ----

# verify # of sql in execucao

df.an <- df.hab.ex %>% 
    group_by(sql_incra, long, lat, endereco_orig, num_imov) %>%
    summarise(ct = n())

max(df.an$ct)

, categ_uso, zona_uso, z_uso_anterior, hp