# Bibliotecas   -------------------------------------------------
library(tidyverse)
library(foreign) #read.dbf
library(rvest) 
library(plyr) #revalue function

# Inicializacao -------------------------------------------------

# determina pasta com os dados
setwd("data")
# pega os nomes dos arquivos na pasta de trabalho com extensao .dbf
files <- list.files("IPTU", pattern = ".DBF")
filesNames <- str_replace(files, ".DBF", "")
files <- paste("IPTU/", files, sep = "")

# Rotina para corrigir nomes e empilhar ----

# le todos os arquivos dbfs
dbfs <- lapply(files, read.dbf, as.is=T)
names(dbfs) <- filesNames

# algumas tabelas nao estavam com a mesma qtde de colunas
# esta parte eh para corrigir isso
dbfs2 <- dbfs
# tabela IPTU_PART3_3A_300K com 42 colunas (4a coluna ID1)
dbfs2$IPTU_PART3_3A_300K <- dbfs2$IPTU_PART3_3A_300K[c(1:3,5:42)]
# tabela 15 com 44 colunas (da 4:7: ID1, ID11, LONGITUDE1, LATITUDE1)
dbfs2$IPTU_PART7_NE_355 <- dbfs2$IPTU_PART7_NE_355[c(1, 5:44)]

# removendo as colunas desnecessarias
dbfs3 <- lapply(dbfs2, function(x){
    y <- x[c(1:37,40)]
    names(y) <- c("id", "long", "lat", "num_contr", 
                  "ano_exerc", "num_nl", "data_cadastro", 
                  "tipo_contr_1", "nome_contr_1", 
                  "tipo_contr_2", "nome_contr_2", 
                  "num_condom", "cod_log_imov", "nome_lograd_orig", 
                  "num_imov", "complement", "bairro ", "referencia", 
                  "cep", "cep_4d", "qtde_frentes", "fracao_ideal", 
                  "area_terr", "area_constr", "area_ocupad", 
                  "val_m2_terr", "val_m2_const", 
                  "ano_const_corr", "qtde_pavim", 
                  "testada_calc", "tipo_uso", "tipo_padr", 
                  "tipo_terr", "fator_obso", "ano_inicio", 
                  "mes_inicio", "fase_contr", "nome_log_padr")
    
    return(y)
    })

# empilhando as tabelas
combined.df <- do.call(rbind, dbfs3)

# Rotina de categorização   -------------------------------------

# a partir da coluna tipo_padr, separa tipo_const de padr_const
df2 <- combined.df %>% mutate(tipo_const= factor(
    ifelse(is.na(str_sub(tipo_padr, 1, 
                         str_locate(tipo_padr, "-")[,1]-2)),
           "Terreno",
           str_sub(tipo_padr, 1, 
                   str_locate(tipo_padr, "-")[,1]-2)),
    labels = c("Barracão/Telheiro/Oficina",
               "Barracão/Telheiro/Oficina/Posto de serviço/Armazém/Depósito/Indústria",
               "Barracão/Telheiro/Oficina",
               "Barracão/Telheiro/Oficina/Posto de serviço/Armazém/Depósito/Indústria",
               "Comercial horizontal",
               "Comercial vertical",
               "Edifício de garagens", 
               "Edifício de garagens", 
               "Indústria","Indústria",
               "Oficina/Posto de serviço/Armazém/Depósito/Indústria",
               "Oficina/Posto de serviço/Armazém/Depósito/Indústria",
               "Residencial horizontal",
               "Residencial vertical",
               "Templo/Clube/Ginásio ou Estádio esportivo/Museu/Hipódromo/Cinema/Teatro/Aeroporto/Estações/etc.",
               "Templo/Clube/Ginásio ou Estádio esportivo/Museu/Hipódromo/Cinema/Teatro/Aeroporto/Estações/etc.",
               "Terreno")),
    padr_const = factor(str_sub(tipo_padr, str_locate(tipo_padr, "-")[,1]+8, 
                            str_locate(tipo_padr, "-")[,1]+9)),
    lat = lat/10^6,
    long = long/10^6,
    tipo_terr = factor(tipo_terr,
                       labels =  c("De duas ou mais frentes",
                                   "De duas ou mais frentes",
                                   "De duas ou mais frentes",
                                   "De esquina",
                                   "Lote de esquina",
                                   "Lote de esquina em ZER",
                                   "Lote de esquina em ZER",
                                   "Lote de fundos",
                                   "Lote de fundos",
                                   "Normal",
                                   "Terreno interno",
                                   "Terreno interno")),
    tipo_uso = factor(tipo_uso,
                      labels = c(
                          "Apartamento em condomínio", 
                          "Apartamento em condomínio", 
                          "Armazéns gerais e depósitos", 
                          "Armazéns gerais e depósitos", 
                          "Asilo, orfanato, creche, seminário ou convento", 
                          "Asilo, orfanato, creche, seminário ou convento", 
                          "Cinema, teatro, casa de diversão, clube ou congênere", 
                          "Cinema, teatro, casa de diversão, clube ou congênere", 
                          "Clube esportivo", 
                          "Cortiço", 
                          "Cortiço", 
                          "Escola", 
                          "Escritório/consultório em condomínio (unidade autônoma)",
                          "Escritório/consultório em condomínio (unidade autônoma)",
                          "Estação radioemissora, de televisão ou empresa jornalística",
                          "Estação radioemissora, de televisão ou empresa jornalística", 
                          "Flat de uso comercial (semelhante a hotel)", 
                          "Flat residencial em condomínio", 
                          "Flat residencial em condomínio", 
                          "Garagem (exclusive em prédio em condomínio)", 
                          "Garagem (exclusive em prédio em condomínio)", 
                          "Garagem (unidade autônoma) de prédio de garagens", 
                          "Garagem (unidade autônoma) em edifício em condomínio de escritórios, consultórios ou misto", 
                          "Garagem (unidade autônoma) em edifício em condomínio de uso exclusivamente residencial", 
                          "Garagem (unidade autônoma) de prédio de garagens", 
                          "Garagem (unidade autônoma) em edifício em condomínio de escritórios, consultórios ou misto", 
                          "Garagem (unidade autônoma) em edifício em condomínio de uso exclusivamente residencial", 
                          "Hospital, ambulatório, casa de saúde e assemelhados", 
                          "Hospital, ambulatório, casa de saúde e assemelhados", 
                          "Hotel, pensão ou hospedaria", "Hotel, pensão ou hospedaria", 
                          "Indústria", 
                          "Indústria", 
                          "Loja", 
                          "Loja e residência (predominância comercial)", 
                          "Loja e residência (predominância comercial)", 
                          "Loja em edifício em condomínio (unidade autônoma)", 
                          "Loja em edifício em condomínio (unidade autônoma)", 
                          "Oficina", 
                          "Outras edificações de uso coletivo, com utilização múltipla", 
                          "Outras edificações de uso comercial, com utilização múltipla", 
                          "Outras edificações de uso de serviço, com utilização múltipla", 
                          "Outras edificações de uso especial, com utilização múltipla", 
                          "Outras edificações de uso coletivo, com utilização múltipla", 
                          "Outras edificações de uso comercial, com utilização múltipla", 
                          "Outras edificações de uso de serviço, com utilização múltipla", 
                          "Outras edificações de uso especial, com utilização múltipla", 
                          "Posto de serviço", 
                          "Posto de serviço", 
                          "Prédio de apartamento, não em condomínio, de uso exclusivamente residencial", 
                          "Prédio de apartamento, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios), com ou sem loja (predominância residencial)", 
                          "Prédio de escritório ou consultório, não em condomínio, com ou sem lojas", 
                          "Prédio de escritório, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios) com ou sem loja (predominância comercial)", 
                          "Prédio de apartamento, não em condomínio, de uso exclusivamente residencial", 
                          "Prédio de apartamento, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios), com ou sem loja (predominância residencial)", 
                          "Prédio de apartamento, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios), com ou sem loja (predominância residencial)", 
                          "Prédio de escritório ou consultório, não em condomínio, com ou sem lojas", 
                          "Prédio de escritório, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios) com ou sem loja (predominância comercial)", 
                          "Prédio de escritório, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios) com ou sem loja (predominância comercial)", 
                          "Residência", 
                          "Residência", 
                          "Residência coletiva, exclusive cortiço (mais de uma residência no lote)", 
                          "Residência coletiva, exclusive cortiço (mais de uma residência no lote)", 
                          "Residência e outro uso (predominância residencial)", 
                          "Residência e outro uso (predominância residencial)", 
                          "Templo", 
                          "Terreno")),
    cod_const = factor(revalue(tipo_const, c(
        "Residencial horizontal" = "Tipo 1",
        "Residencial vertical" = "Tipo 2",
        "Comercial horizontal" = "Tipo 3",
        "Comercial vertical" = "Tipo 4",
        "Barracão/Telheiro/Oficina" = "Tipo 5",
        "Barracão/Telheiro/Oficina/Posto de serviço/Armazém/Depósito/Indústria" = "Tipo 5",
        "Indústria" = "Tipo 5",
        "Oficina/Posto de serviço/Armazém/Depósito/Indústria" = "Tipo 5",
        "Templo/Clube/Ginásio ou Estádio esportivo/Museu/Hipódromo/Cinema/Teatro/Aeroporto/Estações/etc." = "Tipo 6",
        "Edifício de garagens" = "Tipo 6")))
    ) %>% 
    filter(lat!=0)

# salva tabela completa em csv
write.csv(df2, file = "results/IPTU_2016_completo.csv")

# Rotina para separar   ----

tipos <- list("Tipo 1", "Tipo 2", "Tipo 3", "Tipo 4", 
           "Tipo 5", "Tipo 6", "Terreno")

df_tipos <- lapply(tipos, function(x){
    y <- df2 %>% filter(cod_const==x)
    write.csv(y, file = paste("results/IPTU_2016_", x, ".csv", sep = ""))
    return(y)})
names(df_tipos) <- tipos
