# Bibliotecas   -------------------------------------------------
library(tidyverse)
library(foreign) #read.dbf
library(rvest) 
library(plyr) #revalue function

# Inicializacao -------------------------------------------------

# determina pasta com os dados
setwd("data")
# pega os nomes dos arquivos na pasta de trabalho com extensao .dbf
files <- list.files("IPTU", pattern = ".csv")
filesNames <- str_replace(files, ".csv", "")

# Rotina para corrigir nomes e empilhar ----
# le todos os arquivos dbfs 
df_list <- lapply(files, function(x){
    y <- paste("IPTU/", x, sep="")
    read.csv(y, as.is = T, sep = ";")})
names(df_list) <- filesNames

df_list2 <- df_list
df_list2$IPTU_NE_FINAL_com_part6 <- df_list$IPTU_NE_FINAL_com_part6[c(4,6:41, 44)]

# renomeando as colunas 
df_list2 <- lapply(df_list2, function(x){
    names(x) <- c("id", "long", "lat", "num_contr", 
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
    
    return(x)
})

# empilhando as tabelas
combined.df <- do.call(rbind, df_list2)

# Rotina de categorização   -------------------------------------

# a partir da coluna tipo_padr, separa tipo_const de padr_const
df2 <- combined.df %>% mutate(tipo_const= factor(
    ifelse(is.na(str_sub(tipo_padr, 1, 
                         str_locate(tipo_padr, "-")[,1]-2)),
           "Terreno",
           str_sub(tipo_padr, 1, 
                   str_locate(tipo_padr, "-")[,1]-2)),
    labels = c("Barracão/Telheiro/Oficina",
               "Barracão/Telheiro/Oficina",
               "Barracão/Telheiro/Oficina/Posto de serviço/Armazém/Depósito/Indústria",
               "Barracão/Telheiro/Oficina",
               "Comercial horizontal",
               "Comercial vertical",
               "Edifício de garagens", 
               "Edifício de garagens", 
               "Indústria",
               "Oficina/Posto de serviço/Armazém/Depósito/Indústria",
               "Residencial horizontal",
               "Residencial vertical",
               "Templo/Clube/Ginásio ou Estádio esportivo/Museu/Hipódromo/Cinema/Teatro/Aeroporto/Estações/etc.",
               "Terreno")),
    padr_const = factor(str_sub(tipo_padr, str_locate(tipo_padr, "-")[,1]+8, 
                                str_locate(tipo_padr, "-")[,1]+9)),
    lat = lat/10^6,
    long = long/10^6,
    tipo_padr = ifelse(is.na(str_sub(tipo_padr, 1, 
                                     str_locate(tipo_padr, "-")[,1]-2)),
                       "Terreno",
                       paste(tipo_const, padr_const, sep = " - padrão")),
    tipo_uso = factor(tipo_uso,
                      labels = c("Escritório/consultório em condomínio (unidade autônoma)", 
                                 "Garagem (unidade autônoma) em edifício em condomínio de escritórios, consultórios ou misto",
                                 "Garagem (unidade autônoma) em edifício em condomínio de uso exclusivamente residencial", 
                                 "Loja em edifício em condomínio (unidade autônoma)", "Escritório/consultório em condomínio (unidade autônoma)", 
                                 "Garagem (unidade autônoma) em edifício em condomínio de escritórios, consultórios ou misto", 
                                 "Garagem (unidade autônoma) em edifício em condomínio de uso exclusivamente residencial", 
                                 "Loja em edifício em condomínio (unidade autônoma)", 
                                 "Outras edificações de uso coletivo, com utilização múltipla", 
                                 "Outras edificações de uso comercial, com utilização múltipla", 
                                 "Outras edificações de uso de serviço, com utilização múltipla", 
                                 "Outras edificações de uso especial, com utilização múltipla", 
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
                                 "Estação radioemissora, de televisão ou empresa jornalística", 
                                 "Estação radioemissora, de televisão ou empresa jornalística", 
                                 "Estação radioemissora, de televisão ou empresa jornalística", 
                                 "Flat de uso comercial (semelhante a hotel)", 
                                 "Flat residencial em condomínio", 
                                 "Flat residencial em condomínio", 
                                 "Garagem (exclusive em prédio em condomínio)", 
                                 "Garagem (exclusive em prédio em condomínio)", 
                                 "Garagem (unidade autônoma) em edifício em condomínio de escritórios, consultórios ou misto", 
                                 "Garagem (unidade autônoma) em edifício em condomínio de uso exclusivamente residencial", 
                                 "Hospital, ambulatório, casa de saúde e assemelhados", 
                                 "Hospital, ambulatório, casa de saúde e assemelhados", 
                                 "Hotel, pensão ou hospedaria", 
                                 "Hotel, pensão ou hospedaria", 
                                 "Hotel, pensão ou hospedaria", 
                                 "Indústria", 
                                 "Indústria", 
                                 "Indústria", 
                                 "Loja", 
                                 "Loja e residência (predominância comercial)", 
                                 "Loja e residência (predominância comercial)", 
                                 "Loja e residência (predominância comercial)", 
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
                                 "Prédio de apartamento, não em condomínio, de uso exclusivamente residencial", 
                                 "Prédio de apartamento, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios), com ou sem loja (predominância residencial)", 
                                 "Prédio de apartamento, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios), com ou sem loja (predominância residencial)", 
                                 "Prédio de apartamento, não em condomínio, de uso exclusivamente residencial", 
                                 "Prédio de apartamento, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios), com ou sem loja (predominância residencial)", 
                                 "Prédio de escritório ou consultório, não em condomínio, com ou sem lojas",
                                 "Prédio de escritório, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios) com ou sem loja (predominância comercial)", 
                                 "Prédio de escritório ou consultório, não em condomínio, com ou sem lojas", 
                                 "Prédio de escritório ou consultório, não em condomínio, com ou sem lojas", 
                                 "Prédio de escritório, não em condomínio, de uso misto (apartamentos e escritórios e/ou consultórios) com ou sem loja (predominância comercial)", 
                                 "Residência", 
                                 "Residência coletiva, exclusive cortiço (mais de uma residência no lote)", 
                                 "Residência coletiva, exclusive cortiço (mais de uma residência no lote)", 
                                 "Residência e outro uso (predominância residencial)", 
                                 "Residência e outro uso (predominância residencial)", 
                                 "Residência e outro uso (predominância residencial)", 
                                 "Templo", 
                                 "Terreno")),
    tipo_terr = factor(tipo_terr, 
                       labels = c("De duas ou mais frentes",
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
        "Edifício de garagens" = "Tipo 6"
    ))))

# salva tabela completa em csv
write.csv(df2, file = "results/IPTU_2016_naoLocalizados.csv")

