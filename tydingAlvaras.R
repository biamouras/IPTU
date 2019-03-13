# Libraries ----
library(tidyverse)
library(rebus)

# Load data ----
load("rda/Alvaras.rda")
load("rda/IPTU_2016_completo.rda")

# Checking data ----
df.alv2 <- df.alv %>%
    mutate(num_contr = str_replace_all(str_replace_all(sql_incra, "^00\\.", ""), 
                                       "\\.", ""),
           len = str_length(num_contr)) %>%
    dplyr::select(alvara, ano, sql_incra, num_contr, len) %>%
    filter(len<12)

# Load original data ----

files <- list.files("data/Alvaras/org", pattern = ".csv")
fileName <- str_replace_all(files, ".csv", "")

alv.org <- lapply(files, function(x) {
    y <- paste("data/Alvaras/org/", x, sep="")
    z <- read_delim(y, delim = ";", 
                    locale = locale(encoding = "ISO-8859-1"))
    if (names(z)[3] == "X3"){
        names(z) <- z[1,]
        z <- z[2:nrow(z),]
        return(z)
    } else{
        return(z)
        }
    })

names(alv.org) <- 2002:2018
save(alv.org, file = "rda/AlvOriginal.rda")

# Tidying original  ----

# getting the same col names
alv.org2 <- lapply(alv.org, function(z){
    # rename NA cols
    names(z)[is.na(names(z))] <- c("X1", "X2")
    names(z) <- toupper(names(z)) %>%
        str_replace(or("MÊS$", "MÊS ", "MES"), "mes") %>%
        str_replace("UNIDADE$", "unidade") %>%
        str_replace(or("ADMINISTRACAO REGIONAL", "ADMINISTRAÇÃO REGIONAL",
                       "SUBPREFEITURA", "PREFEITURA REGIONAL"), "adm_reg") %>%
        str_replace(or("ALVARA", "ALVARÁ"), "alvara") %>%
        str_replace("PROCESSO", "processo") %>%
        str_replace(or("DESCRICAO", "DESCRIÇÃO"), "descricao") %>%
        str_replace("TIPO DA CONSTRUCAO", "tipo_constr") %>%
        str_replace(or("SQL/INCRA", "SQL_INCRA"), "sql_incra") %>%
        str_replace("CATEGORIA DE USO", "categ_uso") %>%
        str_replace(or("ZONA DE USO$", "ZONA DE USO ATUAL"), "zona_uso") %>%
        str_replace("ZONA DE USO ANTERIOR", "z_uso_anterior") %>%
        str_replace("BAIRRO", "bairro") %>%
        str_replace(or("AREA DA CONSTRUCAO",
                       "ÁREA DA CONSTRUÇÃO"), "area_constr") %>%
        str_replace(or("PROPRIETARIO", "PROPRIETÁRIO"), "proprieta") %>%
        str_replace(or("AREA DO TERRENO", "ÁREA DO TERRENO"), "area_terr") %>%
        str_replace(or("ENDERECO", "ENDEREÇO"), "endereco_orig") %>%
        str_replace(or("APROVACAO", "APROVAÇÃO"), "data_aprov") %>%
        str_replace(or("FIRMA/DIRIGENTE TECNICO", 
                       "DIRIGENTE TÉCNICO"), "firma_dirigtec") %>%
        str_replace(or("RESPONSAVEL TECNICO",
                       "RESPONSÁVEL TÉCNICO"), "respons_tec") %>%
        str_replace(or("AUTOR PROJETO", 
                       "AUTOR DO PROJETO"), "autor_proj") %>%
        str_replace(or("RESPONSAVEL DA FIRMA",
                       "RESPONSÁVEL PELA EMPRESA"), "respons_firm") %>%
        str_replace(or("NUMERO DE BLOCOS", 
                       "\\(B\\)LOCOS/\\(P\\)AVIMENTOS/\\(U\\)NIDADES",
                       "\\(B\\)LOCOS/$",
                       "\\(B\\)LOCOS/\\(P\\)AVIMENTOS/$"), "num_bloc") %>%
        str_replace("NUMERO DE UNIDADES", "num_unid") %>%
        str_replace("NUMERO DE PAVIMENTOS", "num_pav") %>%
        str_replace(or("DATA AUTUAÇÃO", "DTAUTUACAO"), "data_autua") 
    return(z)
})

# adding missing columns
alv.org3 <- lapply(seq_along(alv.org2), function(i){
    if(as.numeric(names(alv.org2)[[i]]) %in% c(2002, 2004:2010)){
        y <- alv.org2[[i]] %>%
            mutate(processo = NA,
                   z_uso_anterior = NA,
                   data_autua = NA)
    } else if (as.numeric(names(alv.org2)[[i]]) %in% c(2012:2015)) {
        y <- alv.org2[[i]] %>%
            mutate(tipo_constr = NA,
                   num_unid = NA,
                   num_pav = NA,
                   data_autua = NA)
    } else if (as.numeric(names(alv.org2)[[i]]) %in% c(2016:2018)) {
        y <- alv.org2[[i]] %>%
            mutate(tipo_constr = NA,
                   num_unid = NA,
                   num_pav = NA)
    } else if (as.numeric(names(alv.org2)[[i]]) == 2003) {
        y <- alv.org2[[i]] %>%
            mutate(z_uso_anterior = NA,
                   num_bloc = NA,
                   num_unid = NA,
                   num_pav = NA)
    } else if (as.numeric(names(alv.org2)[[i]]) == 2011) {
        y <- alv.org2[[i]] %>%
            mutate(processo = NA,
                   tipo_constr = NA,
                   num_unid = NA,
                   num_pav = NA,
                   data_autua = NA)
    }
    # selecting columns
    y %>% mutate(ano = as.numeric(names(alv.org2)[[i]])) %>%
        dplyr::select(ano, mes, unidade, adm_reg, alvara, processo, descricao, 
                  tipo_constr, sql_incra, categ_uso, zona_uso, z_uso_anterior,
                  bairro, area_constr, proprieta, area_terr, endereco_orig, 
                  data_aprov, firma_dirigtec, respons_tec, autor_proj, 
                  respons_firm, num_bloc, num_unid, num_pav, data_autua)
})

# empilhando



# trash ----

alv.org3 <- lapply(alv.org2, function(x){
    
})

# adding ano column = list name
alv.org3 <- lapply(seq_along(alv.org2), function(i){
    alv.org2[[i]] %>% mutate(ANO = names(alv.org2)[[i]])
})

