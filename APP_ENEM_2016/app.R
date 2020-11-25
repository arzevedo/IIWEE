library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(leaflet.minicharts)
# nÃ£o estÃ¡ disponivel para essa versÃ£o install.packages("shinytemplates")
library(shinythemes)
#library(rsconnect)
# theme = shinytheme("darkly)
# bootswatch.com

#########################
###Preparando os dados###
#########################
#municipios####
munici <- read.csv("Enem_2016/municipios.csv")
munici <- munici[,-1]
#selecione sua cidade####
sua_cida <- munici %>% 
  arrange(desc(MEDIA_FINAL)) %>% 
  mutate(MEDIA_FINAL=MEDIA_FINAL/5) %>% 
  select(MUNICIPIO,MEDIA_FINAL) %>% 
  add_column(Rank = paste0(1:5570,"º"),.before = 1) %>% 
  rename(Desempenho = MEDIA_FINAL)
#tabela de rnking####
muni_top_mdfin <- munici %>% 
  top_n(10, wt = MEDIA_FINAL) %>% 
  arrange(desc(MEDIA_FINAL)) %>% 
  mutate(MEDIA_FINAL=MEDIA_FINAL/5) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho = MEDIA_FINAL)
muni_top_mdCN <- munici %>%
  top_n(10,wt=MEDIA_CN) %>%
  arrange(desc(MEDIA_CN)) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_CN)
muni_top_mdCH <- munici %>% 
  top_n(10,wt = MEDIA_CH) %>% 
  arrange(desc(MEDIA_CH)) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_CH)
muni_top_mdLC <- munici %>% 
  top_n(10,wt=MEDIA_LC) %>% 
  arrange(desc(MEDIA_LC)) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_LC)
muni_top_mdMT <- munici %>% 
  top_n(10,wt=MEDIA_MT) %>% 
  arrange(desc(MEDIA_MT)) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_MT)
muni_top_mdRE <- munici %>% 
  top_n(10,wt=MEDIA_REDACAO) %>% 
  arrange(desc(MEDIA_REDACAO)) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_REDACAO)
muni_low_mdfin <- munici %>% 
  top_n(-10, wt = MEDIA_FINAL) %>% 
  arrange(MEDIA_FINAL) %>% 
  mutate(MEDIA_FINAL=MEDIA_FINAL/5) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_FINAL)
muni_low_mdCN <- munici %>% 
  top_n(-10, wt = MEDIA_CN) %>% 
  arrange(MEDIA_CN) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_CN)
muni_low_mdCH <- munici %>% 
  top_n(-10, wt = MEDIA_CH) %>% 
  arrange(MEDIA_CH) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_CH)
muni_low_mdLC <- munici %>%
  top_n(-10, wt = MEDIA_LC) %>% 
  arrange(MEDIA_LC) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_LC)
muni_low_mdMT <- munici %>% 
  top_n(-10, wt = MEDIA_MT) %>% 
  arrange(MEDIA_MT) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_MT)
muni_low_mdRE <- munici %>% 
  top_n(-10, wt = MEDIA_REDACAO) %>% 
  arrange(MEDIA_REDACAO) %>% 
  add_column(Rank = paste0(1:10,"º"),.before = 1) %>% 
  rename(Desempenho=MEDIA_REDACAO)
#####
# Banco Geral
reg_norte <- c(11:17)
reg_nordeste <- c(21:29)
reg_sudeste <- c(31:35)
reg_sul <- c(41:43)
reg_centro_oeste <- c(50:53)

categ <- c('Sexo', 'Cor autodeclarada', 'Tipo de escola', 'Tipo de escola frequentada no EM', 'Turno do EM')

enem_2016_30mil<-read.csv("Enem_2016/amostra_30mil.csv")
enem_2016_30mil$regiao =
  ifelse(enem_2016_30mil$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_30mil$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_30mil$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_30mil$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_30mil$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "Não identificado")))))
enem_2016_30mil$regiao =
  factor(enem_2016_30mil$regiao, levels = c('S','SE','CO','NE','N'))

Regiao<-c('S','SE','CO','NE','N','BR_T')
Lat<-c(-28,-20,-16,-8,-3.5,-23)
Long<-c(-51,-45,-54,-42,-60,-31)
dado1<-c(table(enem_2016_30mil$TP_SEXO,enem_2016_30mil$regiao)[1,], sum(table(enem_2016_30mil$TP_SEXO,enem_2016_30mil$regiao)[1,]))
dado2<-c(table(enem_2016_30mil$TP_SEXO,enem_2016_30mil$regiao)[2,], sum(table(enem_2016_30mil$TP_SEXO,enem_2016_30mil$regiao)[2,]))
# Sexo
dados_g_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_g_s)[4] <- "Mulheres"
colnames(dados_g_s)[5] <- "Homens"
row.names(dados_g_s)[6] <- "Brasil total"
dados_g_s[,4:5]<-round(100*(dados_g_s[,4:5]/rowSums(dados_g_s[,4:5])), digits = 1)

br.shp=readOGR(dsn="grandes_regioes_shp.shp")

colors<-brewer.pal(7,"Spectral")

# Idade
enem_2016_30mil$fx_idade =
  ifelse(enem_2016_30mil$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_30mil$NU_IDADE>=16 & enem_2016_30mil$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_30mil$NU_IDADE>=20 & enem_2016_30mil$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_30mil$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_30mil$fx_idade =
  factor(enem_2016_30mil$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                              "Mais de 25 anos"))
dado1<-c(table(enem_2016_30mil$fx_idade,enem_2016_30mil$regiao)[1,], sum(table(enem_2016_30mil$fx_idade,enem_2016_30mil$regiao)[1,]))
dado2<-c(table(enem_2016_30mil$fx_idade,enem_2016_30mil$regiao)[2,], sum(table(enem_2016_30mil$fx_idade,enem_2016_30mil$regiao)[2,]))
dado3<-c(table(enem_2016_30mil$fx_idade,enem_2016_30mil$regiao)[3,], sum(table(enem_2016_30mil$fx_idade,enem_2016_30mil$regiao)[3,]))
dado4<-c(table(enem_2016_30mil$fx_idade,enem_2016_30mil$regiao)[4,], sum(table(enem_2016_30mil$fx_idade,enem_2016_30mil$regiao)[4,]))

dados_g_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_g_i)[4] <- "Menos de 16 anos"
colnames(dados_g_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_g_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_g_i)[7] <- "Mais de 25 anos"
row.names(dados_g_i)[6] <- "Brasil total"
dados_g_i[,4:7]<-round(100*(dados_g_i[,4:7]/rowSums(dados_g_i[,4:7])), digits = 1)

# RaÃ§a
enem_2016_30mil$TP_COR_RACA =
  ifelse(enem_2016_30mil$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_30mil$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_30mil$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_30mil$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_30mil$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_30mil$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_30mil$TP_COR_RACA =
  factor(enem_2016_30mil$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                 'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[1,], sum(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[1,]))
dado2<-c(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[2,], sum(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[2,]))
dado3<-c(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[3,], sum(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[3,]))
dado4<-c(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[4,], sum(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[4,]))
dado5<-c(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[5,], sum(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[5,]))
dado6<-c(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[6,], sum(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[6,]))
dado7<-c(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[7,], sum(table(enem_2016_30mil$TP_COR_RACA,enem_2016_30mil$regiao)[7,]))

dados_g_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_g_r)[4] <- "Não declarado"
colnames(dados_g_r)[5] <- "Branca"
colnames(dados_g_r)[6] <- "Preta"
colnames(dados_g_r)[7] <- "Parda"
colnames(dados_g_r)[8] <- "Amarela"
colnames(dados_g_r)[9] <- "Indígena"
colnames(dados_g_r)[10] <- "Não dispõe da informação"
row.names(dados_g_r)[6] <- "Brasil total"
dados_g_r[,4:10]<-round(100*(dados_g_r[,4:10]/rowSums(dados_g_r[,4:10])), digits = 1)

#TP Escola
# Complicado usar essa variavel(tipo de escola) por enquanto, mais de 26 mil nÃ£o responderam.
enem_2016_30mil$TP_ESCOLA =
  ifelse(enem_2016_30mil$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_30mil$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_30mil$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_30mil$TP_ESCOLA =
  factor(enem_2016_30mil$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                               "Exterior"))
dado1<-c(table(enem_2016_30mil$TP_ESCOLA,enem_2016_30mil$regiao)[1,], sum(table(enem_2016_30mil$TP_ESCOLA,enem_2016_30mil$regiao)[1,]))
dado2<-c(table(enem_2016_30mil$TP_ESCOLA,enem_2016_30mil$regiao)[2,], sum(table(enem_2016_30mil$TP_ESCOLA,enem_2016_30mil$regiao)[2,]))
dado3<-c(table(enem_2016_30mil$TP_ESCOLA,enem_2016_30mil$regiao)[3,], sum(table(enem_2016_30mil$TP_ESCOLA,enem_2016_30mil$regiao)[3,]))
dado4<-c(table(enem_2016_30mil$TP_ESCOLA,enem_2016_30mil$regiao)[4,], sum(table(enem_2016_30mil$TP_ESCOLA,enem_2016_30mil$regiao)[4,]))

dados_g_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_g_tpe)[4] <- "Não Respondeu"
colnames(dados_g_tpe)[5] <- "Pública"
colnames(dados_g_tpe)[6] <- "Privada"
colnames(dados_g_tpe)[7] <- "Exterior"
row.names(dados_g_tpe)[6] <- "Brasil total"
dados_g_tpe[,4:7]<-round(100*(dados_g_tpe[,4:7]/rowSums(dados_g_tpe[,4:7])), digits = 1)

#Tipo de escola
enem_2016_30mil$Q047 =
  ifelse(enem_2016_30mil$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_30mil$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_30mil$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_30mil$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_30mil$Q047 =
  factor(enem_2016_30mil$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                          "Esc. Pública e Esc. Privada c/ bolsa integral",
                                          "Somente Esc. Privada s/ bolsa integral",
                                          "Somente Esc. Privada c/ bolsa integral")
  )
dado1<-c(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[1,], sum(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[1,]))
dado2<-c(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[2,], sum(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[2,]))
dado3<-c(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[3,], sum(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[3,]))
dado4<-c(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[4,], sum(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[4,]))
dado5<-c(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[5,], sum(table(enem_2016_30mil$Q047,enem_2016_30mil$regiao)[5,]))

dados_g_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_g_q047)[4] <- "Somente Esc. Pública"
colnames(dados_g_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_g_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_g_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_g_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_g_q047)[6] <- "Brasil total"
dados_g_q047[,4:8]<-round(100*(dados_g_q047[,4:8]/rowSums(dados_g_q047[,4:8])), digits = 1)

#TURNO do EM
enem_2016_30mil$Q049 =
  ifelse(enem_2016_30mil$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_30mil$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_30mil$Q049 =
  factor(enem_2016_30mil$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))
dado1<-c(table(enem_2016_30mil$Q049,enem_2016_30mil$regiao)[1,], sum(table(enem_2016_30mil$Q049,enem_2016_30mil$regiao)[1,]))
dado2<-c(table(enem_2016_30mil$Q049,enem_2016_30mil$regiao)[2,], sum(table(enem_2016_30mil$Q049,enem_2016_30mil$regiao)[2,]))
dado3<-c(table(enem_2016_30mil$Q049,enem_2016_30mil$regiao)[3,], sum(table(enem_2016_30mil$Q049,enem_2016_30mil$regiao)[3,]))

dados_g_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_g_tur)[4] <- "Somente no diurno"
colnames(dados_g_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_g_tur)[6] <- "Somente no noturno"
row.names(dados_g_tur)[6] <- "Brasil total"
dados_g_tur[,4:6]<-round(100*(dados_g_tur[,4:6]/rowSums(dados_g_tur[,4:6])), digits = 1)

# RENDA
dois_SM <- c("C", "D")
tres_SM <- c("E", "F")
cinco_SM <- c("G", "H")
acima_cinco_SM <- c("I","J","K","L","M","N", "O", "P", "Q")
# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_30mil$renda_SM =
  ifelse(enem_2016_30mil$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_30mil$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_30mil$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_30mil$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_30mil$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_30mil$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_30mil$renda_SM =
  factor(enem_2016_30mil$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                              "De um a dois Salários-Mínimos",
                                              "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                              "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[2,], sum(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[2,]))
dado2<-c(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[3,], sum(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[3,]))
dado3<-c(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[4,], sum(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[4,]))
dado4<-c(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[5,], sum(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[5,]))
dado5<-c(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[6,], sum(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[6,]))
dado6<-c(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[7,], sum(table(enem_2016_30mil$renda_SM,enem_2016_30mil$regiao)[7,]))

dados_g_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_g_sm)[4] <- "Nenhuma renda"
colnames(dados_g_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_g_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_g_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_g_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_g_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_g_sm)[6] <- "Brasil total"
dados_g_sm[,4:9]<-round(100*(dados_g_sm[,4:9]/rowSums(dados_g_sm[,4:9])), digits = 1)

#####
# Melhores nota final
enem_2016_mil_melhores<-read.csv("Enem_2016/melhores_1000.csv")
enem_2016_mil_melhores$regiao =
  ifelse(enem_2016_mil_melhores$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_mil_melhores$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_mil_melhores$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_mil_melhores$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_mil_melhores$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_mil_melhores$regiao =
  factor(enem_2016_mil_melhores$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_mil_melhores$TP_SEXO,enem_2016_mil_melhores$regiao)[1,], sum(table(enem_2016_mil_melhores$TP_SEXO,enem_2016_mil_melhores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_melhores$TP_SEXO,enem_2016_mil_melhores$regiao)[2,], sum(table(enem_2016_mil_melhores$TP_SEXO,enem_2016_mil_melhores$regiao)[2,]))

dados_m_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_m_s)[4] <- "Mulheres"
colnames(dados_m_s)[5] <- "Homens"
row.names(dados_m_s)[6] <- "Brasil total"
dados_m_s[,4:5]<-round(100*(dados_m_s[,4:5]/rowSums(dados_m_s[,4:5])), digits = 1)
#IDade

enem_2016_mil_melhores$fx_idade =
  ifelse(enem_2016_mil_melhores$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_mil_melhores$NU_IDADE>=16 & enem_2016_mil_melhores$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_mil_melhores$NU_IDADE>=20 & enem_2016_mil_melhores$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_mil_melhores$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_mil_melhores$fx_idade =
  factor(enem_2016_mil_melhores$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                     "Mais de 25 anos"))
dado1<-c(table(enem_2016_mil_melhores$fx_idade,enem_2016_mil_melhores$regiao)[1,], sum(table(enem_2016_mil_melhores$fx_idade,enem_2016_mil_melhores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_melhores$fx_idade,enem_2016_mil_melhores$regiao)[2,], sum(table(enem_2016_mil_melhores$fx_idade,enem_2016_mil_melhores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_melhores$fx_idade,enem_2016_mil_melhores$regiao)[3,], sum(table(enem_2016_mil_melhores$fx_idade,enem_2016_mil_melhores$regiao)[3,]))
dado4<-c(table(enem_2016_mil_melhores$fx_idade,enem_2016_mil_melhores$regiao)[4,], sum(table(enem_2016_mil_melhores$fx_idade,enem_2016_mil_melhores$regiao)[4,]))

dados_m_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_m_i)[4] <- "Menos de 16 anos"
colnames(dados_m_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_m_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_m_i)[7] <- "Mais de 25 anos"
row.names(dados_m_i)[6] <- "Brasil total"
dados_m_i[,4:7]<-round(100*(dados_m_i[,4:7]/rowSums(dados_m_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_mil_melhores$Q047 =
  ifelse(enem_2016_mil_melhores$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_mil_melhores$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_mil_melhores$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_mil_melhores$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_mil_melhores$Q047 =
  factor(enem_2016_mil_melhores$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                                 "Esc. Pública e Esc. Privada c/ bolsa integral",
                                                 "Somente Esc. Privada s/ bolsa integral",
                                                 "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[1,], sum(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[2,], sum(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[3,], sum(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[3,]))
dado4<-c(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[4,], sum(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[4,]))
dado5<-c(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[5,], sum(table(enem_2016_mil_melhores$Q047,enem_2016_mil_melhores$regiao)[5,]))

dados_m_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_m_q047)[4] <- "Somente Esc. Pública"
colnames(dados_m_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_m_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_m_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_m_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_m_q047)[6] <- "Brasil total"
dados_m_q047[,4:8]<-round(100*(dados_m_q047[,4:8]/rowSums(dados_m_q047[,4:8])), digits = 1)

enem_2016_mil_melhores$TP_COR_RACA =
  ifelse(enem_2016_mil_melhores$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_mil_melhores$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_mil_melhores$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_mil_melhores$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_mil_melhores$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_mil_melhores$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_mil_melhores$TP_COR_RACA =
  factor(enem_2016_mil_melhores$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                        'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[1,], sum(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[2,], sum(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[3,], sum(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[3,]))
dado4<-c(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[4,], sum(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[4,]))
dado5<-c(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[5,], sum(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[5,]))
dado6<-c(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[6,], sum(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[6,]))
dado7<-c(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[7,], sum(table(enem_2016_mil_melhores$TP_COR_RACA,enem_2016_mil_melhores$regiao)[7,]))

dados_m_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_m_r)[4] <- "Não declarado"
colnames(dados_m_r)[5] <- "Branca"
colnames(dados_m_r)[6] <- "Preta"
colnames(dados_m_r)[7] <- "Parda"
colnames(dados_m_r)[8] <- "Amarela"
colnames(dados_m_r)[9] <- "Indígena"
colnames(dados_m_r)[10] <- "Não dispõe da informação"
row.names(dados_m_r)[6] <- "Brasil total"
dados_m_r[,4:10]<-round(100*(dados_m_r[,4:10]/rowSums(dados_m_r[,4:10])), digits = 1)

enem_2016_mil_melhores$TP_ESCOLA =
  ifelse(enem_2016_mil_melhores$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_mil_melhores$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_mil_melhores$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_mil_melhores$TP_ESCOLA =
  factor(enem_2016_mil_melhores$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                      "Exterior"))
dado1<-c(table(enem_2016_mil_melhores$TP_ESCOLA,enem_2016_mil_melhores$regiao)[1,], sum(table(enem_2016_mil_melhores$TP_ESCOLA,enem_2016_mil_melhores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_melhores$TP_ESCOLA,enem_2016_mil_melhores$regiao)[2,], sum(table(enem_2016_mil_melhores$TP_ESCOLA,enem_2016_mil_melhores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_melhores$TP_ESCOLA,enem_2016_mil_melhores$regiao)[3,], sum(table(enem_2016_mil_melhores$TP_ESCOLA,enem_2016_mil_melhores$regiao)[3,]))
dado4<-c(table(enem_2016_mil_melhores$TP_ESCOLA,enem_2016_mil_melhores$regiao)[4,], sum(table(enem_2016_mil_melhores$TP_ESCOLA,enem_2016_mil_melhores$regiao)[4,]))

dados_m_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_m_tpe)[4] <- "Não Respondeu"
colnames(dados_m_tpe)[5] <- "Pública"
colnames(dados_m_tpe)[6] <- "Privada"
colnames(dados_m_tpe)[7] <- "Exterior"
row.names(dados_m_tpe)[6] <- "Brasil total"
dados_m_tpe[,4:7]<-round(100*(dados_m_tpe[,4:7]/rowSums(dados_m_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_mil_melhores$renda_SM =
  ifelse(enem_2016_mil_melhores$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_mil_melhores$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_mil_melhores$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_mil_melhores$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_mil_melhores$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_mil_melhores$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_mil_melhores$renda_SM = factor(enem_2016_mil_melhores$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                                     "De um a dois Salários-Mínimos",
                                                                                     "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                                     "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[2,], sum(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[2,]))
dado2<-c(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[3,], sum(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[3,]))
dado3<-c(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[4,], sum(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[4,]))
dado4<-c(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[5,], sum(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[5,]))
dado5<-c(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[6,], sum(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[6,]))
dado6<-c(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[7,], sum(table(enem_2016_mil_melhores$renda_SM,enem_2016_mil_melhores$regiao)[7,]))

dados_m_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_m_sm)[4] <- "Nenhuma renda"
colnames(dados_m_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_m_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_m_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_m_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_m_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_m_sm)[6] <- "Brasil total"
dados_m_sm[,4:9]<-round(100*(dados_m_sm[,4:9]/rowSums(dados_m_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_mil_melhores$Q049 =
  ifelse(enem_2016_mil_melhores$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_mil_melhores$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_mil_melhores$Q049 =
  factor(enem_2016_mil_melhores$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_mil_melhores$Q049, enem_2016_mil_melhores$regiao)[1,], sum(table(enem_2016_mil_melhores$Q049,enem_2016_mil_melhores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_melhores$Q049, enem_2016_mil_melhores$regiao)[2,], sum(table(enem_2016_mil_melhores$Q049,enem_2016_mil_melhores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_melhores$Q049, enem_2016_mil_melhores$regiao)[3,], sum(table(enem_2016_mil_melhores$Q049,enem_2016_mil_melhores$regiao)[3,]))

dados_m_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_m_tur)[4] <- "Somente no diurno"
colnames(dados_m_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_m_tur)[6] <- "Somente no noturno"
row.names(dados_m_tur)[6] <- "Brasil total"
dados_m_tur[,4:6]<-round(100*(dados_m_tur[,4:6]/rowSums(dados_m_tur[,4:6])), digits = 1)
#######
# Melhores em CH
enem_2016_melhores_CH<-read.csv("Enem_2016/melhores_1000_CH.csv")

enem_2016_melhores_CH$regiao =
  ifelse(enem_2016_melhores_CH$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_melhores_CH$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_melhores_CH$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_melhores_CH$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_melhores_CH$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_melhores_CH$regiao =
  factor(enem_2016_melhores_CH$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_melhores_CH$TP_SEXO,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$TP_SEXO,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CH$TP_SEXO,enem_2016_melhores_CH$regiao)[2,], sum(table(enem_2016_melhores_CH$TP_SEXO,enem_2016_melhores_CH$regiao)[2,]))

dados_mCH_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_mCH_s)[4] <- "Mulheres"
colnames(dados_mCH_s)[5] <- "Homens"
row.names(dados_mCH_s)[6] <- "Brasil total"
dados_mCH_s[,4:5]<-round(100*(dados_mCH_s[,4:5]/rowSums(dados_mCH_s[,4:5])), digits = 1)
#IDade

enem_2016_melhores_CH$fx_idade =
  ifelse(enem_2016_melhores_CH$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_melhores_CH$NU_IDADE>=16 & enem_2016_melhores_CH$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_melhores_CH$NU_IDADE>=20 & enem_2016_melhores_CH$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_melhores_CH$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_melhores_CH$fx_idade =
  factor(enem_2016_melhores_CH$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                    "Mais de 25 anos"))
dado1<-c(table(enem_2016_melhores_CH$fx_idade,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$fx_idade,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CH$fx_idade,enem_2016_melhores_CH$regiao)[2,], sum(table(enem_2016_melhores_CH$fx_idade,enem_2016_melhores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CH$fx_idade,enem_2016_melhores_CH$regiao)[3,], sum(table(enem_2016_melhores_CH$fx_idade,enem_2016_melhores_CH$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_CH$fx_idade,enem_2016_melhores_CH$regiao)[4,], sum(table(enem_2016_melhores_CH$fx_idade,enem_2016_melhores_CH$regiao)[4,]))

dados_mCH_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mCH_i)[4] <- "Menos de 16 anos"
colnames(dados_mCH_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_mCH_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_mCH_i)[7] <- "Mais de 25 anos"
row.names(dados_mCH_i)[6] <- "Brasil total"
dados_mCH_i[,4:7]<-round(100*(dados_mCH_i[,4:7]/rowSums(dados_mCH_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_melhores_CH$Q047 =
  ifelse(enem_2016_melhores_CH$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_melhores_CH$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_melhores_CH$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_melhores_CH$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_melhores_CH$Q047 =
  factor(enem_2016_melhores_CH$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                                "Esc. Pública e Esc. Privada c/ bolsa integral",
                                                "Somente Esc. Privada s/ bolsa integral",
                                                "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[2,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[3,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[4,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[5,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[5,]))

dados_mCH_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_mCH_q047)[4] <- "Somente Esc. Pública"
colnames(dados_mCH_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_mCH_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_mCH_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_mCH_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_mCH_q047)[6] <- "Brasil total"
dados_mCH_q047[,4:8]<-round(100*(dados_mCH_q047[,4:8]/rowSums(dados_mCH_q047[,4:8])), digits = 1)

enem_2016_melhores_CH$TP_COR_RACA =
  ifelse(enem_2016_melhores_CH$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_melhores_CH$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_melhores_CH$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_melhores_CH$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_melhores_CH$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_melhores_CH$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_melhores_CH$TP_COR_RACA =
  factor(enem_2016_melhores_CH$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                       'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[2,], sum(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[3,], sum(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[4,], sum(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[5,], sum(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[5,]))
dado6<-c(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[6,], sum(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[6,]))
dado7<-c(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[7,], sum(table(enem_2016_melhores_CH$TP_COR_RACA,enem_2016_melhores_CH$regiao)[7,]))

dados_mCH_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_mCH_r)[4] <- "Não declarado"
colnames(dados_mCH_r)[5] <- "Branca"
colnames(dados_mCH_r)[6] <- "Preta"
colnames(dados_mCH_r)[7] <- "Parda"
colnames(dados_mCH_r)[8] <- "Amarela"
colnames(dados_mCH_r)[9] <- "Indígena"
colnames(dados_mCH_r)[10] <- "Não dispõe da informação"
row.names(dados_mCH_r)[6] <- "Brasil total"
dados_mCH_r[,4:10]<-round(100*(dados_mCH_r[,4:10]/rowSums(dados_mCH_r[,4:10])), digits = 1)

enem_2016_melhores_CH$TP_ESCOLA =
  ifelse(enem_2016_melhores_CH$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_melhores_CH$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_melhores_CH$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_melhores_CH$TP_ESCOLA =
  factor(enem_2016_melhores_CH$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                     "Exterior"))
dado1<-c(table(enem_2016_melhores_CH$TP_ESCOLA,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$TP_ESCOLA,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CH$TP_ESCOLA,enem_2016_melhores_CH$regiao)[2,], sum(table(enem_2016_melhores_CH$TP_ESCOLA,enem_2016_melhores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CH$TP_ESCOLA,enem_2016_melhores_CH$regiao)[3,], sum(table(enem_2016_melhores_CH$TP_ESCOLA,enem_2016_melhores_CH$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_CH$TP_ESCOLA,enem_2016_melhores_CH$regiao)[4,], sum(table(enem_2016_melhores_CH$TP_ESCOLA,enem_2016_melhores_CH$regiao)[4,]))

dados_mCH_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mCH_tpe)[4] <- "Não Respondeu"
colnames(dados_mCH_tpe)[5] <- "Pública"
colnames(dados_mCH_tpe)[6] <- "Privada"
colnames(dados_mCH_tpe)[7] <- "Exterior"
row.names(dados_mCH_tpe)[6] <- "Brasil total"
dados_mCH_tpe[,4:7]<-round(100*(dados_mCH_tpe[,4:7]/rowSums(dados_mCH_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_melhores_CH$renda_SM =
  ifelse(enem_2016_melhores_CH$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_melhores_CH$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_melhores_CH$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_melhores_CH$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_melhores_CH$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_melhores_CH$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_melhores_CH$renda_SM = factor(enem_2016_melhores_CH$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                                   "De um a dois Salários-Mínimos",
                                                                                   "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                                   "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[2,], sum(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[2,]))
dado2<-c(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[3,], sum(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[3,]))
dado3<-c(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[4,], sum(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[4,]))
dado4<-c(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[5,], sum(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[5,]))
dado5<-c(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[6,], sum(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[6,]))
dado6<-c(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[7,], sum(table(enem_2016_melhores_CH$renda_SM,enem_2016_melhores_CH$regiao)[7,]))

dados_mCH_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_mCH_sm)[4] <- "Nenhuma renda"
colnames(dados_mCH_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_mCH_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_mCH_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_mCH_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_mCH_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_mCH_sm)[6] <- "Brasil total"
dados_mCH_sm[,4:9]<-round(100*(dados_mCH_sm[,4:9]/rowSums(dados_mCH_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_melhores_CH$Q049 =
  ifelse(enem_2016_melhores_CH$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_melhores_CH$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_melhores_CH$Q049 =
  factor(enem_2016_melhores_CH$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_melhores_CH$Q049, enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$Q049,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CH$Q049, enem_2016_melhores_CH$regiao)[2,], sum(table(enem_2016_melhores_CH$Q049,enem_2016_melhores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CH$Q049, enem_2016_melhores_CH$regiao)[3,], sum(table(enem_2016_melhores_CH$Q049,enem_2016_melhores_CH$regiao)[3,]))

dados_mCH_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_mCH_tur)[4] <- "Somente no diurno"
colnames(dados_mCH_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_mCH_tur)[6] <- "Somente no noturno"
row.names(dados_mCH_tur)[6] <- "Brasil total"
dados_mCH_tur[,4:6]<-round(100*(dados_mCH_tur[,4:6]/rowSums(dados_mCH_tur[,4:6])), digits = 1)

#######
# Melhores em CN
enem_2016_melhores_CN<-read.csv("Enem_2016/melhores_1000_CN.csv")

enem_2016_melhores_CN$regiao =
  ifelse(enem_2016_melhores_CN$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_melhores_CN$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_melhores_CN$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_melhores_CN$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_melhores_CN$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_melhores_CN$regiao =
  factor(enem_2016_melhores_CN$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_melhores_CN$TP_SEXO,enem_2016_melhores_CN$regiao)[1,], sum(table(enem_2016_melhores_CN$TP_SEXO,enem_2016_melhores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CN$TP_SEXO,enem_2016_melhores_CN$regiao)[2,], sum(table(enem_2016_melhores_CN$TP_SEXO,enem_2016_melhores_CN$regiao)[2,]))

dados_mCN_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_mCN_s)[4] <- "Mulheres"
colnames(dados_mCN_s)[5] <- "Homens"
row.names(dados_mCN_s)[6] <- "Brasil total"
dados_mCN_s[,4:5]<-round(100*(dados_mCN_s[,4:5]/rowSums(dados_mCN_s[,4:5])), digits = 1)
#IDade

enem_2016_melhores_CN$fx_idade =
  ifelse(enem_2016_melhores_CN$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_melhores_CN$NU_IDADE>=16 & enem_2016_melhores_CN$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_melhores_CN$NU_IDADE>=20 & enem_2016_melhores_CN$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_melhores_CN$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_melhores_CN$fx_idade =
  factor(enem_2016_melhores_CN$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                    "Mais de 25 anos"))
dado1<-c(table(enem_2016_melhores_CN$fx_idade,enem_2016_melhores_CN$regiao)[1,], sum(table(enem_2016_melhores_CN$fx_idade,enem_2016_melhores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CN$fx_idade,enem_2016_melhores_CN$regiao)[2,], sum(table(enem_2016_melhores_CN$fx_idade,enem_2016_melhores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CN$fx_idade,enem_2016_melhores_CN$regiao)[3,], sum(table(enem_2016_melhores_CN$fx_idade,enem_2016_melhores_CN$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_CN$fx_idade,enem_2016_melhores_CN$regiao)[4,], sum(table(enem_2016_melhores_CN$fx_idade,enem_2016_melhores_CN$regiao)[4,]))

dados_mCN_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mCN_i)[4] <- "Menos de 16 anos"
colnames(dados_mCN_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_mCN_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_mCN_i)[7] <- "Mais de 25 anos"
row.names(dados_mCN_i)[6] <- "Brasil total"
dados_mCN_i[,4:7]<-round(100*(dados_mCN_i[,4:7]/rowSums(dados_mCN_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_melhores_CN$Q047 =
  ifelse(enem_2016_melhores_CN$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_melhores_CN$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_melhores_CN$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_melhores_CN$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_melhores_CN$Q047 =
  factor(enem_2016_melhores_CN$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                                "Esc. Pública e Esc. Privada c/ bolsa integral",
                                                "Somente Esc. Privada s/ bolsa integral",
                                                "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[1,], sum(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[2,], sum(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[3,], sum(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[4,], sum(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[5,], sum(table(enem_2016_melhores_CN$Q047,enem_2016_melhores_CN$regiao)[5,]))

dados_mCN_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_mCN_q047)[4] <- "Somente Esc. Pública"
colnames(dados_mCN_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_mCN_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_mCN_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_mCN_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_mCN_q047)[6] <- "Brasil total"
dados_mCN_q047[,4:8]<-round(100*(dados_mCN_q047[,4:8]/rowSums(dados_mCN_q047[,4:8])), digits = 1)

enem_2016_melhores_CN$TP_COR_RACA =
  ifelse(enem_2016_melhores_CN$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_melhores_CN$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_melhores_CN$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_melhores_CN$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_melhores_CN$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_melhores_CN$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_melhores_CN$TP_COR_RACA =
  factor(enem_2016_melhores_CN$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                       'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[1,], sum(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[2,], sum(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[3,], sum(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[4,], sum(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[5,], sum(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[5,]))
dado6<-c(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[6,], sum(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[6,]))
dado7<-c(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[7,], sum(table(enem_2016_melhores_CN$TP_COR_RACA,enem_2016_melhores_CN$regiao)[7,]))

dados_mCN_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_mCN_r)[4] <- "Não declarado"
colnames(dados_mCN_r)[5] <- "Branca"
colnames(dados_mCN_r)[6] <- "Preta"
colnames(dados_mCN_r)[7] <- "Parda"
colnames(dados_mCN_r)[8] <- "Amarela"
colnames(dados_mCN_r)[9] <- "Indígena"
colnames(dados_mCN_r)[10] <- "Não dispõe da informação"
row.names(dados_mCN_r)[6] <- "Brasil total"
dados_mCN_r[,4:10]<-round(100*(dados_mCN_r[,4:10]/rowSums(dados_mCN_r[,4:10])), digits = 1)

enem_2016_melhores_CN$TP_ESCOLA =
  ifelse(enem_2016_melhores_CN$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_melhores_CN$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_melhores_CN$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_melhores_CN$TP_ESCOLA =
  factor(enem_2016_melhores_CN$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                     "Exterior"))
dado1<-c(table(enem_2016_melhores_CN$TP_ESCOLA,enem_2016_melhores_CN$regiao)[1,], sum(table(enem_2016_melhores_CN$TP_ESCOLA,enem_2016_melhores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CN$TP_ESCOLA,enem_2016_melhores_CN$regiao)[2,], sum(table(enem_2016_melhores_CN$TP_ESCOLA,enem_2016_melhores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CN$TP_ESCOLA,enem_2016_melhores_CN$regiao)[3,], sum(table(enem_2016_melhores_CN$TP_ESCOLA,enem_2016_melhores_CN$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_CN$TP_ESCOLA,enem_2016_melhores_CN$regiao)[4,], sum(table(enem_2016_melhores_CN$TP_ESCOLA,enem_2016_melhores_CN$regiao)[4,]))

dados_mCN_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mCN_tpe)[4] <- "Não Respondeu"
colnames(dados_mCN_tpe)[5] <- "Pública"
colnames(dados_mCN_tpe)[6] <- "Privada"
colnames(dados_mCN_tpe)[7] <- "Exterior"
row.names(dados_mCN_tpe)[6] <- "Brasil total"
dados_mCN_tpe[,4:7]<-round(100*(dados_mCN_tpe[,4:7]/rowSums(dados_mCN_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_melhores_CN$renda_SM =
  ifelse(enem_2016_melhores_CN$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_melhores_CN$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_melhores_CN$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_melhores_CN$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_melhores_CN$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_melhores_CN$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_melhores_CN$renda_SM = factor(enem_2016_melhores_CN$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                                   "De um a dois Salários-Mínimos",
                                                                                   "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                                   "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[2,], sum(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[2,]))
dado2<-c(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[3,], sum(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[3,]))
dado3<-c(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[4,], sum(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[4,]))
dado4<-c(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[5,], sum(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[5,]))
dado5<-c(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[6,], sum(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[6,]))
dado6<-c(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[7,], sum(table(enem_2016_melhores_CN$renda_SM,enem_2016_melhores_CN$regiao)[7,]))

dados_mCN_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_mCN_sm)[4] <- "Nenhuma renda"
colnames(dados_mCN_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_mCN_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_mCN_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_mCN_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_mCN_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_mCN_sm)[6] <- "Brasil total"
dados_mCN_sm[,4:9]<-round(100*(dados_mCN_sm[,4:9]/rowSums(dados_mCN_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_melhores_CN$Q049 =
  ifelse(enem_2016_melhores_CN$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_melhores_CN$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_melhores_CN$Q049 =
  factor(enem_2016_melhores_CN$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_melhores_CN$Q049, enem_2016_melhores_CN$regiao)[1,], sum(table(enem_2016_melhores_CN$Q049,enem_2016_melhores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_CN$Q049, enem_2016_melhores_CN$regiao)[2,], sum(table(enem_2016_melhores_CN$Q049,enem_2016_melhores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_CN$Q049, enem_2016_melhores_CN$regiao)[3,], sum(table(enem_2016_melhores_CN$Q049,enem_2016_melhores_CN$regiao)[3,]))

dados_mCN_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_mCN_tur)[4] <- "Somente no diurno"
colnames(dados_mCN_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_mCN_tur)[6] <- "Somente no noturno"
row.names(dados_mCN_tur)[6] <- "Brasil total"
dados_mCN_tur[,4:6]<-round(100*(dados_mCN_tur[,4:6]/rowSums(dados_mCN_tur[,4:6])), digits = 1)

#######
# Melhores em LC
enem_2016_melhores_LC<-read.csv("Enem_2016/melhores_1000_LC.csv")

enem_2016_melhores_LC$regiao =
  ifelse(enem_2016_melhores_LC$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_melhores_LC$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_melhores_LC$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_melhores_LC$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_melhores_LC$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_melhores_LC$regiao =
  factor(enem_2016_melhores_LC$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_melhores_LC$TP_SEXO,enem_2016_melhores_LC$regiao)[1,], sum(table(enem_2016_melhores_LC$TP_SEXO,enem_2016_melhores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_LC$TP_SEXO,enem_2016_melhores_LC$regiao)[2,], sum(table(enem_2016_melhores_LC$TP_SEXO,enem_2016_melhores_LC$regiao)[2,]))

dados_mLC_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_mLC_s)[4] <- "Mulheres"
colnames(dados_mLC_s)[5] <- "Homens"
row.names(dados_mLC_s)[6] <- "Brasil total"
dados_mLC_s[,4:5]<-round(100*(dados_mLC_s[,4:5]/rowSums(dados_mLC_s[,4:5])), digits = 1)
#IDade

enem_2016_melhores_LC$fx_idade =
  ifelse(enem_2016_melhores_LC$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_melhores_LC$NU_IDADE>=16 & enem_2016_melhores_LC$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_melhores_LC$NU_IDADE>=20 & enem_2016_melhores_LC$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_melhores_LC$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_melhores_LC$fx_idade =
  factor(enem_2016_melhores_LC$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                    "Mais de 25 anos"))
dado1<-c(table(enem_2016_melhores_LC$fx_idade,enem_2016_melhores_LC$regiao)[1,], sum(table(enem_2016_melhores_LC$fx_idade,enem_2016_melhores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_LC$fx_idade,enem_2016_melhores_LC$regiao)[2,], sum(table(enem_2016_melhores_LC$fx_idade,enem_2016_melhores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_LC$fx_idade,enem_2016_melhores_LC$regiao)[3,], sum(table(enem_2016_melhores_LC$fx_idade,enem_2016_melhores_LC$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_LC$fx_idade,enem_2016_melhores_LC$regiao)[4,], sum(table(enem_2016_melhores_LC$fx_idade,enem_2016_melhores_LC$regiao)[4,]))

dados_mLC_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mLC_i)[4] <- "Menos de 16 anos"
colnames(dados_mLC_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_mLC_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_mLC_i)[7] <- "Mais de 25 anos"
row.names(dados_mLC_i)[6] <- "Brasil total"
dados_mLC_i[,4:7]<-round(100*(dados_mLC_i[,4:7]/rowSums(dados_mLC_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_melhores_LC$Q047 =
  ifelse(enem_2016_melhores_LC$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_melhores_LC$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_melhores_LC$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_melhores_LC$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_melhores_LC$Q047 =
  factor(enem_2016_melhores_LC$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                                "Esc. Pública e Esc. Privada c/ bolsa integral",
                                                "Somente Esc. Privada s/ bolsa integral",
                                                "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[1,], sum(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[2,], sum(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[3,], sum(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[4,], sum(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[5,], sum(table(enem_2016_melhores_LC$Q047,enem_2016_melhores_LC$regiao)[5,]))

dados_mLC_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_mLC_q047)[4] <- "Somente Esc. Pública"
colnames(dados_mLC_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_mLC_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_mLC_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_mLC_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_mLC_q047)[6] <- "Brasil total"
dados_mLC_q047[,4:8]<-round(100*(dados_mLC_q047[,4:8]/rowSums(dados_mLC_q047[,4:8])), digits = 1)

enem_2016_melhores_LC$TP_COR_RACA =
  ifelse(enem_2016_melhores_LC$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_melhores_LC$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_melhores_LC$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_melhores_LC$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_melhores_LC$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_melhores_LC$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_melhores_LC$TP_COR_RACA =
  factor(enem_2016_melhores_LC$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                       'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[1,], sum(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[2,], sum(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[3,], sum(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[4,], sum(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[5,], sum(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[5,]))
dado6<-c(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[6,], sum(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[6,]))
dado7<-c(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[7,], sum(table(enem_2016_melhores_LC$TP_COR_RACA,enem_2016_melhores_LC$regiao)[7,]))

dados_mLC_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_mLC_r)[4] <- "Não declarado"
colnames(dados_mLC_r)[5] <- "Branca"
colnames(dados_mLC_r)[6] <- "Preta"
colnames(dados_mLC_r)[7] <- "Parda"
colnames(dados_mLC_r)[8] <- "Amarela"
colnames(dados_mLC_r)[9] <- "Indígena"
colnames(dados_mLC_r)[10] <- "Não dispõe da informação"
row.names(dados_mLC_r)[6] <- "Brasil total"
dados_mLC_r[,4:10]<-round(100*(dados_mLC_r[,4:10]/rowSums(dados_mLC_r[,4:10])), digits = 1)

enem_2016_melhores_LC$TP_ESCOLA =
  ifelse(enem_2016_melhores_LC$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_melhores_LC$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_melhores_LC$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_melhores_LC$TP_ESCOLA =
  factor(enem_2016_melhores_LC$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                     "Exterior"))
dado1<-c(table(enem_2016_melhores_LC$TP_ESCOLA,enem_2016_melhores_LC$regiao)[1,], sum(table(enem_2016_melhores_LC$TP_ESCOLA,enem_2016_melhores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_LC$TP_ESCOLA,enem_2016_melhores_LC$regiao)[2,], sum(table(enem_2016_melhores_LC$TP_ESCOLA,enem_2016_melhores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_LC$TP_ESCOLA,enem_2016_melhores_LC$regiao)[3,], sum(table(enem_2016_melhores_LC$TP_ESCOLA,enem_2016_melhores_LC$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_LC$TP_ESCOLA,enem_2016_melhores_LC$regiao)[4,], sum(table(enem_2016_melhores_LC$TP_ESCOLA,enem_2016_melhores_LC$regiao)[4,]))

dados_mLC_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mLC_tpe)[4] <- "Não Respondeu"
colnames(dados_mLC_tpe)[5] <- "Pública"
colnames(dados_mLC_tpe)[6] <- "Privada"
colnames(dados_mLC_tpe)[7] <- "Exterior"
row.names(dados_mLC_tpe)[6] <- "Brasil total"
dados_mLC_tpe[,4:7]<-round(100*(dados_mLC_tpe[,4:7]/rowSums(dados_mLC_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_melhores_LC$renda_SM =
  ifelse(enem_2016_melhores_LC$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_melhores_LC$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_melhores_LC$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_melhores_LC$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_melhores_LC$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_melhores_LC$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_melhores_LC$renda_SM = factor(enem_2016_melhores_LC$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                                   "De um a dois Salários-Mínimos",
                                                                                   "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                                   "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[2,], sum(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[2,]))
dado2<-c(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[3,], sum(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[3,]))
dado3<-c(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[4,], sum(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[4,]))
dado4<-c(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[5,], sum(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[5,]))
dado5<-c(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[6,], sum(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[6,]))
dado6<-c(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[7,], sum(table(enem_2016_melhores_LC$renda_SM,enem_2016_melhores_LC$regiao)[7,]))

dados_mLC_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_mLC_sm)[4] <- "Nenhuma renda"
colnames(dados_mLC_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_mLC_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_mLC_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_mLC_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_mLC_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_mLC_sm)[6] <- "Brasil total"
dados_mLC_sm[,4:9]<-round(100*(dados_mLC_sm[,4:9]/rowSums(dados_mLC_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_melhores_LC$Q049 =
  ifelse(enem_2016_melhores_LC$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_melhores_LC$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_melhores_LC$Q049 =
  factor(enem_2016_melhores_LC$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_melhores_LC$Q049, enem_2016_melhores_LC$regiao)[1,], sum(table(enem_2016_melhores_LC$Q049,enem_2016_melhores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_LC$Q049, enem_2016_melhores_LC$regiao)[2,], sum(table(enem_2016_melhores_LC$Q049,enem_2016_melhores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_LC$Q049, enem_2016_melhores_LC$regiao)[3,], sum(table(enem_2016_melhores_LC$Q049,enem_2016_melhores_LC$regiao)[3,]))

dados_mLC_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_mLC_tur)[4] <- "Somente no diurno"
colnames(dados_mLC_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_mLC_tur)[6] <- "Somente no noturno"
row.names(dados_mLC_tur)[6] <- "Brasil total"
dados_mLC_tur[,4:6]<-round(100*(dados_mLC_tur[,4:6]/rowSums(dados_mLC_tur[,4:6])), digits = 1)

#######
# Melhores em MT
enem_2016_melhores_MT<-read.csv("Enem_2016/melhores_1000_MT.csv")

enem_2016_melhores_MT$regiao =
  ifelse(enem_2016_melhores_MT$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_melhores_MT$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_melhores_MT$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_melhores_MT$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_melhores_MT$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_melhores_MT$regiao =
  factor(enem_2016_melhores_MT$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_melhores_MT$TP_SEXO,enem_2016_melhores_MT$regiao)[1,], sum(table(enem_2016_melhores_MT$TP_SEXO,enem_2016_melhores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_MT$TP_SEXO,enem_2016_melhores_MT$regiao)[2,], sum(table(enem_2016_melhores_MT$TP_SEXO,enem_2016_melhores_MT$regiao)[2,]))

dados_mMT_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_mMT_s)[4] <- "Mulheres"
colnames(dados_mMT_s)[5] <- "Homens"
row.names(dados_mMT_s)[6] <- "Brasil total"
dados_mMT_s[,4:5]<-round(100*(dados_mMT_s[,4:5]/rowSums(dados_mMT_s[,4:5])), digits = 1)
#IDade

enem_2016_melhores_MT$fx_idade =
  ifelse(enem_2016_melhores_MT$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_melhores_MT$NU_IDADE>=16 & enem_2016_melhores_MT$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_melhores_MT$NU_IDADE>=20 & enem_2016_melhores_MT$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_melhores_MT$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_melhores_MT$fx_idade =
  factor(enem_2016_melhores_MT$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                    "Mais de 25 anos"))
dado1<-c(table(enem_2016_melhores_MT$fx_idade,enem_2016_melhores_MT$regiao)[1,], sum(table(enem_2016_melhores_MT$fx_idade,enem_2016_melhores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_MT$fx_idade,enem_2016_melhores_MT$regiao)[2,], sum(table(enem_2016_melhores_MT$fx_idade,enem_2016_melhores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_MT$fx_idade,enem_2016_melhores_MT$regiao)[3,], sum(table(enem_2016_melhores_MT$fx_idade,enem_2016_melhores_MT$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_MT$fx_idade,enem_2016_melhores_MT$regiao)[4,], sum(table(enem_2016_melhores_MT$fx_idade,enem_2016_melhores_MT$regiao)[4,]))

dados_mMT_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mMT_i)[4] <- "Menos de 16 anos"
colnames(dados_mMT_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_mMT_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_mMT_i)[7] <- "Mais de 25 anos"
row.names(dados_mMT_i)[6] <- "Brasil total"
dados_mMT_i[,4:7]<-round(100*(dados_mMT_i[,4:7]/rowSums(dados_mMT_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_melhores_MT$Q047 =
  ifelse(enem_2016_melhores_MT$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_melhores_MT$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_melhores_MT$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_melhores_MT$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_melhores_MT$Q047 =
  factor(enem_2016_melhores_MT$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                                "Esc. Pública e Esc. Privada c/ bolsa integral",
                                                "Somente Esc. Privada s/ bolsa integral",
                                                "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[1,], sum(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[2,], sum(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[3,], sum(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[4,], sum(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[5,], sum(table(enem_2016_melhores_MT$Q047,enem_2016_melhores_MT$regiao)[5,]))

dados_mMT_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_mMT_q047)[4] <- "Somente Esc. Pública"
colnames(dados_mMT_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_mMT_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_mMT_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_mMT_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_mMT_q047)[6] <- "Brasil total"
dados_mMT_q047[,4:8]<-round(100*(dados_mMT_q047[,4:8]/rowSums(dados_mMT_q047[,4:8])), digits = 1)

enem_2016_melhores_MT$TP_COR_RACA =
  ifelse(enem_2016_melhores_MT$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_melhores_MT$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_melhores_MT$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_melhores_MT$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_melhores_MT$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_melhores_MT$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_melhores_MT$TP_COR_RACA =
  factor(enem_2016_melhores_MT$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                       'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[1,], sum(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[2,], sum(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[3,], sum(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[4,], sum(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[5,], sum(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[5,]))
dado6<-c(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[6,], sum(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[6,]))
dado7<-c(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[7,], sum(table(enem_2016_melhores_MT$TP_COR_RACA,enem_2016_melhores_MT$regiao)[7,]))

dados_mMT_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_mMT_r)[4] <- "Não declarado"
colnames(dados_mMT_r)[5] <- "Branca"
colnames(dados_mMT_r)[6] <- "Preta"
colnames(dados_mMT_r)[7] <- "Parda"
colnames(dados_mMT_r)[8] <- "Amarela"
colnames(dados_mMT_r)[9] <- "Indígena"
colnames(dados_mMT_r)[10] <- "Não dispõe da informação"
row.names(dados_mMT_r)[6] <- "Brasil total"
dados_mMT_r[,4:10]<-round(100*(dados_mMT_r[,4:10]/rowSums(dados_mMT_r[,4:10])), digits = 1)

enem_2016_melhores_MT$TP_ESCOLA =
  ifelse(enem_2016_melhores_MT$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_melhores_MT$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_melhores_MT$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_melhores_MT$TP_ESCOLA =
  factor(enem_2016_melhores_MT$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                     "Exterior"))
dado1<-c(table(enem_2016_melhores_MT$TP_ESCOLA,enem_2016_melhores_MT$regiao)[1,], sum(table(enem_2016_melhores_MT$TP_ESCOLA,enem_2016_melhores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_MT$TP_ESCOLA,enem_2016_melhores_MT$regiao)[2,], sum(table(enem_2016_melhores_MT$TP_ESCOLA,enem_2016_melhores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_MT$TP_ESCOLA,enem_2016_melhores_MT$regiao)[3,], sum(table(enem_2016_melhores_MT$TP_ESCOLA,enem_2016_melhores_MT$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_MT$TP_ESCOLA,enem_2016_melhores_MT$regiao)[4,], sum(table(enem_2016_melhores_MT$TP_ESCOLA,enem_2016_melhores_MT$regiao)[4,]))

dados_mMT_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mMT_tpe)[4] <- "Não Respondeu"
colnames(dados_mMT_tpe)[5] <- "Pública"
colnames(dados_mMT_tpe)[6] <- "Privada"
colnames(dados_mMT_tpe)[7] <- "Exterior"
row.names(dados_mMT_tpe)[6] <- "Brasil total"
dados_mMT_tpe[,4:7]<-round(100*(dados_mMT_tpe[,4:7]/rowSums(dados_mMT_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_melhores_MT$renda_SM =
  ifelse(enem_2016_melhores_MT$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_melhores_MT$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_melhores_MT$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_melhores_MT$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_melhores_MT$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_melhores_MT$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_melhores_MT$renda_SM = factor(enem_2016_melhores_MT$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                                   "De um a dois Salários-Mínimos",
                                                                                   "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                                   "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[2,], sum(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[2,]))
dado2<-c(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[3,], sum(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[3,]))
dado3<-c(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[4,], sum(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[4,]))
dado4<-c(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[5,], sum(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[5,]))
dado5<-c(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[6,], sum(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[6,]))
dado6<-c(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[7,], sum(table(enem_2016_melhores_MT$renda_SM,enem_2016_melhores_MT$regiao)[7,]))

dados_mMT_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_mMT_sm)[4] <- "Nenhuma renda"
colnames(dados_mMT_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_mMT_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_mMT_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_mMT_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_mMT_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_mMT_sm)[6] <- "Brasil total"
dados_mMT_sm[,4:9]<-round(100*(dados_mMT_sm[,4:9]/rowSums(dados_mMT_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_melhores_MT$Q049 =
  ifelse(enem_2016_melhores_MT$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_melhores_MT$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_melhores_MT$Q049 =
  factor(enem_2016_melhores_MT$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_melhores_MT$Q049, enem_2016_melhores_MT$regiao)[1,], sum(table(enem_2016_melhores_MT$Q049,enem_2016_melhores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_MT$Q049, enem_2016_melhores_MT$regiao)[2,], sum(table(enem_2016_melhores_MT$Q049,enem_2016_melhores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_MT$Q049, enem_2016_melhores_MT$regiao)[3,], sum(table(enem_2016_melhores_MT$Q049,enem_2016_melhores_MT$regiao)[3,]))

dados_mMT_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_mMT_tur)[4] <- "Somente no diurno"
colnames(dados_mMT_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_mMT_tur)[6] <- "Somente no noturno"
row.names(dados_mMT_tur)[6] <- "Brasil total"
dados_mMT_tur[,4:6]<-round(100*(dados_mMT_tur[,4:6]/rowSums(dados_mMT_tur[,4:6])), digits = 1)

#######
# Melhores em redaÃ§Ã£o
enem_2016_melhores_RE<-read.csv("Enem_2016/melhores_1000_REDACAO.csv")

enem_2016_melhores_RE$regiao =
  ifelse(enem_2016_melhores_RE$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_melhores_RE$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_melhores_RE$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_melhores_RE$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_melhores_RE$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_melhores_RE$regiao =
  factor(enem_2016_melhores_RE$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_melhores_RE$TP_SEXO,enem_2016_melhores_RE$regiao)[1,], sum(table(enem_2016_melhores_RE$TP_SEXO,enem_2016_melhores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_RE$TP_SEXO,enem_2016_melhores_RE$regiao)[2,], sum(table(enem_2016_melhores_RE$TP_SEXO,enem_2016_melhores_RE$regiao)[2,]))

dados_mRE_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_mRE_s)[4] <- "Mulheres"
colnames(dados_mRE_s)[5] <- "Homens"
row.names(dados_mRE_s)[6] <- "Brasil total"
dados_mRE_s[,4:5]<-round(100*(dados_mRE_s[,4:5]/rowSums(dados_mRE_s[,4:5])), digits = 1)
#IDade

enem_2016_melhores_RE$fx_idade =
  ifelse(enem_2016_melhores_RE$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_melhores_RE$NU_IDADE>=16 & enem_2016_melhores_RE$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_melhores_RE$NU_IDADE>=20 & enem_2016_melhores_RE$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_melhores_RE$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_melhores_RE$fx_idade =
  factor(enem_2016_melhores_RE$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                    "Mais de 25 anos"))
dado1<-c(table(enem_2016_melhores_RE$fx_idade,enem_2016_melhores_RE$regiao)[1,], sum(table(enem_2016_melhores_RE$fx_idade,enem_2016_melhores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_RE$fx_idade,enem_2016_melhores_RE$regiao)[2,], sum(table(enem_2016_melhores_RE$fx_idade,enem_2016_melhores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_RE$fx_idade,enem_2016_melhores_RE$regiao)[3,], sum(table(enem_2016_melhores_RE$fx_idade,enem_2016_melhores_RE$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_RE$fx_idade,enem_2016_melhores_RE$regiao)[4,], sum(table(enem_2016_melhores_RE$fx_idade,enem_2016_melhores_RE$regiao)[4,]))

dados_mRE_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mRE_i)[4] <- "Menos de 16 anos"
colnames(dados_mRE_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_mRE_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_mRE_i)[7] <- "Mais de 25 anos"
row.names(dados_mRE_i)[6] <- "Brasil total"
dados_mRE_i[,4:7]<-round(100*(dados_mRE_i[,4:7]/rowSums(dados_mRE_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_melhores_RE$Q047 =
  ifelse(enem_2016_melhores_RE$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_melhores_RE$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_melhores_RE$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_melhores_RE$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_melhores_RE$Q047 =
  factor(enem_2016_melhores_RE$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                                "Esc. Pública e Esc. Privada c/ bolsa integral",
                                                "Somente Esc. Privada s/ bolsa integral",
                                                "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[1,], sum(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[2,], sum(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[3,], sum(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[4,], sum(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[5,], sum(table(enem_2016_melhores_RE$Q047,enem_2016_melhores_RE$regiao)[5,]))

dados_mRE_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_mRE_q047)[4] <- "Somente Esc. Pública"
colnames(dados_mRE_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_mRE_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_mRE_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_mRE_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_mRE_q047)[6] <- "Brasil total"
dados_mRE_q047[,4:8]<-round(100*(dados_mRE_q047[,4:8]/rowSums(dados_mRE_q047[,4:8])), digits = 1)

enem_2016_melhores_RE$TP_COR_RACA =
  ifelse(enem_2016_melhores_RE$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_melhores_RE$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_melhores_RE$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_melhores_RE$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_melhores_RE$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_melhores_RE$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_melhores_RE$TP_COR_RACA =
  factor(enem_2016_melhores_RE$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                       'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[1,], sum(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[2,], sum(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[3,], sum(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[4,], sum(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[4,]))
dado5<-c(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[5,], sum(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[5,]))
dado6<-c(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[6,], sum(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[6,]))
dado7<-c(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[7,], sum(table(enem_2016_melhores_RE$TP_COR_RACA,enem_2016_melhores_RE$regiao)[7,]))

dados_mRE_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_mRE_r)[4] <- "Não declarado"
colnames(dados_mRE_r)[5] <- "Branca"
colnames(dados_mRE_r)[6] <- "Preta"
colnames(dados_mRE_r)[7] <- "Parda"
colnames(dados_mRE_r)[8] <- "Amarela"
colnames(dados_mRE_r)[9] <- "Indígena"
colnames(dados_mRE_r)[10] <- "Não dispõe da informação"
row.names(dados_mRE_r)[6] <- "Brasil total"
dados_mRE_r[,4:10]<-round(100*(dados_mRE_r[,4:10]/rowSums(dados_mRE_r[,4:10])), digits = 1)

enem_2016_melhores_RE$TP_ESCOLA =
  ifelse(enem_2016_melhores_RE$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_melhores_RE$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_melhores_RE$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_melhores_RE$TP_ESCOLA =
  factor(enem_2016_melhores_RE$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                     "Exterior"))
dado1<-c(table(enem_2016_melhores_RE$TP_ESCOLA,enem_2016_melhores_RE$regiao)[1,], sum(table(enem_2016_melhores_RE$TP_ESCOLA,enem_2016_melhores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_RE$TP_ESCOLA,enem_2016_melhores_RE$regiao)[2,], sum(table(enem_2016_melhores_RE$TP_ESCOLA,enem_2016_melhores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_RE$TP_ESCOLA,enem_2016_melhores_RE$regiao)[3,], sum(table(enem_2016_melhores_RE$TP_ESCOLA,enem_2016_melhores_RE$regiao)[3,]))
dado4<-c(table(enem_2016_melhores_RE$TP_ESCOLA,enem_2016_melhores_RE$regiao)[4,], sum(table(enem_2016_melhores_RE$TP_ESCOLA,enem_2016_melhores_RE$regiao)[4,]))

dados_mRE_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_mRE_tpe)[4] <- "Não Respondeu"
colnames(dados_mRE_tpe)[5] <- "Pública"
colnames(dados_mRE_tpe)[6] <- "Privada"
colnames(dados_mRE_tpe)[7] <- "Exterior"
row.names(dados_mRE_tpe)[6] <- "Brasil total"
dados_mRE_tpe[,4:7]<-round(100*(dados_mRE_tpe[,4:7]/rowSums(dados_mRE_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_melhores_RE$renda_SM =
  ifelse(enem_2016_melhores_RE$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_melhores_RE$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_melhores_RE$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_melhores_RE$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_melhores_RE$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_melhores_RE$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_melhores_RE$renda_SM = factor(enem_2016_melhores_RE$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                                   "De um a dois Salários-Mínimos",
                                                                                   "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                                   "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[2,], sum(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[2,]))
dado2<-c(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[3,], sum(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[3,]))
dado3<-c(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[4,], sum(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[4,]))
dado4<-c(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[5,], sum(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[5,]))
dado5<-c(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[6,], sum(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[6,]))
dado6<-c(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[7,], sum(table(enem_2016_melhores_RE$renda_SM,enem_2016_melhores_RE$regiao)[7,]))

dados_mRE_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_mRE_sm)[4] <- "Nenhuma renda"
colnames(dados_mRE_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_mRE_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_mRE_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_mRE_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_mRE_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_mRE_sm)[6] <- "Brasil total"
dados_mRE_sm[,4:9]<-round(100*(dados_mRE_sm[,4:9]/rowSums(dados_mRE_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_melhores_RE$Q049 =
  ifelse(enem_2016_melhores_RE$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_melhores_RE$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_melhores_RE$Q049 =
  factor(enem_2016_melhores_RE$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_melhores_RE$Q049, enem_2016_melhores_RE$regiao)[1,], sum(table(enem_2016_melhores_RE$Q049,enem_2016_melhores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_melhores_RE$Q049, enem_2016_melhores_RE$regiao)[2,], sum(table(enem_2016_melhores_RE$Q049,enem_2016_melhores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_melhores_RE$Q049, enem_2016_melhores_RE$regiao)[3,], sum(table(enem_2016_melhores_RE$Q049,enem_2016_melhores_RE$regiao)[3,]))

dados_mRE_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_mRE_tur)[4] <- "Somente no diurno"
colnames(dados_mRE_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_mRE_tur)[6] <- "Somente no noturno"
row.names(dados_mRE_tur)[6] <- "Brasil total"
dados_mRE_tur[,4:6]<-round(100*(dados_mRE_tur[,4:6]/rowSums(dados_mRE_tur[,4:6])), digits = 1)

#####
# Piores Nota final

enem_2016_mil_piores<-read.csv("Enem_2016/piores_1000.csv")
enem_2016_mil_piores$regiao =
  ifelse(enem_2016_mil_piores$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_mil_piores$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_mil_piores$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_mil_piores$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_mil_piores$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_mil_piores$regiao =
  factor(enem_2016_mil_piores$regiao, levels = c('S','SE','CO','NE','N'))
dado1<-c(table(enem_2016_mil_piores$TP_SEXO,enem_2016_mil_piores$regiao)[1,], sum(table(enem_2016_mil_piores$TP_SEXO,enem_2016_mil_piores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_piores$TP_SEXO,enem_2016_mil_piores$regiao)[2,], sum(table(enem_2016_mil_piores$TP_SEXO,enem_2016_mil_piores$regiao)[2,]))

dados_p_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_p_s)[4] <- "Mulheres"
colnames(dados_p_s)[5] <- "Homens"
row.names(dados_p_s)[6] <- "Brasil total"
dados_p_s[,4:5]<-round(100*(dados_p_s[,4:5]/rowSums(dados_p_s[,4:5])), digits = 1)

#IDade

enem_2016_mil_piores$fx_idade =
  ifelse(enem_2016_mil_piores$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_mil_piores$NU_IDADE>=16 & enem_2016_mil_piores$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_mil_piores$NU_IDADE>=20 & enem_2016_mil_piores$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_mil_piores$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_mil_piores$fx_idade =
  factor(enem_2016_mil_piores$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                   "Mais de 25 anos"))
dado1<-c(table(enem_2016_mil_piores$fx_idade,enem_2016_mil_piores$regiao)[1,], sum(table(enem_2016_mil_piores$fx_idade,enem_2016_mil_piores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_piores$fx_idade,enem_2016_mil_piores$regiao)[2,], sum(table(enem_2016_mil_piores$fx_idade,enem_2016_mil_piores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_piores$fx_idade,enem_2016_mil_piores$regiao)[3,], sum(table(enem_2016_mil_piores$fx_idade,enem_2016_mil_piores$regiao)[3,]))
dado4<-c(table(enem_2016_mil_piores$fx_idade,enem_2016_mil_piores$regiao)[4,], sum(table(enem_2016_mil_piores$fx_idade,enem_2016_mil_piores$regiao)[4,]))

dados_p_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_p_i)[4] <- "Menos de 16 anos"
colnames(dados_p_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_p_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_p_i)[7] <- "Mais de 25 anos"
row.names(dados_p_i)[6] <- "Brasil total"
dados_p_i[,4:7]<-round(100*(dados_p_i[,4:7]/rowSums(dados_p_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_mil_piores$Q047 =
  ifelse(enem_2016_mil_piores$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_mil_piores$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_mil_piores$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_mil_piores$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_mil_piores$Q047 =
  factor(enem_2016_mil_piores$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                               "Esc. Pública e Esc. Privada c/ bolsa integral",
                                               "Somente Esc. Privada s/ bolsa integral",
                                               "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[1,], sum(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[2,], sum(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[3,], sum(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[3,]))
dado4<-c(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[4,], sum(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[4,]))
dado5<-c(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[5,], sum(table(enem_2016_mil_piores$Q047,enem_2016_mil_piores$regiao)[5,]))

dados_p_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_p_q047)[4] <- "Somente Esc. Pública"
colnames(dados_p_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_p_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_p_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_p_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_p_q047)[6] <- "Brasil total"
dados_p_q047[,4:8]<-round(100*(dados_p_q047[,4:8]/rowSums(dados_p_q047[,4:8])), digits = 1)

enem_2016_mil_piores$TP_COR_RACA =
  ifelse(enem_2016_mil_piores$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_mil_piores$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_mil_piores$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_mil_piores$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_mil_piores$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_mil_piores$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_mil_piores$TP_COR_RACA =
  factor(enem_2016_mil_piores$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                      'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[1,], sum(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[2,], sum(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[3,], sum(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[3,]))
dado4<-c(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[4,], sum(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[4,]))
dado5<-c(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[5,], sum(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[5,]))
dado6<-c(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[6,], sum(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[6,]))
dado7<-c(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[7,], sum(table(enem_2016_mil_piores$TP_COR_RACA,enem_2016_mil_piores$regiao)[7,]))

dados_p_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_p_r)[4] <- "Não declarado"
colnames(dados_p_r)[5] <- "Branca"
colnames(dados_p_r)[6] <- "Preta"
colnames(dados_p_r)[7] <- "Parda"
colnames(dados_p_r)[8] <- "Amarela"
colnames(dados_p_r)[9] <- "Indígena"
colnames(dados_p_r)[10] <- "Não dispõe da informação"
row.names(dados_p_r)[6] <- "Brasil total"
dados_p_r[,4:10]<-round(100*(dados_p_r[,4:10]/rowSums(dados_p_r[,4:10])), digits = 1)

enem_2016_mil_piores$TP_ESCOLA =
  ifelse(enem_2016_mil_piores$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_mil_piores$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_mil_piores$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_mil_piores$TP_ESCOLA =
  factor(enem_2016_mil_piores$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                    "Exterior"))
dado1<-c(table(enem_2016_mil_piores$TP_ESCOLA,enem_2016_mil_piores$regiao)[1,], sum(table(enem_2016_mil_piores$TP_ESCOLA,enem_2016_mil_piores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_piores$TP_ESCOLA,enem_2016_mil_piores$regiao)[2,], sum(table(enem_2016_mil_piores$TP_ESCOLA,enem_2016_mil_piores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_piores$TP_ESCOLA,enem_2016_mil_piores$regiao)[3,], sum(table(enem_2016_mil_piores$TP_ESCOLA,enem_2016_mil_piores$regiao)[3,]))
dado4<-c(table(enem_2016_mil_piores$TP_ESCOLA,enem_2016_mil_piores$regiao)[4,], sum(table(enem_2016_mil_piores$TP_ESCOLA,enem_2016_mil_piores$regiao)[4,]))

dados_p_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_p_tpe)[4] <- "Não Respondeu"
colnames(dados_p_tpe)[5] <- "Pública"
colnames(dados_p_tpe)[6] <- "Privada"
colnames(dados_p_tpe)[7] <- "Exterior"
row.names(dados_p_tpe)[6] <- "Brasil total"
dados_p_tpe[,4:7]<-round(100*(dados_p_tpe[,4:7]/rowSums(dados_p_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_mil_piores$renda_SM =
  ifelse(enem_2016_mil_piores$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_mil_piores$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_mil_piores$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_mil_piores$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_mil_piores$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_mil_piores$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_mil_piores$renda_SM = factor(enem_2016_mil_piores$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                                 "De um a dois Salários-Mínimos",
                                                                                 "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                                 "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[2,], sum(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[2,]))
dado2<-c(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[3,], sum(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[3,]))
dado3<-c(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[4,], sum(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[4,]))
dado4<-c(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[5,], sum(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[5,]))
dado5<-c(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[6,], sum(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[6,]))
dado6<-c(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[7,], sum(table(enem_2016_mil_piores$renda_SM,enem_2016_mil_piores$regiao)[7,]))

dados_p_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_p_sm)[4] <- "Nenhuma renda"
colnames(dados_p_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_p_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_p_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_p_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_p_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_p_sm)[6] <- "Brasil total"
dados_p_sm[,4:9]<-round(100*(dados_p_sm[,4:9]/rowSums(dados_p_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_mil_piores$Q049 =
  ifelse(enem_2016_mil_piores$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_mil_piores$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_mil_piores$Q049 =
  factor(enem_2016_mil_piores$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_mil_piores$Q049, enem_2016_mil_piores$regiao)[1,], sum(table(enem_2016_mil_piores$Q049,enem_2016_mil_piores$regiao)[1,]))
dado2<-c(table(enem_2016_mil_piores$Q049, enem_2016_mil_piores$regiao)[2,], sum(table(enem_2016_mil_piores$Q049,enem_2016_mil_piores$regiao)[2,]))
dado3<-c(table(enem_2016_mil_piores$Q049, enem_2016_mil_piores$regiao)[3,], sum(table(enem_2016_mil_piores$Q049,enem_2016_mil_piores$regiao)[3,]))

dados_p_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_p_tur)[4] <- "Somente no diurno"
colnames(dados_p_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_p_tur)[6] <- "Somente no noturno"
row.names(dados_p_tur)[6] <- "Brasil total"
dados_p_tur[,4:6]<-round(100*(dados_p_tur[,4:6]/rowSums(dados_p_tur[,4:6])), digits = 1)

#######
# Piores em CH
enem_2016_piores_CH<-read.csv("Enem_2016/piores_1000_CH.csv")


enem_2016_piores_CH$regiao =
  ifelse(enem_2016_piores_CH$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_piores_CH$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_piores_CH$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_piores_CH$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_piores_CH$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_piores_CH$regiao =
  factor(enem_2016_piores_CH$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_piores_CH$TP_SEXO,enem_2016_piores_CH$regiao)[1,], sum(table(enem_2016_piores_CH$TP_SEXO,enem_2016_piores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CH$TP_SEXO,enem_2016_piores_CH$regiao)[2,], sum(table(enem_2016_piores_CH$TP_SEXO,enem_2016_piores_CH$regiao)[2,]))

dados_pCH_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_pCH_s)[4] <- "Mulheres"
colnames(dados_pCH_s)[5] <- "Homens"
row.names(dados_pCH_s)[6] <- "Brasil total"
dados_pCH_s[,4:5]<-round(100*(dados_pCH_s[,4:5]/rowSums(dados_pCH_s[,4:5])), digits = 1)
#IDade

enem_2016_piores_CH$fx_idade =
  ifelse(enem_2016_piores_CH$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_piores_CH$NU_IDADE>=16 & enem_2016_piores_CH$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_piores_CH$NU_IDADE>=20 & enem_2016_piores_CH$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_piores_CH$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_piores_CH$fx_idade =
  factor(enem_2016_piores_CH$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                  "Mais de 25 anos"))
dado1<-c(table(enem_2016_piores_CH$fx_idade,enem_2016_piores_CH$regiao)[1,], sum(table(enem_2016_piores_CH$fx_idade,enem_2016_piores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CH$fx_idade,enem_2016_piores_CH$regiao)[2,], sum(table(enem_2016_piores_CH$fx_idade,enem_2016_piores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CH$fx_idade,enem_2016_piores_CH$regiao)[3,], sum(table(enem_2016_piores_CH$fx_idade,enem_2016_piores_CH$regiao)[3,]))
dado4<-c(table(enem_2016_piores_CH$fx_idade,enem_2016_piores_CH$regiao)[4,], sum(table(enem_2016_piores_CH$fx_idade,enem_2016_piores_CH$regiao)[4,]))

dados_pCH_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pCH_i)[4] <- "Menos de 16 anos"
colnames(dados_pCH_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_pCH_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_pCH_i)[7] <- "Mais de 25 anos"
row.names(dados_pCH_i)[6] <- "Brasil total"
dados_pCH_i[,4:7]<-round(100*(dados_pCH_i[,4:7]/rowSums(dados_pCH_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_piores_CH$Q047 =
  ifelse(enem_2016_piores_CH$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_piores_CH$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_piores_CH$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_piores_CH$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_piores_CH$Q047 =
  factor(enem_2016_piores_CH$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                              "Esc. Pública e Esc. Privada c/ bolsa integral",
                                              "Somente Esc. Privada s/ bolsa integral",
                                              "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_piores_CH$Q047,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CH$Q047,enem_2016_piores_CH$regiao)[2,], sum(table(enem_2016_piores_CH$Q047,enem_2016_piores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CH$Q047,enem_2016_piores_CH$regiao)[3,], sum(table(enem_2016_piores_CH$Q047,enem_2016_piores_CH$regiao)[3,]))
dado4<-c(table(enem_2016_piores_CH$Q047,enem_2016_piores_CH$regiao)[4,], sum(table(enem_2016_piores_CH$Q047,enem_2016_piores_CH$regiao)[4,]))
dado5<-c(table(enem_2016_piores_CH$Q047,enem_2016_piores_CH$regiao)[5,], sum(table(enem_2016_piores_CH$Q047,enem_2016_piores_CH$regiao)[5,]))

dados_pCH_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_pCH_q047)[4] <- "Somente Esc. Pública"
colnames(dados_pCH_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_pCH_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_pCH_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_pCH_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_pCH_q047)[6] <- "Brasil total"
dados_pCH_q047[,4:8]<-round(100*(dados_pCH_q047[,4:8]/rowSums(dados_pCH_q047[,4:8])), digits = 1)

enem_2016_piores_CH$TP_COR_RACA =
  ifelse(enem_2016_piores_CH$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_piores_CH$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_piores_CH$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_piores_CH$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_piores_CH$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_piores_CH$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_piores_CH$TP_COR_RACA =
  factor(enem_2016_piores_CH$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                     'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[1,], sum(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[2,], sum(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[3,], sum(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[3,]))
dado4<-c(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[4,], sum(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[4,]))
dado5<-c(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[5,], sum(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[5,]))
dado6<-c(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[6,], sum(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[6,]))
dado7<-c(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[7,], sum(table(enem_2016_piores_CH$TP_COR_RACA,enem_2016_piores_CH$regiao)[7,]))

dados_pCH_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_pCH_r)[4] <- "Não declarado"
colnames(dados_pCH_r)[5] <- "Branca"
colnames(dados_pCH_r)[6] <- "Preta"
colnames(dados_pCH_r)[7] <- "Parda"
colnames(dados_pCH_r)[8] <- "Amarela"
colnames(dados_pCH_r)[9] <- "Indígena"
colnames(dados_pCH_r)[10] <- "Não dispõe da informação"
row.names(dados_pCH_r)[6] <- "Brasil total"
dados_pCH_r[,4:10]<-round(100*(dados_pCH_r[,4:10]/rowSums(dados_pCH_r[,4:10])), digits = 1)

enem_2016_piores_CH$TP_ESCOLA =
  ifelse(enem_2016_piores_CH$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_piores_CH$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_piores_CH$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_piores_CH$TP_ESCOLA =
  factor(enem_2016_piores_CH$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                   "Exterior"))
dado1<-c(table(enem_2016_piores_CH$TP_ESCOLA,enem_2016_piores_CH$regiao)[1,], sum(table(enem_2016_piores_CH$TP_ESCOLA,enem_2016_piores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CH$TP_ESCOLA,enem_2016_piores_CH$regiao)[2,], sum(table(enem_2016_piores_CH$TP_ESCOLA,enem_2016_piores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CH$TP_ESCOLA,enem_2016_piores_CH$regiao)[3,], sum(table(enem_2016_piores_CH$TP_ESCOLA,enem_2016_piores_CH$regiao)[3,]))
dado4<-c(table(enem_2016_piores_CH$TP_ESCOLA,enem_2016_piores_CH$regiao)[4,], sum(table(enem_2016_piores_CH$TP_ESCOLA,enem_2016_piores_CH$regiao)[4,]))

dados_pCH_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pCH_tpe)[4] <- "Não Respondeu"
colnames(dados_pCH_tpe)[5] <- "Pública"
colnames(dados_pCH_tpe)[6] <- "Privada"
colnames(dados_pCH_tpe)[7] <- "Exterior"
row.names(dados_pCH_tpe)[6] <- "Brasil total"
dados_pCH_tpe[,4:7]<-round(100*(dados_pCH_tpe[,4:7]/rowSums(dados_pCH_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_piores_CH$renda_SM =
  ifelse(enem_2016_piores_CH$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_piores_CH$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_piores_CH$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_piores_CH$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_piores_CH$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_piores_CH$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_piores_CH$renda_SM = factor(enem_2016_piores_CH$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                               "De um a dois Salários-Mínimos",
                                                                               "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                               "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[2,], sum(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[2,]))
dado2<-c(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[3,], sum(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[3,]))
dado3<-c(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[4,], sum(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[4,]))
dado4<-c(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[5,], sum(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[5,]))
dado5<-c(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[6,], sum(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[6,]))
dado6<-c(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[7,], sum(table(enem_2016_piores_CH$renda_SM,enem_2016_piores_CH$regiao)[7,]))

dados_pCH_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_pCH_sm)[4] <- "Nenhuma renda"
colnames(dados_pCH_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_pCH_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_pCH_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_pCH_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_pCH_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_pCH_sm)[6] <- "Brasil total"
dados_pCH_sm[,4:9]<-round(100*(dados_pCH_sm[,4:9]/rowSums(dados_pCH_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_piores_CH$Q049 =
  ifelse(enem_2016_piores_CH$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_piores_CH$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_piores_CH$Q049 =
  factor(enem_2016_piores_CH$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_piores_CH$Q049, enem_2016_piores_CH$regiao)[1,], sum(table(enem_2016_piores_CH$Q049,enem_2016_piores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CH$Q049, enem_2016_piores_CH$regiao)[2,], sum(table(enem_2016_piores_CH$Q049,enem_2016_piores_CH$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CH$Q049, enem_2016_piores_CH$regiao)[3,], sum(table(enem_2016_piores_CH$Q049,enem_2016_piores_CH$regiao)[3,]))

dados_pCH_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_pCH_tur)[4] <- "Somente no diurno"
colnames(dados_pCH_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_pCH_tur)[6] <- "Somente no noturno"
row.names(dados_pCH_tur)[6] <- "Brasil total"
dados_pCH_tur[,4:6]<-round(100*(dados_pCH_tur[,4:6]/rowSums(dados_pCH_tur[,4:6])), digits = 1)

#######
# Piores em CN
enem_2016_piores_CN<-read.csv("Enem_2016/piores_1000_CN.csv")

enem_2016_piores_CN$regiao =
  ifelse(enem_2016_piores_CN$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_piores_CN$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_piores_CN$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_piores_CN$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_piores_CN$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_piores_CN$regiao =
  factor(enem_2016_piores_CN$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_piores_CN$TP_SEXO,enem_2016_piores_CN$regiao)[1,], sum(table(enem_2016_piores_CN$TP_SEXO,enem_2016_piores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CN$TP_SEXO,enem_2016_piores_CN$regiao)[2,], sum(table(enem_2016_piores_CN$TP_SEXO,enem_2016_piores_CN$regiao)[2,]))

dados_pCN_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_pCN_s)[4] <- "Mulheres"
colnames(dados_pCN_s)[5] <- "Homens"
row.names(dados_pCN_s)[6] <- "Brasil total"
dados_pCN_s[,4:5]<-round(100*(dados_pCN_s[,4:5]/rowSums(dados_pCN_s[,4:5])), digits = 1)
#IDade

enem_2016_piores_CN$fx_idade =
  ifelse(enem_2016_piores_CN$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_piores_CN$NU_IDADE>=16 & enem_2016_piores_CN$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_piores_CN$NU_IDADE>=20 & enem_2016_piores_CN$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_piores_CN$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_piores_CN$fx_idade =
  factor(enem_2016_piores_CN$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                  "Mais de 25 anos"))
dado1<-c(table(enem_2016_piores_CN$fx_idade,enem_2016_piores_CN$regiao)[1,], sum(table(enem_2016_piores_CN$fx_idade,enem_2016_piores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CN$fx_idade,enem_2016_piores_CN$regiao)[2,], sum(table(enem_2016_piores_CN$fx_idade,enem_2016_piores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CN$fx_idade,enem_2016_piores_CN$regiao)[3,], sum(table(enem_2016_piores_CN$fx_idade,enem_2016_piores_CN$regiao)[3,]))
dado4<-c(table(enem_2016_piores_CN$fx_idade,enem_2016_piores_CN$regiao)[4,], sum(table(enem_2016_piores_CN$fx_idade,enem_2016_piores_CN$regiao)[4,]))

dados_pCN_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pCN_i)[4] <- "Menos de 16 anos"
colnames(dados_pCN_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_pCN_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_pCN_i)[7] <- "Mais de 25 anos"
row.names(dados_pCN_i)[6] <- "Brasil total"
dados_pCN_i[,4:7]<-round(100*(dados_pCN_i[,4:7]/rowSums(dados_pCN_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_piores_CN$Q047 =
  ifelse(enem_2016_piores_CN$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_piores_CN$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_piores_CN$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_piores_CN$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_piores_CN$Q047 =
  factor(enem_2016_piores_CN$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                              "Esc. Pública e Esc. Privada c/ bolsa integral",
                                              "Somente Esc. Privada s/ bolsa integral",
                                              "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_piores_CN$Q047,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CN$Q047,enem_2016_piores_CN$regiao)[2,], sum(table(enem_2016_piores_CN$Q047,enem_2016_piores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CN$Q047,enem_2016_piores_CN$regiao)[3,], sum(table(enem_2016_piores_CN$Q047,enem_2016_piores_CN$regiao)[3,]))
dado4<-c(table(enem_2016_piores_CN$Q047,enem_2016_piores_CN$regiao)[4,], sum(table(enem_2016_piores_CN$Q047,enem_2016_piores_CN$regiao)[4,]))
dado5<-c(table(enem_2016_piores_CN$Q047,enem_2016_piores_CN$regiao)[5,], sum(table(enem_2016_piores_CN$Q047,enem_2016_piores_CN$regiao)[5,]))

dados_pCN_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_pCN_q047)[4] <- "Somente Esc. Pública"
colnames(dados_pCN_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_pCN_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_pCN_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_pCN_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_pCN_q047)[6] <- "Brasil total"
dados_pCN_q047[,4:8]<-round(100*(dados_pCN_q047[,4:8]/rowSums(dados_pCN_q047[,4:8])), digits = 1)

enem_2016_piores_CN$TP_COR_RACA =
  ifelse(enem_2016_piores_CN$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_piores_CN$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_piores_CN$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_piores_CN$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_piores_CN$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_piores_CN$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_piores_CN$TP_COR_RACA =
  factor(enem_2016_piores_CN$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                     'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[1,], sum(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[2,], sum(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[3,], sum(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[3,]))
dado4<-c(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[4,], sum(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[4,]))
dado5<-c(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[5,], sum(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[5,]))
dado6<-c(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[6,], sum(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[6,]))
dado7<-c(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[7,], sum(table(enem_2016_piores_CN$TP_COR_RACA,enem_2016_piores_CN$regiao)[7,]))

dados_pCN_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_pCN_r)[4] <- "Não declarado"
colnames(dados_pCN_r)[5] <- "Branca"
colnames(dados_pCN_r)[6] <- "Preta"
colnames(dados_pCN_r)[7] <- "Parda"
colnames(dados_pCN_r)[8] <- "Amarela"
colnames(dados_pCN_r)[9] <- "Indígena"
colnames(dados_pCN_r)[10] <- "Não dispõe da informação"
row.names(dados_pCN_r)[6] <- "Brasil total"
dados_pCN_r[,4:10]<-round(100*(dados_pCN_r[,4:10]/rowSums(dados_pCN_r[,4:10])), digits = 1)

enem_2016_piores_CN$TP_ESCOLA =
  ifelse(enem_2016_piores_CN$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_piores_CN$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_piores_CN$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_piores_CN$TP_ESCOLA =
  factor(enem_2016_piores_CN$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                   "Exterior"))
dado1<-c(table(enem_2016_piores_CN$TP_ESCOLA,enem_2016_piores_CN$regiao)[1,], sum(table(enem_2016_piores_CN$TP_ESCOLA,enem_2016_piores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CN$TP_ESCOLA,enem_2016_piores_CN$regiao)[2,], sum(table(enem_2016_piores_CN$TP_ESCOLA,enem_2016_piores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CN$TP_ESCOLA,enem_2016_piores_CN$regiao)[3,], sum(table(enem_2016_piores_CN$TP_ESCOLA,enem_2016_piores_CN$regiao)[3,]))
dado4<-c(table(enem_2016_piores_CN$TP_ESCOLA,enem_2016_piores_CN$regiao)[4,], sum(table(enem_2016_piores_CN$TP_ESCOLA,enem_2016_piores_CN$regiao)[4,]))

dados_pCN_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pCN_tpe)[4] <- "Não Respondeu"
colnames(dados_pCN_tpe)[5] <- "Pública"
colnames(dados_pCN_tpe)[6] <- "Privada"
colnames(dados_pCN_tpe)[7] <- "Exterior"
row.names(dados_pCN_tpe)[6] <- "Brasil total"
dados_pCN_tpe[,4:7]<-round(100*(dados_pCN_tpe[,4:7]/rowSums(dados_pCN_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_piores_CN$renda_SM =
  ifelse(enem_2016_piores_CN$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_piores_CN$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_piores_CN$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_piores_CN$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_piores_CN$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_piores_CN$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_piores_CN$renda_SM = factor(enem_2016_piores_CN$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                               "De um a dois Salários-Mínimos",
                                                                               "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                               "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[2,], sum(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[2,]))
dado2<-c(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[3,], sum(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[3,]))
dado3<-c(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[4,], sum(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[4,]))
dado4<-c(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[5,], sum(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[5,]))
dado5<-c(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[6,], sum(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[6,]))
dado6<-c(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[7,], sum(table(enem_2016_piores_CN$renda_SM,enem_2016_piores_CN$regiao)[7,]))

dados_pCN_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_pCN_sm)[4] <- "Nenhuma renda"
colnames(dados_pCN_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_pCN_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_pCN_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_pCN_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_pCN_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_pCN_sm)[6] <- "Brasil total"
dados_pCN_sm[,4:9]<-round(100*(dados_pCN_sm[,4:9]/rowSums(dados_pCN_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_piores_CN$Q049 =
  ifelse(enem_2016_piores_CN$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_piores_CN$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_piores_CN$Q049 =
  factor(enem_2016_piores_CN$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_piores_CN$Q049, enem_2016_piores_CN$regiao)[1,], sum(table(enem_2016_piores_CN$Q049,enem_2016_piores_CN$regiao)[1,]))
dado2<-c(table(enem_2016_piores_CN$Q049, enem_2016_piores_CN$regiao)[2,], sum(table(enem_2016_piores_CN$Q049,enem_2016_piores_CN$regiao)[2,]))
dado3<-c(table(enem_2016_piores_CN$Q049, enem_2016_piores_CN$regiao)[3,], sum(table(enem_2016_piores_CN$Q049,enem_2016_piores_CN$regiao)[3,]))

dados_pCN_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_pCN_tur)[4] <- "Somente no diurno"
colnames(dados_pCN_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_pCN_tur)[6] <- "Somente no noturno"
row.names(dados_pCN_tur)[6] <- "Brasil total"
dados_pCN_tur[,4:6]<-round(100*(dados_pCN_tur[,4:6]/rowSums(dados_pCN_tur[,4:6])), digits = 1)

#######
# Piores em LC
enem_2016_piores_LC<-read.csv("Enem_2016/piores_1000_LC.csv")

enem_2016_piores_LC$regiao =
  ifelse(enem_2016_piores_LC$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_piores_LC$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_piores_LC$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_piores_LC$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_piores_LC$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_piores_LC$regiao =
  factor(enem_2016_piores_LC$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_piores_LC$TP_SEXO,enem_2016_piores_LC$regiao)[1,], sum(table(enem_2016_piores_LC$TP_SEXO,enem_2016_piores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_piores_LC$TP_SEXO,enem_2016_piores_LC$regiao)[2,], sum(table(enem_2016_piores_LC$TP_SEXO,enem_2016_piores_LC$regiao)[2,]))

dados_pLC_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_pLC_s)[4] <- "Mulheres"
colnames(dados_pLC_s)[5] <- "Homens"
row.names(dados_pLC_s)[6] <- "Brasil total"
dados_pLC_s[,4:5]<-round(100*(dados_pLC_s[,4:5]/rowSums(dados_pLC_s[,4:5])), digits = 1)
#IDade

enem_2016_piores_LC$fx_idade =
  ifelse(enem_2016_piores_LC$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_piores_LC$NU_IDADE>=16 & enem_2016_piores_LC$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_piores_LC$NU_IDADE>=20 & enem_2016_piores_LC$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_piores_LC$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_piores_LC$fx_idade =
  factor(enem_2016_piores_LC$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                  "Mais de 25 anos"))
dado1<-c(table(enem_2016_piores_LC$fx_idade,enem_2016_piores_LC$regiao)[1,], sum(table(enem_2016_piores_LC$fx_idade,enem_2016_piores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_piores_LC$fx_idade,enem_2016_piores_LC$regiao)[2,], sum(table(enem_2016_piores_LC$fx_idade,enem_2016_piores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_piores_LC$fx_idade,enem_2016_piores_LC$regiao)[3,], sum(table(enem_2016_piores_LC$fx_idade,enem_2016_piores_LC$regiao)[3,]))
dado4<-c(table(enem_2016_piores_LC$fx_idade,enem_2016_piores_LC$regiao)[4,], sum(table(enem_2016_piores_LC$fx_idade,enem_2016_piores_LC$regiao)[4,]))

dados_pLC_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pLC_i)[4] <- "Menos de 16 anos"
colnames(dados_pLC_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_pLC_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_pLC_i)[7] <- "Mais de 25 anos"
row.names(dados_pLC_i)[6] <- "Brasil total"
dados_pLC_i[,4:7]<-round(100*(dados_pLC_i[,4:7]/rowSums(dados_pLC_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_piores_LC$Q047 =
  ifelse(enem_2016_piores_LC$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_piores_LC$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_piores_LC$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_piores_LC$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_piores_LC$Q047 =
  factor(enem_2016_piores_LC$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                              "Esc. Pública e Esc. Privada c/ bolsa integral",
                                              "Somente Esc. Privada s/ bolsa integral",
                                              "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_piores_LC$Q047,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_LC$Q047,enem_2016_piores_LC$regiao)[2,], sum(table(enem_2016_piores_LC$Q047,enem_2016_piores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_piores_LC$Q047,enem_2016_piores_LC$regiao)[3,], sum(table(enem_2016_piores_LC$Q047,enem_2016_piores_LC$regiao)[3,]))
dado4<-c(table(enem_2016_piores_LC$Q047,enem_2016_piores_LC$regiao)[4,], sum(table(enem_2016_piores_LC$Q047,enem_2016_piores_LC$regiao)[4,]))
dado5<-c(table(enem_2016_piores_LC$Q047,enem_2016_piores_LC$regiao)[5,], sum(table(enem_2016_piores_LC$Q047,enem_2016_piores_LC$regiao)[5,]))

dados_pLC_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_pLC_q047)[4] <- "Somente Esc. Pública"
colnames(dados_pLC_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_pLC_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_pLC_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_pLC_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_pLC_q047)[6] <- "Brasil total"
dados_pLC_q047[,4:8]<-round(100*(dados_pLC_q047[,4:8]/rowSums(dados_pLC_q047[,4:8])), digits = 1)

enem_2016_piores_LC$TP_COR_RACA =
  ifelse(enem_2016_piores_LC$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_piores_LC$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_piores_LC$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_piores_LC$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_piores_LC$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_piores_LC$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_piores_LC$TP_COR_RACA =
  factor(enem_2016_piores_LC$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                     'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[1,], sum(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[2,], sum(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[3,], sum(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[3,]))
dado4<-c(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[4,], sum(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[4,]))
dado5<-c(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[5,], sum(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[5,]))
dado6<-c(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[6,], sum(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[6,]))
dado7<-c(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[7,], sum(table(enem_2016_piores_LC$TP_COR_RACA,enem_2016_piores_LC$regiao)[7,]))

dados_pLC_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_pLC_r)[4] <- "Não declarado"
colnames(dados_pLC_r)[5] <- "Branca"
colnames(dados_pLC_r)[6] <- "Preta"
colnames(dados_pLC_r)[7] <- "Parda"
colnames(dados_pLC_r)[8] <- "Amarela"
colnames(dados_pLC_r)[9] <- "Indígena"
colnames(dados_pLC_r)[10] <- "Não dispõe da informação"
row.names(dados_pLC_r)[6] <- "Brasil total"
dados_pLC_r[,4:10]<-round(100*(dados_pLC_r[,4:10]/rowSums(dados_pLC_r[,4:10])), digits = 1)

enem_2016_piores_LC$TP_ESCOLA =
  ifelse(enem_2016_piores_LC$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_piores_LC$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_piores_LC$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_piores_LC$TP_ESCOLA =
  factor(enem_2016_piores_LC$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                   "Exterior"))
dado1<-c(table(enem_2016_piores_LC$TP_ESCOLA,enem_2016_piores_LC$regiao)[1,], sum(table(enem_2016_piores_LC$TP_ESCOLA,enem_2016_piores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_piores_LC$TP_ESCOLA,enem_2016_piores_LC$regiao)[2,], sum(table(enem_2016_piores_LC$TP_ESCOLA,enem_2016_piores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_piores_LC$TP_ESCOLA,enem_2016_piores_LC$regiao)[3,], sum(table(enem_2016_piores_LC$TP_ESCOLA,enem_2016_piores_LC$regiao)[3,]))
dado4<-c(table(enem_2016_piores_LC$TP_ESCOLA,enem_2016_piores_LC$regiao)[4,], sum(table(enem_2016_piores_LC$TP_ESCOLA,enem_2016_piores_LC$regiao)[4,]))

dados_pLC_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pLC_tpe)[4] <- "Não Respondeu"
colnames(dados_pLC_tpe)[5] <- "Pública"
colnames(dados_pLC_tpe)[6] <- "Privada"
colnames(dados_pLC_tpe)[7] <- "Exterior"
row.names(dados_pLC_tpe)[6] <- "Brasil total"
dados_pLC_tpe[,4:7]<-round(100*(dados_pLC_tpe[,4:7]/rowSums(dados_pLC_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_piores_LC$renda_SM =
  ifelse(enem_2016_piores_LC$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_piores_LC$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_piores_LC$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_piores_LC$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_piores_LC$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_piores_LC$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_piores_LC$renda_SM = factor(enem_2016_piores_LC$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                               "De um a dois Salários-Mínimos",
                                                                               "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                               "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[2,], sum(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[2,]))
dado2<-c(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[3,], sum(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[3,]))
dado3<-c(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[4,], sum(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[4,]))
dado4<-c(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[5,], sum(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[5,]))
dado5<-c(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[6,], sum(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[6,]))
dado6<-c(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[7,], sum(table(enem_2016_piores_LC$renda_SM,enem_2016_piores_LC$regiao)[7,]))

dados_pLC_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_pLC_sm)[4] <- "Nenhuma renda"
colnames(dados_pLC_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_pLC_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_pLC_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_pLC_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_pLC_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_pLC_sm)[6] <- "Brasil total"
dados_pLC_sm[,4:9]<-round(100*(dados_pLC_sm[,4:9]/rowSums(dados_pLC_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_piores_LC$Q049 =
  ifelse(enem_2016_piores_LC$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_piores_LC$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_piores_LC$Q049 =
  factor(enem_2016_piores_LC$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_piores_LC$Q049, enem_2016_piores_LC$regiao)[1,], sum(table(enem_2016_piores_LC$Q049,enem_2016_piores_LC$regiao)[1,]))
dado2<-c(table(enem_2016_piores_LC$Q049, enem_2016_piores_LC$regiao)[2,], sum(table(enem_2016_piores_LC$Q049,enem_2016_piores_LC$regiao)[2,]))
dado3<-c(table(enem_2016_piores_LC$Q049, enem_2016_piores_LC$regiao)[3,], sum(table(enem_2016_piores_LC$Q049,enem_2016_piores_LC$regiao)[3,]))

dados_pLC_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_pLC_tur)[4] <- "Somente no diurno"
colnames(dados_pLC_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_pLC_tur)[6] <- "Somente no noturno"
row.names(dados_pLC_tur)[6] <- "Brasil total"
dados_pLC_tur[,4:6]<-round(100*(dados_pLC_tur[,4:6]/rowSums(dados_pLC_tur[,4:6])), digits = 1)

#######
# piores em MT
enem_2016_piores_MT<-read.csv("Enem_2016/piores_1000_MT.csv")

enem_2016_piores_MT$regiao =
  ifelse(enem_2016_piores_MT$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_piores_MT$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_piores_MT$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_piores_MT$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_piores_MT$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_piores_MT$regiao =
  factor(enem_2016_piores_MT$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_piores_MT$TP_SEXO,enem_2016_piores_MT$regiao)[1,], sum(table(enem_2016_piores_MT$TP_SEXO,enem_2016_piores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_piores_MT$TP_SEXO,enem_2016_piores_MT$regiao)[2,], sum(table(enem_2016_piores_MT$TP_SEXO,enem_2016_piores_MT$regiao)[2,]))

dados_pMT_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_pMT_s)[4] <- "Mulheres"
colnames(dados_pMT_s)[5] <- "Homens"
row.names(dados_pMT_s)[6] <- "Brasil total"
dados_pMT_s[,4:5]<-round(100*(dados_pMT_s[,4:5]/rowSums(dados_pMT_s[,4:5])), digits = 1)
#IDade

enem_2016_piores_MT$fx_idade =
  ifelse(enem_2016_piores_MT$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_piores_MT$NU_IDADE>=16 & enem_2016_piores_MT$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_piores_MT$NU_IDADE>=20 & enem_2016_piores_MT$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_piores_MT$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_piores_MT$fx_idade =
  factor(enem_2016_piores_MT$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                  "Mais de 25 anos"))
dado1<-c(table(enem_2016_piores_MT$fx_idade,enem_2016_piores_MT$regiao)[1,], sum(table(enem_2016_piores_MT$fx_idade,enem_2016_piores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_piores_MT$fx_idade,enem_2016_piores_MT$regiao)[2,], sum(table(enem_2016_piores_MT$fx_idade,enem_2016_piores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_piores_MT$fx_idade,enem_2016_piores_MT$regiao)[3,], sum(table(enem_2016_piores_MT$fx_idade,enem_2016_piores_MT$regiao)[3,]))
dado4<-c(table(enem_2016_piores_MT$fx_idade,enem_2016_piores_MT$regiao)[4,], sum(table(enem_2016_piores_MT$fx_idade,enem_2016_piores_MT$regiao)[4,]))

dados_pMT_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pMT_i)[4] <- "Menos de 16 anos"
colnames(dados_pMT_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_pMT_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_pMT_i)[7] <- "Mais de 25 anos"
row.names(dados_pMT_i)[6] <- "Brasil total"
dados_pMT_i[,4:7]<-round(100*(dados_pMT_i[,4:7]/rowSums(dados_pMT_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_piores_MT$Q047 =
  ifelse(enem_2016_piores_MT$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_piores_MT$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_piores_MT$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_piores_MT$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_piores_MT$Q047 =
  factor(enem_2016_piores_MT$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                              "Esc. Pública e Esc. Privada c/ bolsa integral",
                                              "Somente Esc. Privada s/ bolsa integral",
                                              "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_piores_MT$Q047,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_MT$Q047,enem_2016_piores_MT$regiao)[2,], sum(table(enem_2016_piores_MT$Q047,enem_2016_piores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_piores_MT$Q047,enem_2016_piores_MT$regiao)[3,], sum(table(enem_2016_piores_MT$Q047,enem_2016_piores_MT$regiao)[3,]))
dado4<-c(table(enem_2016_piores_MT$Q047,enem_2016_piores_MT$regiao)[4,], sum(table(enem_2016_piores_MT$Q047,enem_2016_piores_MT$regiao)[4,]))
dado5<-c(table(enem_2016_piores_MT$Q047,enem_2016_piores_MT$regiao)[5,], sum(table(enem_2016_piores_MT$Q047,enem_2016_piores_MT$regiao)[5,]))

dados_pMT_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_pMT_q047)[4] <- "Somente Esc. Pública"
colnames(dados_pMT_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_pMT_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_pMT_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_pMT_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_pMT_q047)[6] <- "Brasil total"
dados_pMT_q047[,4:8]<-round(100*(dados_pMT_q047[,4:8]/rowSums(dados_pMT_q047[,4:8])), digits = 1)

enem_2016_piores_MT$TP_COR_RACA =
  ifelse(enem_2016_piores_MT$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_piores_MT$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_piores_MT$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_piores_MT$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_piores_MT$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_piores_MT$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_piores_MT$TP_COR_RACA =
  factor(enem_2016_piores_MT$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                     'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[1,], sum(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[2,], sum(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[3,], sum(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[3,]))
dado4<-c(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[4,], sum(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[4,]))
dado5<-c(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[5,], sum(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[5,]))
dado6<-c(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[6,], sum(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[6,]))
dado7<-c(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[7,], sum(table(enem_2016_piores_MT$TP_COR_RACA,enem_2016_piores_MT$regiao)[7,]))

dados_pMT_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_pMT_r)[4] <- "Não declarado"
colnames(dados_pMT_r)[5] <- "Branca"
colnames(dados_pMT_r)[6] <- "Preta"
colnames(dados_pMT_r)[7] <- "Parda"
colnames(dados_pMT_r)[8] <- "Amarela"
colnames(dados_pMT_r)[9] <- "Indígena"
colnames(dados_pMT_r)[10] <- "Não dispõe da informação"
row.names(dados_pMT_r)[6] <- "Brasil total"
dados_pMT_r[,4:10]<-round(100*(dados_pMT_r[,4:10]/rowSums(dados_pMT_r[,4:10])), digits = 1)

enem_2016_piores_MT$TP_ESCOLA =
  ifelse(enem_2016_piores_MT$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_piores_MT$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_piores_MT$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_piores_MT$TP_ESCOLA =
  factor(enem_2016_piores_MT$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                   "Exterior"))
dado1<-c(table(enem_2016_piores_MT$TP_ESCOLA,enem_2016_piores_MT$regiao)[1,], sum(table(enem_2016_piores_MT$TP_ESCOLA,enem_2016_piores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_piores_MT$TP_ESCOLA,enem_2016_piores_MT$regiao)[2,], sum(table(enem_2016_piores_MT$TP_ESCOLA,enem_2016_piores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_piores_MT$TP_ESCOLA,enem_2016_piores_MT$regiao)[3,], sum(table(enem_2016_piores_MT$TP_ESCOLA,enem_2016_piores_MT$regiao)[3,]))
dado4<-c(table(enem_2016_piores_MT$TP_ESCOLA,enem_2016_piores_MT$regiao)[4,], sum(table(enem_2016_piores_MT$TP_ESCOLA,enem_2016_piores_MT$regiao)[4,]))

dados_pMT_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pMT_tpe)[4] <- "Não Respondeu"
colnames(dados_pMT_tpe)[5] <- "Pública"
colnames(dados_pMT_tpe)[6] <- "Privada"
colnames(dados_pMT_tpe)[7] <- "Exterior"
row.names(dados_pMT_tpe)[6] <- "Brasil total"
dados_pMT_tpe[,4:7]<-round(100*(dados_pMT_tpe[,4:7]/rowSums(dados_pMT_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_piores_MT$renda_SM =
  ifelse(enem_2016_piores_MT$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_piores_MT$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_piores_MT$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_piores_MT$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_piores_MT$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_piores_MT$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_piores_MT$renda_SM = factor(enem_2016_piores_MT$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                               "De um a dois Salários-Mínimos",
                                                                               "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                               "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[2,], sum(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[2,]))
dado2<-c(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[3,], sum(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[3,]))
dado3<-c(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[4,], sum(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[4,]))
dado4<-c(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[5,], sum(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[5,]))
dado5<-c(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[6,], sum(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[6,]))
dado6<-c(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[7,], sum(table(enem_2016_piores_MT$renda_SM,enem_2016_piores_MT$regiao)[7,]))

dados_pMT_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_pMT_sm)[4] <- "Nenhuma renda"
colnames(dados_pMT_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_pMT_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_pMT_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_pMT_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_pMT_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_pMT_sm)[6] <- "Brasil total"
dados_pMT_sm[,4:9]<-round(100*(dados_pMT_sm[,4:9]/rowSums(dados_pMT_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_piores_MT$Q049 =
  ifelse(enem_2016_piores_MT$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_piores_MT$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_piores_MT$Q049 =
  factor(enem_2016_piores_MT$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_piores_MT$Q049, enem_2016_piores_MT$regiao)[1,], sum(table(enem_2016_piores_MT$Q049,enem_2016_piores_MT$regiao)[1,]))
dado2<-c(table(enem_2016_piores_MT$Q049, enem_2016_piores_MT$regiao)[2,], sum(table(enem_2016_piores_MT$Q049,enem_2016_piores_MT$regiao)[2,]))
dado3<-c(table(enem_2016_piores_MT$Q049, enem_2016_piores_MT$regiao)[3,], sum(table(enem_2016_piores_MT$Q049,enem_2016_piores_MT$regiao)[3,]))

dados_pMT_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_pMT_tur)[4] <- "Somente no diurno"
colnames(dados_pMT_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_pMT_tur)[6] <- "Somente no noturno"
row.names(dados_pMT_tur)[6] <- "Brasil total"
dados_pMT_tur[,4:6]<-round(100*(dados_pMT_tur[,4:6]/rowSums(dados_pMT_tur[,4:6])), digits = 1)

#######
# Piores em redaÃ§Ã£o
enem_2016_piores_RE<-read.csv("Enem_2016/piores_1000_REDACAO.csv")

enem_2016_piores_RE$regiao =
  ifelse(enem_2016_piores_RE$CO_UF_RESIDENCIA %in% reg_norte, "N",
         ifelse(enem_2016_piores_RE$CO_UF_RESIDENCIA %in% reg_nordeste, "NE",
                ifelse(enem_2016_piores_RE$CO_UF_RESIDENCIA %in% reg_centro_oeste, "CO",
                       ifelse(enem_2016_piores_RE$CO_UF_RESIDENCIA %in% reg_sudeste, "SE",
                              ifelse(enem_2016_piores_RE$CO_UF_RESIDENCIA %in% reg_sul, "S",
                                     "NÃÂ£o identificado")))))
enem_2016_piores_RE$regiao =
  factor(enem_2016_piores_RE$regiao, levels = c('S','SE','CO','NE','N'))
#Sexo
dado1<-c(table(enem_2016_piores_RE$TP_SEXO,enem_2016_piores_RE$regiao)[1,], sum(table(enem_2016_piores_RE$TP_SEXO,enem_2016_piores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_piores_RE$TP_SEXO,enem_2016_piores_RE$regiao)[2,], sum(table(enem_2016_piores_RE$TP_SEXO,enem_2016_piores_RE$regiao)[2,]))

dados_pRE_s<-data.frame(Regiao,Lat,Long,dado1,dado2)
colnames(dados_pRE_s)[4] <- "Mulheres"
colnames(dados_pRE_s)[5] <- "Homens"
row.names(dados_pRE_s)[6] <- "Brasil total"
dados_pRE_s[,4:5]<-round(100*(dados_pRE_s[,4:5]/rowSums(dados_pRE_s[,4:5])), digits = 1)
#IDade

enem_2016_piores_RE$fx_idade =
  ifelse(enem_2016_piores_RE$NU_IDADE<16,"Menos de 16 anos",
         ifelse(enem_2016_piores_RE$NU_IDADE>=16 & enem_2016_piores_RE$NU_IDADE<=19, "Entre 16 e 19 anos",
                ifelse(enem_2016_piores_RE$NU_IDADE>=20 & enem_2016_piores_RE$NU_IDADE<=25, "Entre 20 e 25 anos",
                       ifelse(enem_2016_piores_RE$NU_IDADE>25,"Mais de 25 anos", NA))))
enem_2016_piores_RE$fx_idade =
  factor(enem_2016_piores_RE$fx_idade, levels = c("Menos de 16 anos", "Entre 16 e 19 anos", "Entre 20 e 25 anos",
                                                  "Mais de 25 anos"))
dado1<-c(table(enem_2016_piores_RE$fx_idade,enem_2016_piores_RE$regiao)[1,], sum(table(enem_2016_piores_RE$fx_idade,enem_2016_piores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_piores_RE$fx_idade,enem_2016_piores_RE$regiao)[2,], sum(table(enem_2016_piores_RE$fx_idade,enem_2016_piores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_piores_RE$fx_idade,enem_2016_piores_RE$regiao)[3,], sum(table(enem_2016_piores_RE$fx_idade,enem_2016_piores_RE$regiao)[3,]))
dado4<-c(table(enem_2016_piores_RE$fx_idade,enem_2016_piores_RE$regiao)[4,], sum(table(enem_2016_piores_RE$fx_idade,enem_2016_piores_RE$regiao)[4,]))

dados_pRE_i<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pRE_i)[4] <- "Menos de 16 anos"
colnames(dados_pRE_i)[5] <- "Entre 16 e 19 anos"
colnames(dados_pRE_i)[6] <- "Entre 20 e 25 anos"
colnames(dados_pRE_i)[7] <- "Mais de 25 anos"
row.names(dados_pRE_i)[6] <- "Brasil total"
dados_pRE_i[,4:7]<-round(100*(dados_pRE_i[,4:7]/rowSums(dados_pRE_i[,4:7])), digits = 1)

# Tipo de escola
enem_2016_piores_RE$Q047 =
  ifelse(enem_2016_piores_RE$Q047 == "A", "Somente Esc. Pública",
         ifelse(enem_2016_piores_RE$Q047 == "B", "Esc. Pública e Esc. Privada s/ bolsa integral",
                ifelse(enem_2016_piores_RE$Q047 == "C", "Esc. Pública e Esc. Privada c/ bolsa integral",
                       ifelse(enem_2016_piores_RE$Q047 == "D", "Somente Esc. Privada s/ bolsa integral",
                              "Somente Esc. Privada c/ bolsa integral"))))
enem_2016_piores_RE$Q047 =
  factor(enem_2016_piores_RE$Q047, levels = c("Somente Esc. Pública","Esc. Pública e Esc. Privada s/ bolsa integral",
                                              "Esc. Pública e Esc. Privada c/ bolsa integral",
                                              "Somente Esc. Privada s/ bolsa integral",
                                              "Somente Esc. Privada c/ bolsa integral"))
dado1<-c(table(enem_2016_piores_RE$Q047,enem_2016_melhores_CH$regiao)[1,], sum(table(enem_2016_melhores_CH$Q047,enem_2016_melhores_CH$regiao)[1,]))
dado2<-c(table(enem_2016_piores_RE$Q047,enem_2016_piores_RE$regiao)[2,], sum(table(enem_2016_piores_RE$Q047,enem_2016_piores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_piores_RE$Q047,enem_2016_piores_RE$regiao)[3,], sum(table(enem_2016_piores_RE$Q047,enem_2016_piores_RE$regiao)[3,]))
dado4<-c(table(enem_2016_piores_RE$Q047,enem_2016_piores_RE$regiao)[4,], sum(table(enem_2016_piores_RE$Q047,enem_2016_piores_RE$regiao)[4,]))
dado5<-c(table(enem_2016_piores_RE$Q047,enem_2016_piores_RE$regiao)[5,], sum(table(enem_2016_piores_RE$Q047,enem_2016_piores_RE$regiao)[5,]))

dados_pRE_q047<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5)
colnames(dados_pRE_q047)[4] <- "Somente Esc. Pública"
colnames(dados_pRE_q047)[5] <- "Esc. Pública e Esc. Privada s/ bolsa integral"
colnames(dados_pRE_q047)[6] <- "Esc. Pública e Esc. Privada c/ bolsa integral"
colnames(dados_pRE_q047)[7] <- "Somente Esc. Privada s/ bolsa integral"
colnames(dados_pRE_q047)[8] <- "Somente Esc. Privada c/ bolsa integral"
row.names(dados_pRE_q047)[6] <- "Brasil total"
dados_pRE_q047[,4:8]<-round(100*(dados_pRE_q047[,4:8]/rowSums(dados_pRE_q047[,4:8])), digits = 1)

enem_2016_piores_RE$TP_COR_RACA =
  ifelse(enem_2016_piores_RE$TP_COR_RACA == 1, "Branca",
         ifelse(enem_2016_piores_RE$TP_COR_RACA == 2, "Preta",
                ifelse(enem_2016_piores_RE$TP_COR_RACA == 3, "Parda",
                       ifelse(enem_2016_piores_RE$TP_COR_RACA == 4, "Amarela",
                              ifelse(enem_2016_piores_RE$TP_COR_RACA == 5, "Indígena",
                                     ifelse(enem_2016_piores_RE$TP_COR_RACA == 6, "Não dispõe da informação",
                                            "Não declarado"))))))
enem_2016_piores_RE$TP_COR_RACA =
  factor(enem_2016_piores_RE$TP_COR_RACA, levels = c('Não declarado', 'Branca', 'Preta', 'Parda',
                                                     'Amarela', 'Indígena', 'Não dispõe da informação'))
dado1<-c(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[1,], sum(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[2,], sum(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[3,], sum(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[3,]))
dado4<-c(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[4,], sum(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[4,]))
dado5<-c(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[5,], sum(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[5,]))
dado6<-c(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[6,], sum(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[6,]))
dado7<-c(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[7,], sum(table(enem_2016_piores_RE$TP_COR_RACA,enem_2016_piores_RE$regiao)[7,]))

dados_pRE_r<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6,dado7)
colnames(dados_pRE_r)[4] <- "Não declarado"
colnames(dados_pRE_r)[5] <- "Branca"
colnames(dados_pRE_r)[6] <- "Preta"
colnames(dados_pRE_r)[7] <- "Parda"
colnames(dados_pRE_r)[8] <- "Amarela"
colnames(dados_pRE_r)[9] <- "Indígena"
colnames(dados_pRE_r)[10] <- "Não dispõe da informação"
row.names(dados_pRE_r)[6] <- "Brasil total"
dados_pRE_r[,4:10]<-round(100*(dados_pRE_r[,4:10]/rowSums(dados_pRE_r[,4:10])), digits = 1)

enem_2016_piores_RE$TP_ESCOLA =
  ifelse(enem_2016_piores_RE$TP_ESCOLA == 1, "Não Respondeu",
         ifelse(enem_2016_piores_RE$TP_ESCOLA == 2, "Pública",
                ifelse(enem_2016_piores_RE$TP_ESCOLA == 3, "Privada",
                       "Exterior")))
enem_2016_piores_RE$TP_ESCOLA =
  factor(enem_2016_piores_RE$TP_ESCOLA, levels = c("Não Respondeu","Pública","Privada",
                                                   "Exterior"))
dado1<-c(table(enem_2016_piores_RE$TP_ESCOLA,enem_2016_piores_RE$regiao)[1,], sum(table(enem_2016_piores_RE$TP_ESCOLA,enem_2016_piores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_piores_RE$TP_ESCOLA,enem_2016_piores_RE$regiao)[2,], sum(table(enem_2016_piores_RE$TP_ESCOLA,enem_2016_piores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_piores_RE$TP_ESCOLA,enem_2016_piores_RE$regiao)[3,], sum(table(enem_2016_piores_RE$TP_ESCOLA,enem_2016_piores_RE$regiao)[3,]))
dado4<-c(table(enem_2016_piores_RE$TP_ESCOLA,enem_2016_piores_RE$regiao)[4,], sum(table(enem_2016_piores_RE$TP_ESCOLA,enem_2016_piores_RE$regiao)[4,]))

dados_pRE_tpe<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4)
colnames(dados_pRE_tpe)[4] <- "Não Respondeu"
colnames(dados_pRE_tpe)[5] <- "Pública"
colnames(dados_pRE_tpe)[6] <- "Privada"
colnames(dados_pRE_tpe)[7] <- "Exterior"
row.names(dados_pRE_tpe)[6] <- "Brasil total"
dados_pRE_tpe[,4:7]<-round(100*(dados_pRE_tpe[,4:7]/rowSums(dados_pRE_tpe[,4:7])), digits = 1)

# MediÃ§Ã£o da renda a partir do salario mÃ?nimo (DivisÃ£o a partir do PNDA 2015 do IBGE, desconsiderando as fraÃ§Ãµes de salario mÃ?nimo.)
enem_2016_piores_RE$renda_SM =
  ifelse(enem_2016_piores_RE$Q006 == "A", "Nenhuma renda",
         ifelse(enem_2016_piores_RE$Q006 == "B", "Até um Salário-Mínimo",
                ifelse(enem_2016_piores_RE$Q006 %in% dois_SM, "De um a dois Salários-Mínimos",
                       ifelse(enem_2016_piores_RE$Q006 %in% tres_SM, "De dois a três Salários-Mínimos",
                              ifelse(enem_2016_piores_RE$Q006 %in% cinco_SM, "De três a cinco Salários-Mínimos",
                                     ifelse(enem_2016_piores_RE$Q006 %in% acima_cinco_SM, "Acima de cinco Salários-Mínimos", "NÃ£o identificado"))))))


enem_2016_piores_RE$renda_SM = factor(enem_2016_piores_RE$renda_SM, levels = c("NÃ£o identificado" ,"Nenhuma renda","Até um Salário-Mínimo",
                                                                               "De um a dois Salários-Mínimos",
                                                                               "De dois a três Salários-Mínimos", "De três a cinco Salários-Mínimos",
                                                                               "Acima de cinco Salários-Mínimos"))
dado1<-c(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[2,], sum(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[2,]))
dado2<-c(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[3,], sum(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[3,]))
dado3<-c(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[4,], sum(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[4,]))
dado4<-c(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[5,], sum(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[5,]))
dado5<-c(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[6,], sum(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[6,]))
dado6<-c(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[7,], sum(table(enem_2016_piores_RE$renda_SM,enem_2016_piores_RE$regiao)[7,]))

dados_pRE_sm<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3,dado4,dado5,dado6)
colnames(dados_pRE_sm)[4] <- "Nenhuma renda"
colnames(dados_pRE_sm)[5] <- "Até um Salário-Mínimo"
colnames(dados_pRE_sm)[6] <- "De um a dois Salários-Mínimos"
colnames(dados_pRE_sm)[7] <- "De dois a três Salários-Mínimos"
colnames(dados_pRE_sm)[8] <- "De três a cinco Salários-Mínimos"
colnames(dados_pRE_sm)[9] <- "Acima de cinco Salários-Mínimos"
row.names(dados_pRE_sm)[6] <- "Brasil total"
dados_pRE_sm[,4:9]<-round(100*(dados_pRE_sm[,4:9]/rowSums(dados_pRE_sm[,4:9])), digits = 1)

# Turno EM
enem_2016_piores_RE$Q049 =
  ifelse(enem_2016_piores_RE$Q049 == "A", "Somente no diurno",
         ifelse(enem_2016_piores_RE$Q049 == "B", "Parte no diurno e parte no noturno", "Somente no noturno"))

enem_2016_piores_RE$Q049 =
  factor(enem_2016_piores_RE$Q049, levels = c("Somente no diurno", "Parte no diurno e parte no noturno", "Somente no noturno"))

dado1<-c(table(enem_2016_piores_RE$Q049, enem_2016_piores_RE$regiao)[1,], sum(table(enem_2016_piores_RE$Q049,enem_2016_piores_RE$regiao)[1,]))
dado2<-c(table(enem_2016_piores_RE$Q049, enem_2016_piores_RE$regiao)[2,], sum(table(enem_2016_piores_RE$Q049,enem_2016_piores_RE$regiao)[2,]))
dado3<-c(table(enem_2016_piores_RE$Q049, enem_2016_piores_RE$regiao)[3,], sum(table(enem_2016_piores_RE$Q049,enem_2016_piores_RE$regiao)[3,]))

dados_pRE_tur<-data.frame(Regiao,Lat,Long,dado1,dado2,dado3)
colnames(dados_pRE_tur)[4] <- "Somente no diurno"
colnames(dados_pRE_tur)[5] <- "Parte no diurno e parte no noturno"
colnames(dados_pRE_tur)[6] <- "Somente no noturno"
row.names(dados_pRE_tur)[6] <- "Brasil total"
dados_pRE_tur[,4:6]<-round(100*(dados_pRE_tur[,4:6]/rowSums(dados_pRE_tur[,4:6])), digits = 1)

##############################
#############APP##############
##############################

ui <- fluidPage(theme = shinytheme("journal" ),
                
                titlePanel("Aplicação em shiny: Intersecção entre gênero, classe e raça no ENEM de 2016"),
                sidebarLayout(position = "right",
                              sidebarPanel(conditionalPanel(condition = "input.navbar != 'rank1' &
                                                            input.navbar != 'aut' & input.navbar != 'art' & input.navbar != 'sob' ", # Por enquanto só funciona na força bruta.
                                                            selectInput(inputId = "banco",label = "Selecione o banco de dados",
                                                                        choices = c('Quem presta o ENEM ?'='geral','Quem são os melhores ?'='melhores',
                                                                                    'Quem são os piores ?'='piores')),
                                                            radioButtons(inputId = "var", label = "Selecione a variável",
                                                                         choices = c("Sexo","Idade", "Raça/Cor autodeclarada"="Cor autodeclarada",
                                                                                     "Renda",
                                                                                     "Tipo de escola frequentada no EM",
                                                                                     "Turno do EM")),
                                                            conditionalPanel(
                                                              condition = "input.banco != 'geral'",
                                                              radioButtons("hab", "Selecione o tipo de habilidade",
                                                                           choices = c("Performance Geral"="Performance Geral",# Resultado geral
                                                                                       "Ciências Naturais" = "Ciencias Naturais", "Ciências Humanas"  = "Ciencias Humanas",
                                                                                       "Linguagens e Códigos" = "Linguagens e Codigos", "Matemática" = "Matematica",
                                                                                       "Redação" = "Redacao"))
                                                            ),conditionalPanel(
                                                              condition = "input.navbar == 'anbi'",
                                                              checkboxGroupInput("var2", "Selecione uma segunda variável",
                                                                           choices = c("Sexo","Idade", "Raça/Cor autodeclarada"="Cor autodeclarada",
                                                                                       "Renda",
                                                                                       "Tipo de escola frequentada no EM",
                                                                                       "Turno do EM"))
                                                            )),br("Realização :"),tags$br(),
                                           a(href= "http://www.led.ufba.br/",img(src="LED-Logo1.png", height = 120*0.8, width = 380*0.8), target="_blank")
                              ),
                              mainPanel(
                                navbarPage(title = "ENEM 2016",id="navbar",
                                           tabPanel(title = "Mapa",align="center",
                                                    leafletOutput("mymap", height = 540, width = 580) %>% withSpinner(color = "#679941",type = 3,
                                                                                                                      color.background = "white"),
                                                    br() , tableOutput("t_dados")),
                                           tabPanel(title = "Desempenho",
                                                    plotlyOutput("des")),
                                           tabPanel(title = "Análise bivariada",value = "anbi",align="center",
                                                    plotlyOutput("box"),br(),
                                                    tableOutput("t_dados1")),
                                           tabPanel(title = "Ranking dos municípios",align="left",value = "rank1",splitLayout(
                                             div(tableOutput("rank"), style = "font-size:80%"),verticalLayout(
                                               radioButtons("updown", "Selecione entre melhores cidades e piores cidades",
                                                            choices = c("Melhores","Piores")), radioButtons("area", "Selecione a área do conhecimento",
                                                                                                            choices = c("Geral"="Geral",# Resultado geral
                                                                                                                        "Ciências Naturais" = "Ciencias Naturais", "Ciências Humanas"  = "Ciencias Humanas",
                                                                                                                        "Linguagens e Códigos" = "Linguagens e Codigos", "Matemática" = "Matematica",
                                                                                                                        "Redação" = "Redacao")))),br(),
                                             splitLayout(verticalLayout(h3("Descubra a performance da sua cidade."),
                                                                        textInput(inputId = "nomcid",label = "Insira aqui o nome de sua cidade")),
                                                         tableOutput("t_dados2"))),
                                           navbarMenu(title = "Mais",
                                                      tabPanel(title = "Artigo",h1("Em construção"),value = "art"),
                                                      tabPanel(title = "Autores",br(),value = "aut",
                                                               h4("ARTHUR RIOS DE AZEVEDO - Instituto de Matemática e Estatística - UFBA"),
                                                               a(href="","arthur.rios.az@hotmail.com"),br(),br(),
                                                               h4("ANDERSON ARA - Instituto de Matemática e Estatística - UFBA"),
                                                               a(href="","anderson.ara@ufba.br"),br(),br(),
                                                               h4("MARIANA YUKARI NOGUTI - Ministério Público do Paraná - MPPR"),
                                                               a(href="","mynoguti@mppr.mp.br"),br(),br(),
                                                               h4("ANGELA ERNESTINA CARDOSO DE BRITO - Instituto de Psicologia - UFBA"),
                                                               a(href="","angelafro@yahoo.com.br")),
                                                      tabPanel(title = "Sobre",value = "sob",h1("Sobre:") , textOutput('sob'),br(),textOutput('sob1'),br(),textOutput('sob2')))
                                           
                                )
                              )
                ))
server <- function(input, output,session) {
  output$t_dados2 <- renderTable({
    sua_cida %>% 
      filter(MUNICIPIO==input$nomcid)
  })
  output$t_dados1 <- renderTable(rownames = T,{
    data2 <- switch(input$banco,
                    'geral' = switch(input$var,
                                     'Sexo' = dados_g_s[,-c(1,2,3)],
                                     'Idade' = dados_g_i[,-c(1,2,3)],
                                     'Cor autodeclarada' = dados_g_r[,-c(1,2,3)],
                                     
                                     'Renda' = dados_g_sm[,-c(1,2,3)],
                                     'Tipo de escola frequentada no EM' = dados_g_q047[,-c(1,2,3)],
                                     'Turno do EM' = dados_g_tur[,-c(1,2,3)]) ,
                    'melhores' = switch(input$hab,
                                        'Performance Geral' = switch(input$var,
                                                                     'Sexo' = dados_m_s[,-c(1,2,3)],
                                                                     'Idade' = dados_m_i[,-c(1,2,3)],
                                                                     'Cor autodeclarada' = dados_m_r[,-c(1,2,3)],
                                                                     
                                                                     'Renda' = dados_m_sm[,-c(1,2,3)],
                                                                     'Tipo de escola frequentada no EM' = dados_m_q047[,-c(1,2,3)],
                                                                     'Turno do EM' = dados_m_tur[,-c(1,2,3)]),
                                        'Ciencias Naturais' = switch(input$var,#tem
                                                                     'Sexo' = dados_mCN_s[,-c(1,2,3)],
                                                                     'Idade' = dados_mCN_i[,-c(1,2,3)],
                                                                     'Cor autodeclarada' = dados_mCN_r[,-c(1,2,3)],
                                                                     
                                                                     'Renda' = dados_mCN_sm[,-c(1,2,3)],
                                                                     'Tipo de escola frequentada no EM' = dados_mCN_q047[,-c(1,2,3)],
                                                                     'Turno do EM' = dados_mCN_tur[,-c(1,2,3)]),
                                        'Ciencias Humanas' = switch(input$var,
                                                                    'Sexo' = dados_mCH_s[,-c(1,2,3)],
                                                                    'Idade' = dados_mCH_i[,-c(1,2,3)],
                                                                    'Cor autodeclarada' = dados_mCH_r[,-c(1,2,3)],
                                                                    
                                                                    'Renda' = dados_mCH_sm[,-c(1,2,3)],
                                                                    'Tipo de escola frequentada no EM' = dados_mCH_q047[,-c(1,2,3)],
                                                                    'Turno do EM' = dados_mCN_tur[,-c(1,2,3)]),
                                        'Linguagens e Codigos' = switch(input$var,
                                                                        'Sexo' = dados_mLC_s[,-c(1,2,3)],
                                                                        'Idade' = dados_mLC_i[,-c(1,2,3)],
                                                                        'Cor autodeclarada' = dados_mLC_r[,-c(1,2,3)],
                                                                        
                                                                        'Renda' = dados_mLC_sm[,-c(1,2,3)],
                                                                        'Tipo de escola frequentada no EM' = dados_mLC_q047[,-c(1,2,3)],
                                                                        'Turno do EM' = dados_mCN_tur[,-c(1,2,3)]),
                                        'Matematica' = switch(input$var,
                                                              'Sexo' = dados_mMT_s[,-c(1,2,3)],
                                                              'Idade' = dados_mMT_i[,-c(1,2,3)],
                                                              'Cor autodeclarada' = dados_mMT_r[,-c(1,2,3)],
                                                              
                                                              'Renda' = dados_mMT_sm[,-c(1,2,3)],
                                                              'Tipo de escola frequentada no EM' = dados_mMT_q047[,-c(1,2,3)],
                                                              'Turno do EM' = dados_mMT_tur[,-c(1,2,3)]),
                                        'Redacao' = switch(input$var,
                                                           'Sexo' = dados_mRE_s[,-c(1,2,3)],
                                                           'Idade' = dados_mRE_i[,-c(1,2,3)],
                                                           'Cor autodeclarada' = dados_mRE_r[,-c(1,2,3)],
                                                           
                                                           'Renda' = dados_mRE_sm[,-c(1,2,3)],
                                                           'Tipo de escola frequentada no EM' = dados_mRE_q047[,-c(1,2,3)],
                                                           'Turno do EM' = dados_mRE_tur[,-c(1,2,3)])
                    ),
                    'piores' = switch(input$hab,
                                      'Performance Geral' = switch(input$var,
                                                                   'Sexo' = dados_p_s[,-c(1,2,3)],
                                                                   'Idade' = dados_p_i[,-c(1,2,3)],
                                                                   'Cor autodeclarada' = dados_p_r[,-c(1,2,3)],
                                                                   
                                                                   'Renda' = dados_p_sm[,-c(1,2,3)],
                                                                   'Tipo de escola frequentada no EM' = dados_p_q047[,-c(1,2,3)],
                                                                   'Turno do EM' = dados_p_tur[,-c(1,2,3)]),
                                      'Ciencias Naturais' = switch(input$var,
                                                                   'Sexo' = dados_pCN_s[,-c(1,2,3)],
                                                                   'Idade' = dados_pCN_i[,-c(1,2,3)],
                                                                   'Cor autodeclarada' = dados_pCN_r[,-c(1,2,3)],
                                                                   
                                                                   'Renda' = dados_pCN_sm[,-c(1,2,3)],
                                                                   'Tipo de escola frequentada no EM' = dados_pCN_q047[,-c(1,2,3)],
                                                                   'Turno do EM' = dados_pCN_tur[,-c(1,2,3)]),
                                      'Ciencias Humanas' = switch(input$var,
                                                                  'Sexo' = dados_pCH_s[,-c(1,2,3)],
                                                                  'Idade' = dados_pCH_i[,-c(1,2,3)],
                                                                  'Cor autodeclarada' = dados_pCH_r[,-c(1,2,3)],
                                                                  
                                                                  'Renda' = dados_pCH_sm[,-c(1,2,3)],
                                                                  'Tipo de escola frequentada no EM' = dados_pCH_q047[,-c(1,2,3)],
                                                                  'Turno do EM' = dados_pCH_tur[,-c(1,2,3)]),
                                      'Linguagens e Codigos' = switch(input$var,
                                                                      'Sexo' = dados_pLC_s[,-c(1,2,3)],
                                                                      'Idade' = dados_pLC_i[,-c(1,2,3)],
                                                                      'Cor autodeclarada' = dados_pLC_r[,-c(1,2,3)],
                                                                      
                                                                      'Renda' = dados_pLC_sm[,-c(1,2,3)],
                                                                      'Tipo de escola frequentada no EM' = dados_pLC_q047[,-c(1,2,3)],
                                                                      'Turno do EM' = dados_pLC_tur[,-c(1,2,3)]),
                                      'Matematica' = switch(input$var,
                                                            'Sexo' = dados_pMT_s[,-c(1,2,3)],
                                                            'Idade' = dados_pMT_i[,-c(1,2,3)],
                                                            'Cor autodeclarada' = dados_pMT_r[,-c(1,2,3)],
                                                            
                                                            'Renda' = dados_pMT_sm[,-c(1,2,3)],
                                                            'Tipo de escola frequentada no EM' = dados_pMT_q047[,-c(1,2,3)],
                                                            'Turno do EM' = dados_pMT_tur[,-c(1,2,3)]),
                                      'Redacao' = switch(input$var,
                                                         'Sexo' = dados_pRE_s[,-c(1,2,3)],
                                                         'Idade' = dados_pRE_i[,-c(1,2,3)],
                                                         'Cor autodeclarada' = dados_pRE_r[,-c(1,2,3)],
                                                         
                                                         'Renda' = dados_pRE_sm[,-c(1,2,3)],
                                                         'Tipo de escola frequentada no EM' = dados_pRE_q047[,-c(1,2,3)],
                                                         'Turno do EM' = dados_pRE_tur[,-c(1,2,3)])
                    )
    )
    data2
  })
  output$rank <- renderTable(rownames = F,bordered = T,{
    rank_tabe <- switch(input$updown,
                        'Melhores' = switch(input$area,
                                            'Geral' = muni_top_mdfin[,-c(4:8)],
                                            'Ciencias Naturais' = muni_top_mdCN[,-c(3,5:8)],
                                            'Ciencias Humanas' = muni_top_mdCH[,-c(3,4,6:8)],
                                            'Linguagens e Codigos' = muni_top_mdLC[,-c(3:5,7:8)],
                                            'Matematica' = muni_top_mdMT[,-c(3:6,8)],
                                            'Redacao' = muni_top_mdRE[,-c(3:7)]),
                        'Piores' = switch(input$area,
                                          'Geral' = muni_low_mdfin[,-c(4:8)],
                                          'Ciencias Naturais' = muni_low_mdCN[,-c(3,5:8)],
                                          'Ciencias Humanas' = muni_low_mdCH[,-c(3,4,6:8)],
                                          'Linguagens e Codigos' = muni_low_mdLC[,-c(3:5,7:8)],
                                          'Matematica' = muni_low_mdMT[,-c(3:6,8)],
                                          'Redacao' = muni_low_mdRE[,-c(3:7)]))
  })
  output$box <- renderPlotly({
    data3 <- switch(input$banco,
                    'geral' = enem_2016_30mil,
                    'melhores' = switch(input$hab,
                                        "Performance Geral" = enem_2016_mil_melhores,
                                        "Ciencias Naturais" = enem_2016_melhores_CN,
                                        "Ciencias Humanas" = enem_2016_melhores_CH,
                                        "Linguagens e Codigos" = enem_2016_melhores_LC,
                                        "Matematica" = enem_2016_melhores_MT,
                                        "Redacao" = enem_2016_melhores_RE),
                    'piores' = switch(input$hab,
                                      "Performance Geral" = enem_2016_mil_piores,
                                      "Ciencias Naturais" = enem_2016_piores_CN,
                                      "Ciencias Humanas" = enem_2016_piores_CH,
                                      "Linguagens e Codigos" = enem_2016_piores_LC,
                                      "Matematica" = enem_2016_melhores_MT,
                                      "Redacao" = enem_2016_melhores_RE))
    data4 <- switch(input$banco,
                    'geral' = switch(input$hab,
                                     'Performance Geral' = enem_2016_30mil$NOTA_FINAL_ENEM/5,
                                     'Ciencias Naturais' = enem_2016_30mil$NU_NOTA_CN,
                                     'Ciencias Humanas' = enem_2016_30mil$NU_NOTA_CH,
                                     'Linguagens e Codigos' = enem_2016_30mil$NU_NOTA_LC,
                                     'Matematica' = enem_2016_30mil$NU_NOTA_MT,
                                     'Redacao' = enem_2016_30mil$NU_NOTA_REDACAO),
                    'melhores' = switch(input$hab,
                                        'Performance Geral' = enem_2016_mil_melhores$NOTA_FINAL_ENEM/5,
                                        'Ciencias Naturais' = enem_2016_melhores_CN$NU_NOTA_CN,
                                        'Ciencias Humanas' = enem_2016_melhores_CH$NU_NOTA_CH,
                                        'Linguagens e Codigos' = enem_2016_melhores_LC$NU_NOTA_LC,
                                        'Matematica' = enem_2016_melhores_MT$NU_NOTA_MT,
                                        'Redacao' = enem_2016_melhores_RE$NU_NOTA_REDACAO),
                    'piores' = switch(input$hab,
                                      'Performance Geral' = enem_2016_mil_piores$NOTA_FINAL_ENEM/5,
                                      'Ciencias Naturais' = enem_2016_piores_CN$NU_NOTA_CN,
                                      'Ciencias Humanas' = enem_2016_piores_CH$NU_NOTA_CH,
                                      'Linguagens e Codigos' = enem_2016_piores_LC$NU_NOTA_LC,
                                      'Matematica' = enem_2016_piores_MT$NU_NOTA_MT,
                                      'Redacao' = enem_2016_piores_RE$NU_NOTA_REDACAO))
    data5 <- switch(input$banco,
                    'geral' = switch(input$var,
                                     'Sexo' = enem_2016_30mil$TP_SEXO,
                                     'Idade' = enem_2016_30mil$fx_idade,
                                     'Cor autodeclarada' = enem_2016_30mil$TP_COR_RACA,
                                     'Renda' = enem_2016_30mil$renda_SM,
                                     'Tipo de escola frequentada no EM' = enem_2016_30mil$Q047,
                                     'Turno do EM' = enem_2016_30mil$Q049),
                    'melhores' = switch(input$hab,
                                        'Performance Geral' = switch(input$var,
                                                                     'Sexo' = enem_2016_mil_melhores$TP_SEXO,
                                                                     'Idade' = enem_2016_mil_melhores$fx_idade,
                                                                     'Cor autodeclarada' = enem_2016_mil_melhores$TP_COR_RACA,
                                                                     'Renda' = enem_2016_mil_melhores$renda_SM,
                                                                     'Tipo de escola frequentada no EM' = enem_2016_mil_melhores$Q047,
                                                                     'Turno do EM' = enem_2016_mil_melhores$Q049),
                                        'Ciencias Naturais' = switch(input$var,
                                                                     'Sexo' = enem_2016_melhores_CN$TP_SEXO,
                                                                     'Idade' = enem_2016_melhores_CN$fx_idade,
                                                                     'Cor autodeclarada' = enem_2016_melhores_CN$TP_COR_RACA,
                                                                     'Renda' = enem_2016_melhores_CN$renda_SM,
                                                                     'Tipo de escola frequentada no EM' = enem_2016_melhores_CN$Q047,
                                                                     'Turno do EM' = enem_2016_melhores_CN$Q049),
                                        'Ciencias Humanas' = switch(input$var,
                                                                    'Sexo' = enem_2016_melhores_CH$TP_SEXO,
                                                                    'Idade' = enem_2016_melhores_CH$fx_idade,
                                                                    'Cor autodeclarada' = enem_2016_melhores_CH$TP_COR_RACA,
                                                                    'Renda' = enem_2016_melhores_CH$renda_SM,
                                                                    'Tipo de escola frequentada no EM' = enem_2016_melhores_CH$Q047,
                                                                    'Turno do EM' = enem_2016_melhores_CH$Q049),
                                        'Linguagens e Codigos' = switch(input$var,
                                                                        'Sexo' = enem_2016_melhores_LC$TP_SEXO,
                                                                        'Idade' = enem_2016_melhores_LC$fx_idade,
                                                                        'Cor autodeclarada' = enem_2016_melhores_LC$TP_COR_RACA,
                                                                        'Renda' = enem_2016_melhores_LC$renda_SM,
                                                                        'Tipo de escola frequentada no EM' = enem_2016_melhores_LC$Q047,
                                                                        'Turno do EM' = enem_2016_melhores_LC$Q049),
                                        'Matematica' = switch(input$var,
                                                              'Sexo' = enem_2016_melhores_MT$TP_SEXO,
                                                              'Idade' = enem_2016_melhores_MT$fx_idade,
                                                              'Cor autodeclarada' = enem_2016_melhores_MT$TP_COR_RACA,
                                                              'Renda' = enem_2016_melhores_MT$renda_SM,
                                                              'Tipo de escola frequentada no EM' = enem_2016_melhores_MT$Q047,
                                                              'Turno do EM' = enem_2016_melhores_MT$Q049),
                                        'Redacao' = switch(input$var,
                                                           'Sexo' = enem_2016_melhores_RE$TP_SEXO,
                                                           'Idade' = enem_2016_melhores_RE$fx_idade,
                                                           'Cor autodeclarada' = enem_2016_melhores_RE$TP_COR_RACA,
                                                           'Renda' = enem_2016_melhores_RE$renda_SM,
                                                           'Tipo de escola frequentada no EM' = enem_2016_melhores_RE$Q047,
                                                           'Turno do EM' = enem_2016_melhores_RE$Q049)),
                    'piores' = switch(input$hab,
                                      'Performance Geral' = switch(input$var,
                                                                   'Sexo' = enem_2016_mil_piores$TP_SEXO,
                                                                   'Idade' = enem_2016_mil_piores$fx_idade,
                                                                   'Cor autodeclarada' = enem_2016_mil_piores$TP_COR_RACA,
                                                                   'Renda' = enem_2016_mil_piores$renda_SM,
                                                                   'Tipo de escola frequentada no EM' = enem_2016_mil_piores$Q047,
                                                                   'Turno do EM' = enem_2016_mil_piores$Q049),
                                      'Ciencias Naturais' = switch(input$var,
                                                                   'Sexo' = enem_2016_piores_CN$TP_SEXO,
                                                                   'Idade' = enem_2016_piores_CN$fx_idade,
                                                                   'Cor autodeclarada' = enem_2016_piores_CN$TP_COR_RACA,
                                                                   'Renda' = enem_2016_piores_CN$renda_SM,
                                                                   'Tipo de escola frequentada no EM' = enem_2016_piores_CN$Q047,
                                                                   'Turno do EM' = enem_2016_piores_CN$Q049),
                                      'Ciencias Humanas' = switch(input$var,
                                                                  'Sexo' = enem_2016_piores_CH$TP_SEXO,
                                                                  'Idade' = enem_2016_piores_CH$fx_idade,
                                                                  'Cor autodeclarada' = enem_2016_piores_CH$TP_COR_RACA,
                                                                  'Renda' = enem_2016_piores_CH$renda_SM,
                                                                  'Tipo de escola frequentada no EM' = enem_2016_piores_CH$Q047,
                                                                  'Turno do EM' = enem_2016_piores_CH$Q049),
                                      'Linguagens e Codigos' = switch(input$var,
                                                                      'Sexo' = enem_2016_piores_LC$TP_SEXO,
                                                                      'Idade' = enem_2016_piores_LC$fx_idade,
                                                                      'Cor autodeclarada' = enem_2016_piores_LC$TP_COR_RACA,
                                                                      'Renda' = enem_2016_piores_LC$renda_SM,
                                                                      'Tipo de escola frequentada no EM' = enem_2016_piores_LC$Q047,
                                                                      'Turno do EM' = enem_2016_piores_LC$Q049),
                                      'Matematica' = switch(input$var,
                                                            'Sexo' = enem_2016_piores_MT$TP_SEXO,
                                                            'Idade' = enem_2016_piores_MT$fx_idade,
                                                            'Cor autodeclarada' = enem_2016_piores_MT$TP_COR_RACA,
                                                            'Renda' = enem_2016_piores_MT$renda_SM,
                                                            'Tipo de escola frequentada no EM' = enem_2016_piores_MT$Q047,
                                                            'Turno do EM' = enem_2016_piores_MT$Q049),
                                      'Redacao' = switch(input$var,
                                                         'Sexo' = enem_2016_piores_RE$TP_SEXO,
                                                         'Idade' = enem_2016_piores_RE$fx_idade,
                                                         'Cor autodeclarada' = enem_2016_piores_RE$TP_COR_RACA,
                                                         'Renda' = enem_2016_piores_RE$renda_SM,
                                                         'Tipo de escola frequentada no EM' = enem_2016_piores_RE$Q047,
                                                         'Turno do EM' = enem_2016_piores_RE$Q049)))
    
    
    b <- ggplot(data = data3, aes(x = data5, y = data4))+
      geom_boxplot(fill="royalblue2")+
      labs(x=NULL,y=switch(input$banco,
                           'geral' = 'Nota Geral',
                           'melhores' = switch(input$hab,
                                               'Performance Geral' = 'Nota Geral dos Melhores',
                                               'Ciencias Naturais' = 'Nota em Ciências Naturais',
                                               'Ciencias Humanas' = 'Nota em Ciências Humanas',
                                               'Linguagens e Codigos' = 'Nota em Linguagens e Códigos',
                                               'Matematica' = 'Nota em Matemática',
                                               'Redacao' = 'Nota em Redação'),
                           'piores' = switch(input$hab,
                                             'Performance Geral' = 'Nota Geral dos Piores',
                                             'Ciencias Naturais' = 'Nota em Ciências Naturais',
                                             'Ciencias Humanas' = 'Nota em Ciências Humanas',
                                             'Linguagens e Codigos' = 'Nota em Linguagens e Códigos',
                                             'Matematica' = 'Nota em Matemática',
                                             'Redacao' = 'Nota em Redação')))+
      scale_x_discrete()+
      theme_bw()+
      theme(
        axis.text.x =switch(input$var, 
                            "Renda" = element_text(angle = -90, hjust = 0,vjust = 1),
                            "Tipo de escola frequentada no EM"=element_text(angle = -45, hjust = 0,vjust = 0),
                            "Sexo" = element_text(angle = 0, hjust = 0,vjust = 1),
                            "Turno do EM" = element_text(angle = 0, hjust = 0,vjust = 1),
                            "Cor autodeclarada"=element_text(angle = 0, hjust = 0,vjust = 1 ),
                            "Idade"=element_text(angle = 0, hjust = 0,vjust = 1)))
    ggplotly(b)
  })
  output$sob <- renderText("Essa aplicação foi produzida para melhor expor o notável ciclo de desigualdade que o Brasil possui, que não é exclusiva da contemporaneidade e é uma possibilidade de futuro com desigualdade de renda, gênero e etnia. Os jovens que prestam o Exame Nacional do Ensino Médio (ENEM) representam parte do futuro do país, mais de 8 milhões de candidatos realizaram a prova no ano de 2016.")
  output$sob1 <- renderText("O website está em contínuo desenvolvimento, no qual será implementado outras análises de dados no sentido de sumarizar ainda mais os pontos mais críticos da análise, bem como aguarda a divulgação de novos dados pelo INEP, relativos a edições posteriores do ENEM.")
  output$sob2 <- renderText("Nas abas 'Desempenho' e 'Análise estatística bivariada' a Nota geral corresponde a média aritimética de todas as provas, desconsiderando qualquer peso especifico para alguma habilidade")
  output$des <- renderPlotly({
    data3 <- switch(input$banco,
                    'geral' = enem_2016_30mil,
                    'melhores' = switch(input$hab,
                                        "Performance Geral" = enem_2016_mil_melhores,
                                        "Ciencias Naturais" = enem_2016_melhores_CN,
                                        "Ciencias Humanas" = enem_2016_melhores_CH,
                                        "Linguagens e Codigos" = enem_2016_melhores_LC,
                                        "Matematica" = enem_2016_melhores_MT,
                                        "Redacao" = enem_2016_melhores_RE),
                    'piores' = switch(input$hab,
                                      "Performance Geral" = enem_2016_mil_piores,
                                      "Ciencias Naturais" = enem_2016_piores_CN,
                                      "Ciencias Humanas" = enem_2016_piores_CH,
                                      "Linguagens e Codigos" = enem_2016_piores_LC,
                                      "Matematica" = enem_2016_melhores_MT,
                                      "Redacao" = enem_2016_melhores_RE))
    data4 <- switch(input$banco,
                    'geral' = switch(input$hab,
                                     'Performance Geral' = enem_2016_30mil$NOTA_FINAL_ENEM/5,
                                     'Ciencias Naturais' = enem_2016_30mil$NU_NOTA_CN,
                                     'Ciencias Humanas' = enem_2016_30mil$NU_NOTA_CH,
                                     'Linguagens e Codigos' = enem_2016_30mil$NU_NOTA_LC,
                                     'Matematica' = enem_2016_30mil$NU_NOTA_MT,
                                     'Redacao' = enem_2016_30mil$NU_NOTA_REDACAO),
                    'melhores' = switch(input$hab,
                                        'Performance Geral' = enem_2016_mil_melhores$NOTA_FINAL_ENEM/5,
                                        'Ciencias Naturais' = enem_2016_melhores_CN$NU_NOTA_CN,
                                        'Ciencias Humanas' = enem_2016_melhores_CH$NU_NOTA_CH,
                                        'Linguagens e Codigos' = enem_2016_melhores_LC$NU_NOTA_LC,
                                        'Matematica' = enem_2016_melhores_MT$NU_NOTA_MT,
                                        'Redacao' = enem_2016_melhores_RE$NU_NOTA_REDACAO),
                    'piores' = switch(input$hab,
                                      'Performance Geral' = enem_2016_mil_piores$NOTA_FINAL_ENEM/5,
                                      'Ciencias Naturais' = enem_2016_piores_CN$NU_NOTA_CN,
                                      'Ciencias Humanas' = enem_2016_piores_CH$NU_NOTA_CH,
                                      'Linguagens e Codigos' = enem_2016_piores_LC$NU_NOTA_LC,
                                      'Matematica' = enem_2016_piores_MT$NU_NOTA_MT,
                                      'Redacao' = enem_2016_piores_RE$NU_NOTA_REDACAO))
    data5 <- switch(input$banco,
                    'geral' = switch(input$var,
                                     'Sexo' = enem_2016_30mil$TP_SEXO,
                                     'Idade' = enem_2016_30mil$fx_idade,
                                     'Cor autodeclarada' = enem_2016_30mil$TP_COR_RACA,
                                     'Renda' = enem_2016_30mil$renda_SM,
                                     'Tipo de escola frequentada no EM' = enem_2016_30mil$Q047,
                                     'Turno do EM' = enem_2016_30mil$Q049),
                    'melhores' = switch(input$hab,
                                        'Performance Geral' = switch(input$var,
                                                                     'Sexo' = enem_2016_mil_melhores$TP_SEXO,
                                                                     'Idade' = enem_2016_mil_melhores$fx_idade,
                                                                     'Cor autodeclarada' = enem_2016_mil_melhores$TP_COR_RACA,
                                                                     'Renda' = enem_2016_mil_melhores$renda_SM,
                                                                     'Tipo de escola frequentada no EM' = enem_2016_mil_melhores$Q047,
                                                                     'Turno do EM' = enem_2016_mil_melhores$Q049),
                                        'Ciencias Naturais' = switch(input$var,
                                                                     'Sexo' = enem_2016_melhores_CN$TP_SEXO,
                                                                     'Idade' = enem_2016_melhores_CN$fx_idade,
                                                                     'Cor autodeclarada' = enem_2016_melhores_CN$TP_COR_RACA,
                                                                     'Renda' = enem_2016_melhores_CN$renda_SM,
                                                                     'Tipo de escola frequentada no EM' = enem_2016_melhores_CN$Q047,
                                                                     'Turno do EM' = enem_2016_melhores_CN$Q049),
                                        'Ciencias Humanas' = switch(input$var,
                                                                    'Sexo' = enem_2016_melhores_CH$TP_SEXO,
                                                                    'Idade' = enem_2016_melhores_CH$fx_idade,
                                                                    'Cor autodeclarada' = enem_2016_melhores_CH$TP_COR_RACA,
                                                                    'Renda' = enem_2016_melhores_CH$renda_SM,
                                                                    'Tipo de escola frequentada no EM' = enem_2016_melhores_CH$Q047,
                                                                    'Turno do EM' = enem_2016_melhores_CH$Q049),
                                        'Linguagens e Codigos' = switch(input$var,
                                                                        'Sexo' = enem_2016_melhores_LC$TP_SEXO,
                                                                        'Idade' = enem_2016_melhores_LC$fx_idade,
                                                                        'Cor autodeclarada' = enem_2016_melhores_LC$TP_COR_RACA,
                                                                        'Renda' = enem_2016_melhores_LC$renda_SM,
                                                                        'Tipo de escola frequentada no EM' = enem_2016_melhores_LC$Q047,
                                                                        'Turno do EM' = enem_2016_melhores_LC$Q049),
                                        'Matematica' = switch(input$var,
                                                              'Sexo' = enem_2016_melhores_MT$TP_SEXO,
                                                              'Idade' = enem_2016_melhores_MT$fx_idade,
                                                              'Cor autodeclarada' = enem_2016_melhores_MT$TP_COR_RACA,
                                                              'Renda' = enem_2016_melhores_MT$renda_SM,
                                                              'Tipo de escola frequentada no EM' = enem_2016_melhores_MT$Q047,
                                                              'Turno do EM' = enem_2016_melhores_MT$Q049),
                                        'Redacao' = switch(input$var,
                                                           'Sexo' = enem_2016_melhores_RE$TP_SEXO,
                                                           'Idade' = enem_2016_melhores_RE$fx_idade,
                                                           'Cor autodeclarada' = enem_2016_melhores_RE$TP_COR_RACA,
                                                           'Renda' = enem_2016_melhores_RE$renda_SM,
                                                           'Tipo de escola frequentada no EM' = enem_2016_melhores_RE$Q047,
                                                           'Turno do EM' = enem_2016_melhores_RE$Q049)),
                    'piores' = switch(input$hab,
                                      'Performance Geral' = switch(input$var,
                                                                   'Sexo' = enem_2016_mil_piores$TP_SEXO,
                                                                   'Idade' = enem_2016_mil_piores$fx_idade,
                                                                   'Cor autodeclarada' = enem_2016_mil_piores$TP_COR_RACA,
                                                                   'Renda' = enem_2016_mil_piores$renda_SM,
                                                                   'Tipo de escola frequentada no EM' = enem_2016_mil_piores$Q047,
                                                                   'Turno do EM' = enem_2016_mil_piores$Q049),
                                      'Ciencias Naturais' = switch(input$var,
                                                                   'Sexo' = enem_2016_piores_CN$TP_SEXO,
                                                                   'Idade' = enem_2016_piores_CN$fx_idade,
                                                                   'Cor autodeclarada' = enem_2016_piores_CN$TP_COR_RACA,
                                                                   'Renda' = enem_2016_piores_CN$renda_SM,
                                                                   'Tipo de escola frequentada no EM' = enem_2016_piores_CN$Q047,
                                                                   'Turno do EM' = enem_2016_piores_CN$Q049),
                                      'Ciencias Humanas' = switch(input$var,
                                                                  'Sexo' = enem_2016_piores_CH$TP_SEXO,
                                                                  'Idade' = enem_2016_piores_CH$fx_idade,
                                                                  'Cor autodeclarada' = enem_2016_piores_CH$TP_COR_RACA,
                                                                  'Renda' = enem_2016_piores_CH$renda_SM,
                                                                  'Tipo de escola frequentada no EM' = enem_2016_piores_CH$Q047,
                                                                  'Turno do EM' = enem_2016_piores_CH$Q049),
                                      'Linguagens e Codigos' = switch(input$var,
                                                                      'Sexo' = enem_2016_piores_LC$TP_SEXO,
                                                                      'Idade' = enem_2016_piores_LC$fx_idade,
                                                                      'Cor autodeclarada' = enem_2016_piores_LC$TP_COR_RACA,
                                                                      'Renda' = enem_2016_piores_LC$renda_SM,
                                                                      'Tipo de escola frequentada no EM' = enem_2016_piores_LC$Q047,
                                                                      'Turno do EM' = enem_2016_piores_LC$Q049),
                                      'Matematica' = switch(input$var,
                                                            'Sexo' = enem_2016_piores_MT$TP_SEXO,
                                                            'Idade' = enem_2016_piores_MT$fx_idade,
                                                            'Cor autodeclarada' = enem_2016_piores_MT$TP_COR_RACA,
                                                            'Renda' = enem_2016_piores_MT$renda_SM,
                                                            'Tipo de escola frequentada no EM' = enem_2016_piores_MT$Q047,
                                                            'Turno do EM' = enem_2016_piores_MT$Q049),
                                      'Redacao' = switch(input$var,
                                                         'Sexo' = enem_2016_piores_RE$TP_SEXO,
                                                         'Idade' = enem_2016_piores_RE$fx_idade,
                                                         'Cor autodeclarada' = enem_2016_piores_RE$TP_COR_RACA,
                                                         'Renda' = enem_2016_piores_RE$renda_SM,
                                                         'Tipo de escola frequentada no EM' = enem_2016_piores_RE$Q047,
                                                         'Turno do EM' = enem_2016_piores_RE$Q049)))
    
    xx=reactive(input$var)
    tofill <- switch(xx(),
                     'Sexo' = list(name = "Sexo", values = c("F"="#ff4247","M"="#2d44c5"),breaks=c("F","M"),labels = c("Mulheres","Homens")),
                     'Idade' = list(name = "Idade", values = c("Menos de 16 anos"="#fec44f",
                                                               "Entre 16 e 19 anos"="#fe9929",
                                                               "Entre 20 e 25 anos"="#d95f0e",
                                                               "Mais de 25 anos"="#993404"),labels = c("Menos de 16 anos",
                                                                                                       "Entre 16 e 19 anos",
                                                                                                       "Entre 20 e 25 anos",
                                                                                                       "Mais de 25 anos")),
                     'Cor autodeclarada' = list(name = "Raça/Cor autodeclarada",values = c(brewer.pal(7, "Set1"))),
                     'Renda' = list(name = "Renda", values = c(brewer.pal(7, "Oranges"))),
                     'Tipo de escola frequentada no EM' = list(name = 'Tipo de escola frequentada no EM', values = c(brewer.pal(7, "Set1"))),
                     'Turno do EM' = list(name = "Turno do Ensino Médio", values = c(brewer.pal(3, "Set1"))))
    
    a <-  ggplot(data = data3, aes(x = data4, fill = data5))+
      geom_histogram(position = "identity", alpha=0.5, binwidth = ifelse(length(data4)<1001,15,70))+
      do.call(scale_fill_manual, tofill)+
      labs(x = "Nota", y = NULL)+
      theme_bw()+
      theme(panel.grid.major = element_blank())
    ggplotly(a)
  })
  output$t_dados = renderTable(rownames = T,{
    data2 <- switch(input$banco,
                    'geral' = switch(input$var,
                                     'Sexo' = dados_g_s[,-c(1,2,3)],
                                     'Idade' = dados_g_i[,-c(1,2,3)],
                                     'Cor autodeclarada' = dados_g_r[,-c(1,2,3)],
                                     
                                     'Renda' = dados_g_sm[,-c(1,2,3)],
                                     'Tipo de escola frequentada no EM' = dados_g_q047[,-c(1,2,3)],
                                     'Turno do EM' = dados_g_tur[,-c(1,2,3)]) ,
                    'melhores' = switch(input$hab,
                                        'Performance Geral' = switch(input$var,
                                                                     'Sexo' = dados_m_s[,-c(1,2,3)],
                                                                     'Idade' = dados_m_i[,-c(1,2,3)],
                                                                     'Cor autodeclarada' = dados_m_r[,-c(1,2,3)],
                                                                     
                                                                     'Renda' = dados_m_sm[,-c(1,2,3)],
                                                                     'Tipo de escola frequentada no EM' = dados_m_q047[,-c(1,2,3)],
                                                                     'Turno do EM' = dados_m_tur[,-c(1,2,3)]),
                                        'Ciencias Naturais' = switch(input$var,#tem
                                                                     'Sexo' = dados_mCN_s[,-c(1,2,3)],
                                                                     'Idade' = dados_mCN_i[,-c(1,2,3)],
                                                                     'Cor autodeclarada' = dados_mCN_r[,-c(1,2,3)],
                                                                     
                                                                     'Renda' = dados_mCN_sm[,-c(1,2,3)],
                                                                     'Tipo de escola frequentada no EM' = dados_mCN_q047[,-c(1,2,3)],
                                                                     'Turno do EM' = dados_mCN_tur[,-c(1,2,3)]),
                                        'Ciencias Humanas' = switch(input$var,
                                                                    'Sexo' = dados_mCH_s[,-c(1,2,3)],
                                                                    'Idade' = dados_mCH_i[,-c(1,2,3)],
                                                                    'Cor autodeclarada' = dados_mCH_r[,-c(1,2,3)],
                                                                    
                                                                    'Renda' = dados_mCH_sm[,-c(1,2,3)],
                                                                    'Tipo de escola frequentada no EM' = dados_mCH_q047[,-c(1,2,3)],
                                                                    'Turno do EM' = dados_mCN_tur[,-c(1,2,3)]),
                                        'Linguagens e Codigos' = switch(input$var,
                                                                        'Sexo' = dados_mLC_s[,-c(1,2,3)],
                                                                        'Idade' = dados_mLC_i[,-c(1,2,3)],
                                                                        'Cor autodeclarada' = dados_mLC_r[,-c(1,2,3)],
                                                                        
                                                                        'Renda' = dados_mLC_sm[,-c(1,2,3)],
                                                                        'Tipo de escola frequentada no EM' = dados_mLC_q047[,-c(1,2,3)],
                                                                        'Turno do EM' = dados_mCN_tur[,-c(1,2,3)]),
                                        'Matematica' = switch(input$var,
                                                              'Sexo' = dados_mMT_s[,-c(1,2,3)],
                                                              'Idade' = dados_mMT_i[,-c(1,2,3)],
                                                              'Cor autodeclarada' = dados_mMT_r[,-c(1,2,3)],
                                                              
                                                              'Renda' = dados_mMT_sm[,-c(1,2,3)],
                                                              'Tipo de escola frequentada no EM' = dados_mMT_q047[,-c(1,2,3)],
                                                              'Turno do EM' = dados_mMT_tur[,-c(1,2,3)]),
                                        'Redacao' = switch(input$var,
                                                           'Sexo' = dados_mRE_s[,-c(1,2,3)],
                                                           'Idade' = dados_mRE_i[,-c(1,2,3)],
                                                           'Cor autodeclarada' = dados_mRE_r[,-c(1,2,3)],
                                                           
                                                           'Renda' = dados_mRE_sm[,-c(1,2,3)],
                                                           'Tipo de escola frequentada no EM' = dados_mRE_q047[,-c(1,2,3)],
                                                           'Turno do EM' = dados_mRE_tur[,-c(1,2,3)])
                    ),
                    'piores' = switch(input$hab,
                                      'Performance Geral' = switch(input$var,
                                                                   'Sexo' = dados_p_s[,-c(1,2,3)],
                                                                   'Idade' = dados_p_i[,-c(1,2,3)],
                                                                   'Cor autodeclarada' = dados_p_r[,-c(1,2,3)],
                                                                   
                                                                   'Renda' = dados_p_sm[,-c(1,2,3)],
                                                                   'Tipo de escola frequentada no EM' = dados_p_q047[,-c(1,2,3)],
                                                                   'Turno do EM' = dados_p_tur[,-c(1,2,3)]),
                                      'Ciencias Naturais' = switch(input$var,
                                                                   'Sexo' = dados_pCN_s[,-c(1,2,3)],
                                                                   'Idade' = dados_pCN_i[,-c(1,2,3)],
                                                                   'Cor autodeclarada' = dados_pCN_r[,-c(1,2,3)],
                                                                   
                                                                   'Renda' = dados_pCN_sm[,-c(1,2,3)],
                                                                   'Tipo de escola frequentada no EM' = dados_pCN_q047[,-c(1,2,3)],
                                                                   'Turno do EM' = dados_pCN_tur[,-c(1,2,3)]),
                                      'Ciencias Humanas' = switch(input$var,
                                                                  'Sexo' = dados_pCH_s[,-c(1,2,3)],
                                                                  'Idade' = dados_pCH_i[,-c(1,2,3)],
                                                                  'Cor autodeclarada' = dados_pCH_r[,-c(1,2,3)],
                                                                  
                                                                  'Renda' = dados_pCH_sm[,-c(1,2,3)],
                                                                  'Tipo de escola frequentada no EM' = dados_pCH_q047[,-c(1,2,3)],
                                                                  'Turno do EM' = dados_pCH_tur[,-c(1,2,3)]),
                                      'Linguagens e Codigos' = switch(input$var,
                                                                      'Sexo' = dados_pLC_s[,-c(1,2,3)],
                                                                      'Idade' = dados_pLC_i[,-c(1,2,3)],
                                                                      'Cor autodeclarada' = dados_pLC_r[,-c(1,2,3)],
                                                                      
                                                                      'Renda' = dados_pLC_sm[,-c(1,2,3)],
                                                                      'Tipo de escola frequentada no EM' = dados_pLC_q047[,-c(1,2,3)],
                                                                      'Turno do EM' = dados_pLC_tur[,-c(1,2,3)]),
                                      'Matematica' = switch(input$var,
                                                            'Sexo' = dados_pMT_s[,-c(1,2,3)],
                                                            'Idade' = dados_pMT_i[,-c(1,2,3)],
                                                            'Cor autodeclarada' = dados_pMT_r[,-c(1,2,3)],
                                                            
                                                            'Renda' = dados_pMT_sm[,-c(1,2,3)],
                                                            'Tipo de escola frequentada no EM' = dados_pMT_q047[,-c(1,2,3)],
                                                            'Turno do EM' = dados_pMT_tur[,-c(1,2,3)]),
                                      'Redacao' = switch(input$var,
                                                         'Sexo' = dados_pRE_s[,-c(1,2,3)],
                                                         'Idade' = dados_pRE_i[,-c(1,2,3)],
                                                         'Cor autodeclarada' = dados_pRE_r[,-c(1,2,3)],
                                                         
                                                         'Renda' = dados_pRE_sm[,-c(1,2,3)],
                                                         'Tipo de escola frequentada no EM' = dados_pRE_q047[,-c(1,2,3)],
                                                         'Turno do EM' = dados_pRE_tur[,-c(1,2,3)])
                    )
    )
    data2
  })
  output$mymap <- renderLeaflet({
    mapa<-leaflet(width='100%',height='700px',
                  options = leafletOptions(minZoom = 4, maxZoom = 4,
                                           zoomControl=FALSE, doubleClickZoom =FALSE, bounceAtZoomLimits = FALSE,
                                           dragging=FALSE)) %>%
      setView(lng=-49.561972,lat=-14.446204, zoom = 4) %>%
      #addProviderTiles("Esri.WorldShadedRelief")  %>%
      addPolygons(data=br.shp,weight=2,color='#78BD67',fillOpacity=0.4)
    mapa
  })
  
  observe({
    data1 <- switch(input$banco,
                    'geral' = switch(input$var,
                                     'Sexo' = dados_g_s,
                                     'Idade' = dados_g_i,
                                     'Cor autodeclarada' = dados_g_r,
                                     
                                     'Renda' = dados_g_sm,
                                     'Tipo de escola frequentada no EM' = dados_g_q047,
                                     'Turno do EM' = dados_g_tur) ,
                    'melhores' = switch(input$hab,
                                        'Performance Geral' = switch(input$var,
                                                                     'Sexo' = dados_m_s,
                                                                     'Idade' = dados_m_i,
                                                                     'Cor autodeclarada' = dados_m_r,
                                                                     
                                                                     'Renda' = dados_m_sm,
                                                                     'Tipo de escola frequentada no EM' = dados_m_q047,
                                                                     'Turno do EM' = dados_m_tur),
                                        'Ciencias Naturais' = switch(input$var,#tem
                                                                     'Sexo' = dados_mCN_s,
                                                                     'Idade' = dados_mCN_i,
                                                                     'Cor autodeclarada' = dados_mCN_r,
                                                                     
                                                                     'Renda' = dados_mCN_sm,
                                                                     'Tipo de escola frequentada no EM' = dados_mCN_q047,
                                                                     'Turno do EM' = dados_mCN_tur),
                                        'Ciencias Humanas' = switch(input$var,#algo
                                                                    'Sexo' = dados_mCH_s,
                                                                    'Idade' = dados_mCH_i,
                                                                    'Cor autodeclarada' = dados_mCH_r,
                                                                    
                                                                    'Renda' = dados_mCH_sm,
                                                                    'Tipo de escola frequentada no EM' = dados_mCH_q047,
                                                                    'Turno do EM' = dados_mCN_tur),
                                        'Linguagens e Codigos' = switch(input$var,#estranho aqui
                                                                        'Sexo' = dados_mLC_s,
                                                                        'Idade' = dados_mLC_i,
                                                                        'Cor autodeclarada' = dados_mLC_r,
                                                                        
                                                                        'Renda' = dados_mLC_sm,
                                                                        'Tipo de escola frequentada no EM' = dados_mLC_q047,
                                                                        'Turno do EM' = dados_mCN_tur),
                                        'Matematica' = switch(input$var,
                                                              'Sexo' = dados_mMT_s,
                                                              'Idade' = dados_mMT_i,
                                                              'Cor autodeclarada' = dados_mMT_r,
                                                              
                                                              'Renda' = dados_mMT_sm,
                                                              'Tipo de escola frequentada no EM' = dados_mMT_q047,
                                                              'Turno do EM' = dados_mMT_tur),
                                        'Redacao' = switch(input$var,
                                                           'Sexo' = dados_mRE_s,
                                                           'Idade' = dados_mRE_i,
                                                           'Cor autodeclarada' = dados_mRE_r,
                                                           
                                                           'Renda' = dados_mRE_sm,
                                                           'Tipo de escola frequentada no EM' = dados_mRE_q047,
                                                           'Turno do EM' = dados_mRE_tur)
                    ),
                    'piores' = switch(input$hab,
                                      'Performance Geral' = switch(input$var,
                                                                   'Sexo' = dados_p_s,
                                                                   'Idade' = dados_p_i,
                                                                   'Cor autodeclarada' = dados_p_r,
                                                                   
                                                                   'Renda' = dados_p_sm,
                                                                   'Tipo de escola frequentada no EM' = dados_p_q047,
                                                                   'Turno do EM' = dados_p_tur),
                                      'Ciencias Naturais' = switch(input$var,
                                                                   'Sexo' = dados_pCN_s,
                                                                   'Idade' = dados_pCN_i,
                                                                   'Cor autodeclarada' = dados_pCN_r,
                                                                   
                                                                   'Renda' = dados_pCN_sm,
                                                                   'Tipo de escola frequentada no EM' = dados_pCN_q047,
                                                                   'Turno do EM' = dados_pCN_tur),
                                      'Ciencias Humanas' = switch(input$var,
                                                                  'Sexo' = dados_pCH_s,
                                                                  'Idade' = dados_pCH_i,
                                                                  'Cor autodeclarada' = dados_pCH_r,
                                                                  
                                                                  'Renda' = dados_pCH_sm,
                                                                  'Tipo de escola frequentada no EM' = dados_pCH_q047,
                                                                  'Turno do EM' = dados_pCH_tur),
                                      'Linguagens e Codigos' = switch(input$var,
                                                                      'Sexo' = dados_pLC_s,
                                                                      'Idade' = dados_pLC_i,
                                                                      'Cor autodeclarada' = dados_pLC_r,
                                                                      
                                                                      'Renda' = dados_pLC_sm,
                                                                      'Tipo de escola frequentada no EM' = dados_pLC_q047,
                                                                      'Turno do EM' = dados_pLC_tur),
                                      'Matematica' = switch(input$var,
                                                            'Sexo' = dados_pMT_s,
                                                            'Idade' = dados_pMT_i,
                                                            'Cor autodeclarada' = dados_pMT_r,
                                                            
                                                            'Renda' = dados_pMT_sm,
                                                            'Tipo de escola frequentada no EM' = dados_pMT_q047,
                                                            'Turno do EM' = dados_pMT_tur),
                                      'Redacao' = switch(input$var,
                                                         'Sexo' = dados_pRE_s,
                                                         'Idade' = dados_pRE_i,
                                                         'Cor autodeclarada' = dados_pRE_r,
                                                         
                                                         'Renda' = dados_pRE_sm,
                                                         'Tipo de escola frequentada no EM' = dados_pRE_q047,
                                                         'Turno do EM' = dados_pRE_tur)
                    )
    )
    proxy <-
      leafletProxy("mymap", data = data1) %>%
      addMinicharts(
        data1$Long,data1$Lat,
        type='pie',
        width = 55,
        chartdata=data1[,4:length(data1)],
        colorPalette= if(input$var %in% categ){
          brewer.pal(length(data1), "Set1")
        } else {
          brewer.pal(length(data1), "Oranges")
        },transitionTime=0) %>%
      addMinicharts(
        data1["Brasil total","Long"],data1["Brasil total","Lat"],
        type = 'pie',
        width = 150,
        chartdata = data1["Brasil total",4:length(data1)],
        colorPalette = if(input$var %in% categ){
          brewer.pal(length(data1), "Set1")
        } else {
          brewer.pal(length(data1), "Oranges")
        }, transitionTime = 1)
  })
}

shinyApp(ui = ui, server = server)

