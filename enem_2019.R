library(sparklyr)
library(rvest)
library(tidyverse)
library(patchwork)
library(ggrepel)

sc <- spark_connect(master = "local")

pesos_enem <- read_html("https://api.querobolsa.com.br/sisu/notas-de-corte/faculdades/universidade-federal-da-bahia") %>% 
  html_nodes("table") %>% html_table() %>% as.data.frame() %>% as_tibble() %>% janitor::clean_names() %>% 
  filter(cidade_arrow_down == "Salvador") %>% 
  transmute(curso = curso_arrow_down, nota_corte = nota_de_corte_arrow_down) %>% 
  mutate(curso = str_replace_all(curso, "\n", "") %>% str_replace("   ", "-") %>% 
           str_replace(" ", "-") %>% 
           str_replace("(?s) .*", "")) %>% 
  filter(curso == "Ciências-Econômicas-")

enem_2019_geral_tbl <- spark_read_csv(sc = sc,name = "enem_2019_geral_tbl",
                                      path = "IIWEE/MICRODADOS_ENEM_2019.csv",
                                      delimiter = ";", charset = "ISO-8859-1",
                                      memory = FALSE)


enem_2019_pesos <- enem_2019_geral_tbl %>% 
  filter(TP_PRESENCA_CH == 1, TP_PRESENCA_CN == 1, TP_PRESENCA_LC == 1,
         TP_PRESENCA_MT == 1) %>% 
  filter(NU_NOTA_CH != 0, NU_NOTA_CN != 0, NU_NOTA_LC !=0, NU_NOTA_MT != 0,
         NU_NOTA_REDACAO != 0) %>% 
  mutate(media_final_eco = (NU_NOTA_CN*2 + NU_NOTA_CH*4 + NU_NOTA_LC*4 + NU_NOTA_MT*2 + 
                              NU_NOTA_REDACAO*3)/15,
         media_final_est = (NU_NOTA_CN*4 + NU_NOTA_CH*2 + NU_NOTA_LC*2 + NU_NOTA_MT*4 + 
                              NU_NOTA_REDACAO*3)/15,
         media_final_med = (NU_NOTA_CN*4 + NU_NOTA_CH*3 + NU_NOTA_LC*3 + NU_NOTA_MT*2 + 
                              NU_NOTA_REDACAO*3)/15
  ) %>% 
  transmute(CO_UF_RESIDENCIA,TP_SEXO,TP_COR_RACA, TP_ESCOLA, Q006, media_final_eco, media_final_est, media_final_med)

incom_eco <- enem_2019_pesos %>% 
  select(Q006, media_final_eco) %>% collect()

plot_1 <- incom_eco %>% 
  ggplot(aes(x = factor(Q006), y = media_final_eco)) +
  geom_hline(yintercept = 689.04, linetype = 2, color = "gray50") +
  geom_boxplot(color = "#2F5597") +
  scale_x_discrete(labels = c("A"="Nenhuma\nrenda","B"="Até 1\nsalário mínimo", "Q" = "Mais de 20\nsalários mínimos"),
                   breaks = c("A","B", "Q")) +
  scale_y_continuous(breaks = c(200, 400, 600, 689, 800)) +
  labs(
    title = "Distribuição das notas médias com pesos para Ciências Econômicas na UFBA", x = NULL, y = NULL
  ) +
  theme(panel.grid = element_blank(), panel.grid.major.y = element_line(size = .5, color = "gray90"),
        axis.text.x.bottom = element_blank(), axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = NA))


plot_2 <- incom_eco %>% mutate(is_apr = media_final_eco >= 689.04) %>% 
  group_by(Q006) %>% count(is_apr) %>% 
  mutate(prop = n/sum(n)) %>% filter(is_apr == TRUE) %>% 
  ggplot(aes(x = Q006, y = prop)) +
  geom_segment(aes(x=Q006, xend=Q006, y=0, yend=prop)) +
  geom_point(size = 5, color = "#2F5597", fill = "white")+
  scale_y_continuous(labels = scales::percent, breaks = c(seq(0,0.25,by=.05), .23)) +
  scale_x_discrete(labels = c("A"="Nenhuma\nrenda","B"="Até 1\nsalário mínimo", "Q" = "Mais de 20\nsalários mínimos"),
                   breaks = c("A","B", "Q")) +
  labs(
    title = "Proporção de candidatos acima da nota de corte", x = "Renda mensal da família", y = NULL
  ) +
  theme(panel.grid = element_blank(), panel.grid.major.y = element_line(size = .5, color = "gray90"),
        axis.text.x.bottom = element_text(),
        panel.background = element_rect(fill = NA))


plot_1 / plot_2 +  plot_annotation(tag_levels = 'A', caption = "Salário mínimo base 2019: R$ 998,00")

top_mil <- enem_2019_pesos %>% 
  arrange(desc(media_final_eco)) %>% 
  head(1000) %>% collect()
bottom_mil <- enem_2019_pesos %>% 
  arrange(media_final_eco) %>% 
  head(1000) %>% collect()
geral_sexo <- enem_2019_pesos %>% count(TP_SEXO) %>% collect()
geral_raca <- enem_2019_pesos %>% count(TP_COR_RACA) %>% collect()
geral_escola <- enem_2019_pesos %>% count(TP_ESCOLA) %>% collect()
geral_renda <- enem_2019_pesos %>% count(Q006) %>% collect()

top_mil %>% count(TP_SEXO) %>% mutate(classe = "Melhores") %>% 
  bind_rows(
    bottom_mil %>% count(TP_SEXO) %>% mutate(classe = "Piores")
  ) %>% 
  bind_rows(
    geral_sexo %>% mutate(classe = "Geral")
  ) %>% 
  group_by(classe) %>% 
  mutate(freq = n/sum(n))

pie_1 <- top_mil %>% count(TP_COR_RACA) %>% mutate(classe = "Melhores") %>% 
  bind_rows(
    bottom_mil %>% count(TP_COR_RACA) %>% mutate(classe = "Piores")
  ) %>% 
  bind_rows(
    geral_raca %>% mutate(classe = "Geral")
  ) %>% 
  mutate(
    TP_COR_RACA = factor(
      TP_COR_RACA, 
      levels = c(0,1,2,3,4,5,6),
      labels=c('Não declarado',
               'Branca','Preta',
               'Parda','Amarela',
               'Indígena',
               'Não dispôe da informação')
      
      
    )
  ) %>% mutate(classe = factor(classe, levels = c("Melhores","Piores","Geral"))) %>% 
  group_by(classe) %>% mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = "",y = freq, fill = factor(TP_COR_RACA))) +
  geom_bar(stat="identity", width = 1, color = "white") +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~classe,nrow = 2) +
  coord_polar(theta = "y", start=0) +
  ggrepel::geom_text_repel(aes(label = paste0(round(freq*100), "%")), 
                           position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = NULL, y = NULL, fill = NULL, title = "Proporção de Raça/Cor autodeclarada\npelos candidatos no ENEM 2019") +
  theme_classic() + theme(axis.line = element_blank(),
                          legend.position = c(.8, .3),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

#top_mil %>% count(TP_ESCOLA) %>% mutate(classe = "Melhores") %>% 
#  bind_rows(
#    bottom_mil %>% count(TP_ESCOLA) %>% mutate(classe = "Piores")
#  ) %>% 
#  bind_rows(
#    geral_escola %>% mutate(classe = "Geral")
#  ) %>% 
#  group_by(classe) %>% mutate(freq = n/sum(n)) %>% 
#  ggplot(aes(x = TP_ESCOLA, y = freq, fill = classe)) +
#  geom_bar(stat="identity", position = "stack") +
#  scale_y_continuous(labels = scales::percent)


pie_2 <- top_mil %>% count(Q006) %>% mutate(classe = "Melhores") %>% 
  bind_rows(
    bottom_mil %>% count(Q006) %>% mutate(classe = "Piores")
  ) %>% 
  bind_rows(
    geral_renda %>% mutate(classe = "Geral")
  ) %>% 
  group_by(classe) %>% mutate(freq = n/sum(n)) %>% 
  mutate(
    renda = factor(
      case_when(
        Q006 == "A" ~ "Nenhuma Renda",
        Q006 == "B" ~ "Até um salário mínimo",
        Q006 %in% c("C", "D") ~ "De um a dois salários mínimos",
        Q006 %in% c("E", "F") ~ "De dois a três salários mínimos",
        Q006 %in% c("G", "H") ~ "De três a cinco salários mínimos",
        TRUE ~ "Acima de cinco salários mínimos"
      ),
      levels = c("Nenhuma Renda", "Até um salário mínimo", "De um a dois salários mínimos",
                 "De dois a três salários mínimos", "De três a cinco salários mínimos",
                 "Acima de cinco salários mínimos")
    )
    
  ) %>%  mutate(classe = factor(classe, levels = c("Melhores","Piores","Geral"))) %>% 
  group_by(renda, classe) %>% summarise(freq=sum(freq)) %>% 
  ggplot(aes(x = "",y = freq, fill = factor(renda))) +
  geom_bar(stat="identity", width = 1, color = "white") +
  ggrepel::geom_text_repel(aes(label = paste0(round(freq*100), "%")), 
                           position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~classe,nrow = 2) +
  coord_polar(theta = "y", start=0) +
  labs(
    title = "Retrato socioeconômico dos candidatos no ENEM 2019", x = NULL, y = NULL,
    fill = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Nenhuma Renda" = "#52B3B6", "Até um salário mínimo" = "#FDB777", 
      "De um a dois salários mínimos" = "#FDA766", "De dois a três salários mínimos" = "#FD9346", 
      "De três a cinco salários mínimos" = "#FD7F2C",
      "Acima de cinco salários mínimos" = "#FF6200"
    )
  ) +
  theme_classic() + theme(axis.line = element_blank(),
                          legend.position = c(.8, .3),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

pie_1 + pie_2





## 2019 descritiva----

#reg_norte <- c(11:17)
#reg_nordeste <- c(21:29)
#reg_sudeste <- c(31:35)
#reg_sul <- c(41:43)
#reg_centro_oeste <- c(50:53)
#
#sexo <- enem_2019_pesos %>% 
#  mutate(
#    reg = case_when(
#      CO_UF_RESIDENCIA %in% reg_norte ~ "Norte",
#      CO_UF_RESIDENCIA %in% reg_nordeste ~ "Nordeste",
#      CO_UF_RESIDENCIA %in% reg_sudeste ~ "Sudeste",
#      CO_UF_RESIDENCIA %in% reg_sul ~ "Sul",
#      TRUE ~ "Centro-Oeste"
#    )
#  ) %>% 
#  group_by(reg) %>% 
#  count(TP_SEXO) %>% mutate(freq =  n/sum(n)) %>% 
#  collect()
#
#raca <- enem_2019_pesos %>% 
#  mutate(
#    reg = case_when(
#      CO_UF_RESIDENCIA %in% reg_norte ~ "Norte",
#      CO_UF_RESIDENCIA %in% reg_nordeste ~ "Nordeste",
#      CO_UF_RESIDENCIA %in% reg_sudeste ~ "Sudeste",
#      CO_UF_RESIDENCIA %in% reg_sul ~ "Sul",
#      TRUE ~ "Centro-Oeste"
#    )
#  ) %>% 
#  group_by(reg) %>% 
#  count(TP_COR_RACA) %>% mutate(freq =  n/sum(n)) %>% 
#  collect()
#
#esco <- enem_2019_pesos %>% 
#  mutate(
#    reg = case_when(
#      CO_UF_RESIDENCIA %in% reg_norte ~ "Norte",
#      CO_UF_RESIDENCIA %in% reg_nordeste ~ "Nordeste",
#      CO_UF_RESIDENCIA %in% reg_sudeste ~ "Sudeste",
#      CO_UF_RESIDENCIA %in% reg_sul ~ "Sul",
#      TRUE ~ "Centro-Oeste"
#    )
#  ) %>% 
#  group_by(reg) %>% 
#  count(TP_ESCOLA) %>% mutate(freq =  n/sum(n)) %>% 
#  collect()
#
#rend <- enem_2019_pesos %>% 
#  mutate(
#    reg = case_when(
#      CO_UF_RESIDENCIA %in% reg_norte ~ "Norte",
#      CO_UF_RESIDENCIA %in% reg_nordeste ~ "Nordeste",
#      CO_UF_RESIDENCIA %in% reg_sudeste ~ "Sudeste",
#      CO_UF_RESIDENCIA %in% reg_sul ~ "Sul",
#      TRUE ~ "Centro-Oeste"
#    )
#  ) %>% 
#  group_by(reg) %>% 
#  count(Q006) %>% mutate(freq =  n/sum(n)) %>% 
#  collect()
#rend %>% 
#  mutate(
#    renda = factor(
#      case_when(
#        Q006 == "A" ~ "Nenhuma Renda",
#        Q006 == "B" ~ "Até um salário mínimo",
#        Q006 %in% c("C", "D") ~ "De um a dois salários mínimos",
#        Q006 %in% c("E", "F") ~ "De dois a três salários mínimos",
#        Q006 %in% c("G", "H") ~ "De três a cinco salários mínimos",
#        TRUE ~ "Acima de cinco salários mínimos"
#      ),
#      levels = c("Nenhuma Renda", "Até um salário mínimo", "De um a dois salários mínimos",
#                 "De dois a três salários mínimos", "De três a cinco salários mínimos",
#                 "Acima de cinco salários mínimos")
#    )
#    
#  ) %>% group_by(reg, renda) %>% summarise(soma = sum(n)) %>% 
#  #group_by()
#  mutate(freq = sum(soma))
####################
#
#
#enem_2019_pesos_by_reg <- enem_2019_pesos %>% 
#  mutate(
#    reg = case_when(
#      CO_UF_RESIDENCIA %in% reg_norte ~ "Norte",
#      CO_UF_RESIDENCIA %in% reg_nordeste ~ "Nordeste",
#      CO_UF_RESIDENCIA %in% reg_sudeste ~ "Sudeste",
#      CO_UF_RESIDENCIA %in% reg_sul ~ "Sul",
#      TRUE ~ "Centro-Oeste"
#    )
#  )#