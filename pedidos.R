#Juntando vários pedidos:

library(dplyr)
library(janitor)
library(stringr)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\text_mining")

mppi15 <- fread("mppi15.csv", encoding="UTF-8")
govmg15 <- fread("govmg15.csv", encoding="UTF-8")
cnj13 <- fread("cnj13.csv", encoding="UTF-8")
prefrecif15 <- read_delim("C:/Users/jvoig/OneDrive/Documentos/text_mining/pedidostransparencia2015.csv", 
                          +     ";", escape_double = FALSE, trim_ws = TRUE)
tcu13 <- fread("tcu13.csv", encoding = "UTF-8")
  
#validado importação correta.

#eu vou colocar um ID em cada pedido e vou deixar apenas a descrição da demanda

mppi15 <- mppi15 %>% select(TEOR) %>% rename(descricao_demanda = TEOR)
View(mppi15)

govmg15 <- govmg15 %>% clean_names() %>% select(descricao_demanda)
View(govmg15)

cnj13 <- cnj13 %>% clean_names() %>% select(texto_do_relato) %>% rename(descricao_demanda = texto_do_relato)
View(cnj13)

prefrecif15 <- prefrecif15 %>% select(descricao) %>% rename(descricao_demanda = descricao)
View(prefrecif15)

tcu13 <- tcu13 %>% select(TEOR) %>% rename( descricao_demanda = TEOR)
View(tcu13)

pedidos <- mppi15 %>% rbind(govmg15) %>% rbind(cnj13) %>% rbind(prefrecif15) %>% rbind(tcu13) %>%
  mutate(ID = 1:n()) %>% select(ID, descricao_demanda)
View(pedidos)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\text_mining")
save(pedidos, file="pedidos.Rdata")
