
##########################################
## LDA Pedidos do Achados e Pedidos ######
##########################################

#um dos melhores crans que eu já vi:
#https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

# pacotes
library(XML)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)
library(seqinr)
library(RTextTools)
library(topicmodels)
library(data.table)
library(devtools)
#devtools::install_github("sfirke/janitor")
library(janitor)
# devtools::install_github("mgaldino/tbaep")
library(tbaep)
library(dplyr)


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\text_mining")

load("C:/Users/jvoig/OneDrive/Documentos/text_mining/pedidos.Rdata") #Script pedidos.R

# remove acentos, lowecase etc.

pedidos <- pedidos %>%
  mutate(descricao_demanda = snakecase::to_any_case(descricao_demanda, case = "none",
                                                    transliterations = c("Latin-ASCII")))


##stopwords:
pt_stop_words <- read.csv(url("http://raw.githubusercontent.com/stopwords-iso/stopwords-pt/master/stopwords-pt.txt"),
                          encoding = "UTF-8", header = FALSE)

# If you know the encoding of the string, or if its encoding is the 
# current locale encoding, then you can use the iconv function to convert 
# the string to ASCII. Something like: 
#   
#   iconv(accented.string, to="ASCII//TRANSLIT") 

pt_stop_words2 <- data.frame(iconv(pt_stop_words$V1, from="UTF-8",to="ASCII//TRANSLIT")) #estou fazendo uma stopwords sem acento

colnames(pt_stop_words2) = c("V1") #deixando os dfs com o mesmo nome

pt_stopwordsfinal <- pt_stop_words %>%
  rbind(pt_stop_words2) %>%
  distinct(V1, .keep_all = TRUE) %>%
  mutate(V1 = as.character(V1)) %>%
  arrange(V1)
  
my_stopwords <- unique(c(stopwords("portuguese"), "governador",
                         "pedido", "lei", "acesso", "informação", "º", "ª", "nº",
                         "cordiais", "cumprimentos", "ola", "boa tarde", "bom dia",
                         "boa noite", "atenciosamente", "prezado", "prezada",
                         "prezado(a)", "prezados", pt_stopwordsfinal$V1, "pública",
                         "gosto", "gostaria", "solicito", "saber", "portal", "transparencia",
                         "caros", "solicitar", "solicitação", "solicitando", "acordo",
                         "dados", "referente", "dado", "referente", "informacao", "sobre", "caso",
                         "acerca", "referente", "refere", "base", "informacoes", "informações",
                         "informacao", "receber", "recebimento", "att", "cordialmente",
                         "xxx", "preciso", "precisaria", "agradeço", "agradecido", "agradecida",
                         "agradecimento", "agradecimentos", "público", "publico","publicado",  "agradeco", "grato",
                         "grata", "solicitei", "solicitação", "solicitacao", "enviada", "enviado",
                         "encaminhada", "encaminhado", "encaminho", "anexo", "anexada", "anexado",
                         "sr.", "sr", "senhor", "senhora", "senhores", "senhoras", "senhorita",
                         "senhoritas", "doutor", "doutores", "doutora", "doutoras", "dr.", "dr",
                         "publicado", "publicada", "atendido", "atendida", "atendimento", "aguardo",
                         "resposta", "respostas", "respondida", "respondido", "aguardando", "art",
                         "artigo"))
                         

duplicated(my_stopwords) #não há duplicatas

#Removendo acentos

pedidos1 <- pedidos %>%
  select(descricao_demanda) 

# transforma DF em vetor 
pedidos2 <- pedidos1$descricao_demanda #com o Dataframesource poderia não ter transformado o df em vetor

ped <- Corpus(VectorSource(pedidos2)) # tenho que transformar o dataframe em corpus
ped
inspect(ped[15:18])
# my_stopwords <- unique(unlist(c(stopwords("portuguese"),pt_stop_words)))

ped <- tm_map(ped, content_transformer(tolower))
ped <- tm_map(ped, removeNumbers)
ped <- tm_map(ped, removePunctuation) #tira os pontos
f <- content_transformer(function(x, pattern) gsub("¿", "", x))
ped <- tm_map(ped, f)

ped <- tm_map(ped, removeWords, my_stopwords) # demora um pouco

ped <- tm_map(ped , stripWhitespace) #extrawhitespace
ped <- tm_map(ped, stemDocument, language = "portuguese")



#no caso do tm_map(), usar content_transformer(function), 
#para usar funções do R para dfs , pode ser usado com o tolower ou o gsub

inspect(ped[22:24])
dtm.control <- list(wordLengths = c(3,Inf),
                    weighting = weightTf)

dtm <- DocumentTermMatrix(ped, control = dtm.control)
dim(dtm)
inspect(dtm[1:20,1:20])



## cada doc é uma linha
# cada palavra uma coluna
# número de colunas e igual número de palavras únicas em todos os docs


freq_words <- rowSums(as.matrix(dtm)) # Quantas palavras cada documento (linha) tem
index <- which(freq_words==0) # índice de documentos em que não há palavras

dtm1 <- dtm[-index, ] # remove # palavras que não ocorrem em nenhum documento.
findFreqTerms(dtm1, 5) #encontrando termos que ocorreram ao menos 5x


# a partir daqui começa LDA

set.seed(51)
trainpoints <- sample(1:nrow(dtm1), 1*nrow(dtm1),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8

k <- 20 # número de tópicos. Estou chutando

## função pra extrair termos
SpecificTerms <- function(lda.model,n=1) {
  p <- posterior(lda.model)$terms
  n <- min(n,ncol(p))
  cumulativep <- colSums(p)
  specificp <- t(apply(p,1,function(x) ((x) + (x/cumulativep) )))
  
  topterms <- t(apply(specificp, 1, function(x)
    (colnames(specificp)[order(x,decreasing=T)[1:n]]) ))
  
  topterms
}

set.seed(2)
lda1 <- LDA(dtm1, k)

# t termos mais prováveis por tópico
t <- 10
View(terms(lda1, t))

## vamos olhar o Topic 17
## Aprov Eleitoral? Bush, Clinton etc.



# t termos com prob acima de minimo
minimo <- .02
terms(lda1, t, threshold=minimo)  

#20 é um numero alto



# tópicos mais prováveis por documentos
head(topics(lda), 10)