library(rtweet)
library(tidyverse)
require("tm")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
twitter_tokens <- create_token(app = "BigDataUd",
                               consumer_key = "9Nk2DP5mMvCxRl0SvB45ehRgd", 
                               consumer_secret = "RFSSRLDs0kAghBfMsJyXQvamI0eLCIWplp2CIIl9LtUW8En6Wc")


rt <- search_tweets(
  "ICBF", n = 10000, include_rts = FALSE
)
head(rt)
icbf<-as.data.frame(rt$text)
write.csv(icbf,file='~/icbftweets2.csv',row.names = F )

icbf_tweets<-read.csv(file.choose(),header = F)
icbf_tweets<-iconv(ICBF_tweets$text,to="ISO-8859-1")
icbf_sentiment<- get_sentiment(icbf_tweets)
plot(
  icbf_sentiment, 
  type="l", 
  main="Comportamiento emocional de los Tweets en el tiempo", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)


nrc_data <- get_nrc_sentiment(icbf_tweets)

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emociones con respecto al ICBF", xlab="Percentage"
)
head(nrc_data)
##Análisis con Tratamiento de texto##
ICBF<-Corpus(VectorSource(icbf_tweets))
inspect(ICBF[1:10])
#Cleaning Data#
#Pasar todo el texto a minusculas#
ICBF<-tm_map(ICBF,tolower)
#Remover puntuación#
ICBF<-tm_map(ICBF,removePunctuation)
#Remover números#
ICBF<-tm_map(ICBF,removeNumbers)
#remover URL#
removeURL<-function(x)gsub('http[[:alnum:]]*',"",x)
ICBFclean<-tm_map(ICBFclean,content_transformer(removeURL))
#Remover articulos#
ICBFclean<-tm_map(ICBF,removeWords,stopwords('spanish'))
inspect(ICBFclean[1:10])
#Cuando remueve una palabra tembien deja un espacion en blanco que se debe remover#
ICBFclean<-tm_map(ICBFclean,stripWhitespace)
#Como ya se tiene entendido que todos los tweets son acerca del ICBF procedemos a remover esa palabra#
ICBFclean<-tm_map(ICBFclean,removeWords,c('ICBF','icbf'))
#Transformar en una matrix para estructurar los datos#
icbfm<-TermDocumentMatrix(ICBFclean)
icbfm
icbfm<-as.matrix(icbfm)
icbfm[1:10,1:20]
#Bar plot#
#determinar la frecuencia por palabra#
w<-rowSums(icbfm)
#Generar un subset#
w<-subset(w,w>=25)
library(wordcloud)
w<-sort(rowSums(icbfm),decreasing = TRUE)
set.seed(222)
#análsis de sentiemientos#
write.csv(ICBFclean$content,file='~/icbftweets3.csv',row.names = F )
nrc_data <- get_nrc_sentiment(ICBFclean$content[1:1000])

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emociones con respecto al ICBF filtrado", xlab="Percentage"
)
