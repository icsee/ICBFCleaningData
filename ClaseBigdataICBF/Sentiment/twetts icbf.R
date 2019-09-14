install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")
require("twitteR")
require("RCurl")
require("bitops")
require("tm")
consumer_key <- '9Nk2DP5mMvCxRl0SvB45ehRgd'
consumer_secret <-'RFSSRLDs0kAghBfMsJyXQvamI0eLCIWplp2CIIl9LtUW8En6Wc'
access_token <-'918859744517001216-6T2vhTJxYQgmALrOstkhC0Gomde81Wx'
access_secret<-'mNNdPDtx8Dkz6bzkr98DgIS0wLtLmskKV6Gg0sN3xjw8I'
setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)
#para buscar tanencias se usa ..1) se identifica el lugar del cual se quirere realizar el análisis#
trend<-availableTrendLocations()
head(trend)
#se ejecuta el comanda para extraer la información de las tendencias#
word<-getTrends(368148)
word
ICBF_tweets <- searchTwitter("ICBF", n=10, lang="es")
k<-userTimeline("ICBF", n = 10, includeRts = T, tweet_mode = "extended")
ICBF_tweetsdf<-twListToDF(ICBF_tweets)
write.csv(ICBF_tweetsdf,file='~/icbftweets.csv',row.names = F )
#Análisis de sentimientos#
ICBF_tweets<-read.csv(file.choose(),header = T)
ICBF<-iconv(ICBF_tweets$text,to="ISO-8859-1")
ICBF<-Corpus(VectorSource(ICBF))
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
#reemplazar análsis#
ICBFclean<-tm_map(ICBFclean,gsub,pattern='direcciÃ',replacement='dirección')
ICBFclean<-tm_map(ICBFclean,gsub,pattern='explotaciÃ',replacement='explotación')
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
#análisis de sentimientos#
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
#leer el archivo#
ICBF_tweets<-read.csv(file.choose(),header = T)
ICBF<-iconv(ICBF_tweets$text,to="ISO-8859-1")
#obtener frecuencia de los sentimientos#
s<-get_nrc_sentiment(ICBF)
head(s)

