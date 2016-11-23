#install the necessary packages
#install.packages("twitteR")
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("rvest")
#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("ggplot2")

library("twitteR")
library("stringr")
library("tm")

#Data-Inicio Data-Termino Hashtag 
  
args = commandArgs(trailingOnly=TRUE)

caminho = "F:/analisador_sentimentos/"

setwd(caminho)

data.inicial = args[1]
data.final = args[2]
hashtag = args[3]

print(data.inicial)

consumer_key <- 'fNzv8UHijytruZ5K12wR8BukT'
consumer_secret <- '62gJKnpIumCsD4od8ICj0XiPocssjhUm6QIsqeyS4QKylKtE7B'
access_token <- '720060230617427969-Jc8EplKKJ0GqcGPwLqTEm2sApvKvv4U'
access_secret <- 'FkARP3SI3KvQtQ25XOtd8WAtR7hLfbKElKcGE9nzlQsAG'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

tweets <- searchTwitter(hashtag, n=5000, lang = "pt")
#tweets_date <- searchTwitter("#DoisMesesProFimdoAno", since = data.inicial, until = data.final, n = 5000, lang = "pt")

#Remove os retweets
#tweets_new = strip_retweets(tweets)

df <- do.call("rbind", lapply(tweets, as.data.frame))
data.temp <- strsplit(as.character(df$created), " ")
data.matrix <- matrix(unlist(data.temp), ncol=2, byrow=TRUE)
df$data <- as.Date(data.matrix[,1])

#save text
#r_stats_text <- sapply(df$text, function(x) x$getText())

#Create Corpus
corpus_tweets <- Corpus(VectorSource(df$text))
#print(df$text[2])

#clean up
corpus_tweets <- tm_map(corpus_tweets, removePunctuation)
corpus_tweets <- tm_map(corpus_tweets, content_transformer(tolower))
#corpus_tweets <- tm_map(corpus_tweets, function(x)removeWords(x,stopwords()))


#paste concatena strings
#cria as variaveis de caminho
arquivo.csv = paste("tweets/", str_replace_all(paste(hashtag,".csv", sep = ""), "#", ""), sep = "")
arquivo.txt = paste("tweets/", str_replace_all(paste(hashtag,".txt", sep = ""), "#", ""), sep = "")

write.table(unlist(sapply(corpus_tweets, `[`, "content")), arquivo.txt, sep=";") 

write.csv(df, arquivo.csv)

caminho.arquivo = paste(caminho, arquivo.txt, sep = "")

comando = paste("java -jar lematizador.jar ", caminho.arquivo, " nf", sep="")
comando.proximo = paste("Rscipt analises.R", hashtag, arquivo.csv, arquivo.txt, sep=" ")
system(comando, input = NULL, show.output.on.console = TRUE, wait = TRUE)
system(comando.proximo, input = NULL, show.output.on.console = TRUE, wait = TRUE)


