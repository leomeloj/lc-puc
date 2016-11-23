library("tm")
library("wordcloud")
library("RColorBrewer")

args = commandArgs(trailingOnly=TRUE)

hashtag = args[1]
arquivo.csv = args[2]
arquivo.txt = args[3]

caminho = "F:/analisador_sentimentos/"


setwd(caminho)

leitor = readLines("stopwords_portugues.txt")
stopwords = lapply(leitor, c)

leitor = readLines("positivas.txt")
positivas = lapply(leitor, c)

leitor = readLines("negativas.txt")
negativas = lapply(leitor, c)


score.sentiment = function(sentences, pos.words, neg.words, .progress='none') {
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    

    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

classifica_tweets = function(pontuacao, .progress='none') {
  
  sentimento = laply(pontuacao$score, function(pontuacao) {
    
    if(pontuacao > 0)
      sentimento = "Positivo"
    else if (pontuacao < 0)
      sentimento = "Negativo"
    else
      sentimento = "Neutro"
    return(sentimento)
  })
  
  return(sentimento)
  
}

df <- read.csv(arquivo.csv)

df.new<-read.csv(arquivo.csv)
#write.table(unlist(sapply(corpus_tweets, `[`, "content")), "tweets.txt", sep=";") 
#system("java –jar F:/analisador_sentimentos/LematizadorV2a/lematizador.jar F:/analisador_sentimentos/tweets.txt nf")
df.new$text = unlist(read.table(arquivo.txt, sep=";"))
length(df.new$text)

pontuacao = score.sentiment(df.new$text, positivas, negativas, .progress='text')
pontuacao$sentimento = classifica_tweets(pontuacao, .progress='text')
#print(length(df.new$text))
pontuacao$very.pos = as.numeric(pontuacao$score >= 1)
pontuacao$very.neg = as.numeric(pontuacao$score <= -1)
pontuacao$data = as.Date(df.new$data)
#scores$Month = df.texts.clean$Month
#scores$BankID = df.texts.clean$BankID

numpos = sum(pontuacao$very.pos)
numneg = sum(pontuacao$very.neg)

df.negativo = pontuacao[pontuacao$very.neg == 1, c(1, 2)]
df.positivo = pontuacao[pontuacao$very.pos == 1, c(1, 2)]


# Library gplot2
library(ggplot2)

p  <- ggplot(pontuacao, aes(data, colour=sentimento, fill=sentimento))
p  <- p + geom_density(alpha=0.45)
ggsave(filename = paste("graficos/", str_replace_all(hashtag, "#", ""), ".jpeg", sep = ""), plot = p, device = "jpeg")




ggsave("grafico.png", p)

#Create Corpus
corpus_tweets <- Corpus(VectorSource(df$text))
#print(df$text[2])

#clean up
corpus_tweets <- tm_map(corpus_tweets, removePunctuation)
corpus_tweets <- tm_map(corpus_tweets, content_transformer(tolower))
#corpus_tweets <- tm_map(corpus_tweets, function(x)removeWords(x,stopwords()))
corpus_tweets <- tm_map(corpus_tweets, removeWords, c("pec55", "httpstcox7tn", "pec", "httpstcobk8rlxkwrf"))

corpus_tweets_pos <- Corpus(VectorSource(df.positivo$text))
corpus_tweets_pos <- tm_map(corpus_tweets_pos, content_transformer(tolower))
corpus_tweets_pos <- tm_map(corpus_tweets_pos, function(x)removeWords(x,stopwords()))
corpus_tweets_pos <- tm_map(corpus_tweets_pos, removeWords, c("pec55", "httpstcox7tn", "pec", "httpstcobk8rlxkwrf"))

corpus_tweets_neg <- Corpus(VectorSource(df.negativo$text))
corpus_tweets_neg <- tm_map(corpus_tweets_neg, content_transformer(tolower))
corpus_tweets_neg <- tm_map(corpus_tweets_neg, function(x)removeWords(x,stopwords()))
corpus_tweets_neg <- tm_map(corpus_tweets_neg, removeWords, c("pec55", "httpstcox7tn", "pec", "httpstcobk8rlxkwrf"))

pal2 <- brewer.pal(8,"Dark2")
png(paste("wordclouds/", str_replace_all(hashtag, "#", ""), "-negative.png", sep = ""), width=1280,height=800)
wordcloud(corpus_tweets_neg, min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

pal2 <- brewer.pal(8,"Dark2")
png(paste("wordclouds/", str_replace_all(hashtag, "#", ""), "-positive.png", sep = ""), width=1280,height=800)
wordcloud(corpus_tweets_pos, min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
