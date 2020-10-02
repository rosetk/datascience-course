#install.packages("twitteR")
library("twitteR")
#install.packages("ROAuth")
library("ROAuth")
#install.packages("base64enc")
library(base64enc)
#install.packages("httpuv")
library(httpuv)



library(rjava)
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)
library(textir)
library(igraph)
library(qdap)
library(maptpx)



cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
 #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('msdhoni', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets_dhoni.csv",row.names = F)

#########################
dhoni_tweets<- searchTwitter('msdhoni', n=100, lang="en", resultType = "recent")
class(dhoni_tweets)
dhoni_tweets[1:20]

dhoni_txt<-sapply(dhoni_tweets, function(x) x$getText())

str(dhoni_txt)

#https://apps.twitter.com/ 


dhoni_corpus<-Corpus(VectorSource(dhoni_txt))
inspect(dhoni_corpus[100])


dhoni_clean<-tm_map(dhoni_corpus, removePunctuation)
dhoni_clean<-tm_map(dhoni_clean, content_transformer(tolower))
dhoni_clean<-tm_map(dhoni_clean, removeWords, stopwords("english"))
dhoni_clean<-tm_map(dhoni_clean,removeNumbers)
dhoni_clean<-tm_map(dhoni_clean, stripWhitespace)

dhoni_clean<-tm_map(dhoni_clean, removeWords, c("gameofthrones")) ## clean some words

install.packages("wordcloud")
library(wordcloud)


wordcloud(dhoni_clean, random.order = F, max.words = 50,colors=rainbow(50))
wordcloud(dhoni_clean, rot.per=0.5, random.order=TRUE,colors=brewer.pal(8, "Dark2"))

