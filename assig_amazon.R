install.packages("rvest")
install.packages("XML")
install.packages("magrittr")



library(rvest)
library(XML)
library(magrittr)

aurl<-"https://www.amazon.in/Redmi-6A-Black-2GBRAM-Storage/dp/B077PW9V3J/ref=sr_1_1?keywords=redmi&qid=1559827220&s=gateway&sr=8-1#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"redmi.txt",row.names = F)

## Using While loop to get all reviews without using page number ############## 
#### Simple Example ##################################################
samp_url <- "https://www.amazon.in/Redmi-6A-Black-2GBRAM-Storage/dp/B077PW9V3J/ref=sr_1_1?keywords=redmi&qid=1559827220&s=gateway&sr=8-1#customerReviews"
i=1
p=1
redmi_reviews <- NULL
while(p>0){
  t_url <- read_html(as.character(paste(samp_url,i,sep="=")))
  rev <- t_url %>%
    html_nodes(".review-text") %>%
    html_text()
  redmi_reviews <- c(redmi_reviews,rev)
  i <- i+1
  p=length(rev)
}

length(redmi_reviews)
write.table(redmi_reviews,"redmi1.txt",row.names = F)

#######################################################################################
install.packages("rjava")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RWeka")
install.packages("textir")
install.packages("igraph")
install.packages("qdap")
install.packages("maptpx")



library(rjava)
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)
library(textir)
library(igraph)
library(qdap)
library(maptpx)




redmi_corpus<-Corpus(VectorSource(redmi_reviews))
inspect(redmi_corpus[1000])


redmi_clean<-tm_map(redmi_corpus, removePunctuation)
redmi_clean<-tm_map(redmi_clean, content_transformer(tolower))
redmi_clean<-tm_map(redmi_clean, removeWords, stopwords("english"))
redmi_clean<-tm_map(redmi_clean,removeNumbers)
redmi_clean<-tm_map(redmi_clean, stripWhitespace)

redmi_clean<-tm_map(redmi_clean, removeWords, c("gameofthrones")) ## clean some words

install.packages("wordcloud")
library(wordcloud)


wordcloud(redmi_clean, random.order = F, max.words = 50,colors=rainbow(50))
wordcloud(redmi_clean, rot.per=0.5, random.order=TRUE,colors=brewer.pal(8, "Dark2"))



# func to make wordcloud and view some freqs for docs 

makewordc = function(a){
  
  a.colsum = apply(a, 2, sum);
  
  words = colnames(a)
  
  freq = a.colsum*100
  
  wordcloud(words, freq, scale=c(8, 0.3), colors=1:10)  } 

# func to make cluster dendograms 

clusdend = function(a){     
  
  mydata.df = as.data.frame(inspect(a));    
  
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  
  min1 = min(ncol(mydata.df), 40)   # minimum dimn of dist matrix
  
  test = matrix(0,min1,min1)
  
  test1 = test
  
  for(i1 in 1:(min1-1)){ 
    
    for(i2 in i1:min1){
      
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2]    }
    
  }
  
  # making dissimilarity matrix out of the freq one
  
  test2 = test1
  
  rownames(test2) = colnames(mydata1.df)[1:min1]
  
  # now plot collocation dendogram
  
  d <- dist(test2, method = "euclidean") # distance matrix
  
  fit <- hclust(d, method="ward")
  
  plot(fit) # display dendogram
  
} 






# Sentimental analysis
pos<-scan(file.choose(),what="character", comment.char=";")
neg<-scan(file.choose(),what="character", comment.char=";")
pos.words=c(pos,"wow", "kudos", "hurray")           
neg.words = c(neg)


# positive sentiment wordcloud

makeposwordc = function(a){ 
  
  pos.matches = match(colnames(a), pos.words)       # match() returns the position of the matched term or NA
  
  pos.matches = !is.na(pos.matches)
  
  b1 = apply(a, 2, sum)[pos.matches];    b1 = as.data.frame(b1);
  
  colnames(b1) = c("freq");
  
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)   # wordcloud of positive words
  
}   

# negative sentiment wordlist

makenegwordc = function(a){ 
  
  neg.matches = match(colnames(a), neg.words)       # match() returns the position of the matched term or NA
  
  neg.matches = !is.na(neg.matches)
  
  b1 = apply(a, 2, sum)[neg.matches];    b1 = as.data.frame(b1);
  
  colnames(b1) = c("freq");
  
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)   # wordcloud of negative words
  
}   

x<-readLines(file.choose())
length(x)
x1<-Corpus(VectorSource(x))

x1<-tm_map(x1, stripWhitespace)
x1<-tm_map(x1,tolower)
x1<-tm_map(x1, removePunctuation)
x1<-tm_map(x1,removeNumbers)
x1<-tm_map(x1, removeWords, c(stopwords('english')))
x1<-tm_map(x1,PlainTextDocument)

tdm1 <-TermDocumentMatrix(x1, control=list(weighting=function(x) weightTfIdf(x,normalize = FALSE),stopwords = TRUE)); 
tdm0 <-TermDocumentMatrix(x1) 
dim(tdm1)


a0 = NULL; 

for (i1 in 1:ncol(tdm0)){ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }

length(a0)
if (length(a0) >0) { tdm01 = tdm0[, -a0]} else {tdm01 = tdm0};  dim(tdm01) 
if (length(a0) >0) { tdm11 = tdm1[, -a0]} else {tdm11 = tdm1};  dim(tdm11) 

dtm0 = t(tdm01)            

dtm = t(tdm11)                 

windows()          

makewordc(dtm)
title(sub = "UNIGRAM - Wordcloud using TFIDF")

#windows();  semantic.na(dtm0)
windows();  clusdend(dtm0)
windows();  makeposwordc(dtm)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")
windows();  makenegwordc(dtm)
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")









