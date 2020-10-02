sms_raw <- read.csv(file.choose())
View(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

corpus_clean<-tm_map(sms_corpus, tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean, removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean, removePunctuation)
corpus_clean<-tm_map(corpus_clean, stripWhitespace)
corpus_clean<-tm_map(corpus_clean, PlainTextDocument)

sms_dtm <-DocumentTermMatrix(corpus_clean)
sms_dtm

sms_raw_train <- sms_raw[1:1469 ,]
sms_raw_test <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:1469 ,]
sms_dtm_test <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:1469 ]
sms_corpus_test <- corpus_clean[4170:5559 ]


#check that proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

#indicator features for freuent words
sms_dict <-findFreqTerms(sms_dtm_train ,5)
sms_dict

sms_train <-DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <-DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_train

#convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x>0 , 1 ,0)
  x <- factor(x, levels = c(0,1) , labels =c("No","Yes"))
}

#apply() convert_counts() to columns of train or test data
sms_train  <- apply(sms_train , MARGIN = 2 ,convert_counts)
sms_test  <- apply (sms_test ,MARGIN = 2 ,convert_counts )

#Trainig a model on the data
library(e1071)
sms_classifier <-naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

#Evaluating model performance
sms_test_pred <-predict(sms_classifier , sms_test)

library(gmodels)
CrossTable(sms_test_pred ,sms_raw_test$type ,
           prop.chisq = FALSE , prop.t = FALSE , prop.r = FALSE ,
           dnn = c('predicted','actual'))
