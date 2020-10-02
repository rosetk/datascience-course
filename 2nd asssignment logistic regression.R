library(dplyr)


bank<-read.csv(file.choose(), stringsAsFactors = FALSE, header = T)
apply(bank,2,function(x)sum(is.na(x)))



bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$default<- ifelse(bank$default == "yes", 1, 0)
bank$housing <- ifelse(bank$housing== "yes", 1, 0)
bank$loan<- ifelse(bank$loan== "yes", 1, 0)
bank$month <- as.numeric(as.factor(bank$month))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))
bank$y <- ifelse(bank$y== "yes", 1, 0)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

bank <- as.data.frame(lapply(bank, normalize))


model <- glm(y ~.,data=bank,family = "binomial")
exp(coef(model))
prob <- predict(model,bank,type="response")
summary(model)
confusion<-table(prob>0.5,y)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

summary(model)
