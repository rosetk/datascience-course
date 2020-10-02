creditcard<- read.csv(file.choose())

creditcard <- creditcard[,-1] # Removing the first column which is is an Index
attach(creditcard)

model <- glm(card ~.,data=creditcard,family = "binomial")
exp(coef(model))
prob <- predict(model,creditcard,type="response")
summary(model)
confusion<-table(prob>0.5,card)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy


reports<-as.factor(reports)
dependents<-as.factor(dependents)
owner<-as.factor(owner)
selfemp<-as.factor(selfemp)
majorcards<-as.factor(majorcards)


creditcard2<-data.frame(creditcard ,reports,dependents, owner, selfemp, majorcards )


logit1<- glm(card ~ reports.1 +age +income + share + expenditure + owner.1
            + selfemp.1 + dependents.1 + months + majorcards.1 + 
              active , family= "poisson" , data = creditcard2)

exp(coef(logit1))
# Confusion matrix table 
prob <- predict(logit1,creditcard2,type="response")
summary(logit1)

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,creditcard2$card)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
