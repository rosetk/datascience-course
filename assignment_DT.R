install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
install.packages("caret")
install.packages("e1071")
install.packages("gmodels")

library(C50)
data()
data("iris")
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 


iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building model on training data 
irisc5.0_train <- C5.0(iris_train[,-5],iris_train$Species)
windows()
plot(irisc5.0_train) # Tree graph
# Training accuracy
pred_train <- predict(irisc5.0_train,iris_train)

mean(iris_train$Species==pred_train) # 97.33% Accuracy

library(e1071)
library(caret)
confusionMatrix(pred_train,iris_train$Species)

predc5.0_test <- predict(irisc5.0_train,newdata=iris_test) # predicting on test data
mean(predc5.0_test==iris_test$Species) # 94.66% accuracy 
confusionMatrix(predc5.0_test,iris_test$Species)
library(gmodels)
# Cross tablez
CrossTable(iris_test$Species,predc5.0_test)



#Bagging
fittree<- vector(mode="list", length=100)
#fittree is a vector to store different bagged models
test1<-vector(mode="list", length=100)
#test1 is a vector to store different test samples
#to be used in prediction
set.seed(2)
for(i in 1:100)
{
  partition1 <- createDataPartition(iris$Species , p=0.8 , list=F)
  train1<-iris[partition1,]
  test1[[i]] <- iris[-partition1,]
  fittree[[i]] <-C5.0(train1$Species~., data=train1[,-5])
}

fittree #100 bagged models stored here
test1 #100 test sample data stored here
# both fittree and test1 are used here

#Predicting Bagged models on test data
pred1<-vector(mode="list",length=100) # defining pred1 as vector
conf1<-vector(mode="list",length=100) # defining conf1 as vector
acc1<-vector(mode="list",length=100) # defining acc1 as vector

for(j in 1 :length (fittree))
{
  pred1[[j]] <- predict.C5.0 (fittree[[j]],test1[[j]] [,-5], type="class")
  conf1[[j]] <-table(pred1[[j]],test1[[j]]$Species)
  acc1[[j]] <- sum(diag(conf1[[j]]))/sum(conf1[[j]])
}

pred1
conf1
acc1
class(acc1) #cannot perform mean() on object of class "list"
mean(unlist(acc1))

#Boosting
#Model building 100 times
acc<-c()
for (i in 1:100)
{
  inTraininglocal<-createDataPartition(iris$Species , p=0.85 , list=F)
  training1 =iris[inTraininglocal,]
  testing = iris[-inTraininglocal,]
  
  fittree = C5.0(training1$Species~.,data=training1[,-5],trials =15)
  pred=predict.C5.0(fittree,testing[,-5])
  a=table(pred,testing$Species)
  acc=c(acc,sum(diag(a))/sum(a))
  
  }
mean(acc)

