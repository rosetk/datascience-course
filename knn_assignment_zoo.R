zoo <- read.csv(file.choose())
View(zoo)
zoo<-zoo[-1]
View(zoo)
# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(zoo$type))*100,1)





#create training and test datasets
zoo_train <- zoo[1:75 ,]
zoo_test <- zoo[76:101,]



zoo_train_labels <- zoo[1:75,17]
zoo_test_labels <- zoo[76:101,17]

install.packages("class")
library("class")

# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_zoo_pred <- knn(train=zoo_train,test=zoo_train,cl=zoo_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred==zoo_train_labels))
  test_zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==zoo_test_labels))
}


# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=1)

install.packages("gmodels")
library("gmodels")

CrossTable(x=zoo_test_labels , y= zoo_pred ,prop.chisq=FALSE)

install.packages("sjPlot")
library(sjPlot)
sjc.elbow(zoo)
