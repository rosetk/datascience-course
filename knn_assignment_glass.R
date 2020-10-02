glass <- read.csv(file.choose())
View(glass)


# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(glass$Type))*100,1)

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}


#Apply the normalization function to wbcd dataset
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)

#create training and test datasets
#glass_train <- glass_n[1:151 ,]
#glass_test <- glass_n[151:214,]

g1<- glass_n[1:50, ]
g2<- glass_n[71:120,]
g3<-glass_n[147:155,]
g4<-glass_n[164:170,]
g5<-glass_n[177:182,]
g6<-glass_n[186:200,]
glass_train<-rbind(g1,g2,g3,g4,g5,g6)

g7<- glass_n[51:70, ]
g8<- glass_n[121:146, ]
g9<- glass_n[156:163, ]
g10<- glass_n[171:176, ]
g11<- glass_n[183:185, ]
g12<- glass_n[201:214, ]

glass_test<-rbind(g7,g8,g9,g10,g11,g12)
#Get labels for training and test datasets

 g13<- rbind (glass[1:50, ],
                             glass[71:120,],
                             glass[147:155,],
                             glass[164:170,],
                             glass[177:182,],
                             glass[186:200,])
 g14<- rbind (glass[51:70, ],
                            glass[121:146, ],
                            glass[156:163, ],
                            glass[171:176, ],
                            glass[183:185, ],
                            glass[201:214, ])
glass_train_labels<-g13[,10]
glass_test_labels<-g14[,10]



install.packages("class")
library("class")

# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_glass_pred <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==glass_train_labels))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==glass_test_labels))
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


glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)

install.packages("gmodels")
library("gmodels")

CrossTable(x=glass_test_labels , y= glass_pred ,prop.chisq=FALSE)

install.packages("sjPlot")
library(sjPlot)
sjc.elbow(glass_n)
          