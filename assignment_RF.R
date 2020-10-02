# Using Random Forest
install.packages("randomForest")
library(randomForest)
data(iris)
View(iris)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) 

# Prediction of train data
pred_train <- predict(fit.forest,iris_train)
library(caret)



# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species)



# Visualization 
windows()
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

library("gmodels")
CrossTable(x=iris_test$Species , y= pred_test  ,prop.chisq=FALSE)
