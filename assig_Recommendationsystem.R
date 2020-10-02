install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
library("recommenderlab")
library(caTools)

bookrec1<- read.csv(file.choose())
bookrec<-bookrec[-1]
str(bookrec)
hist(bookrec$Book.Rating)


#the datatype should be realRatingMatrix inorder to build recommendation engine
bookrec_matrix <- as(bookrec, 'realRatingMatrix')

#Popularity based 

book_recomm_model1 <- Recommender(bookrec_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, bookrec_matrix[413:414], n=5)
as(recommended_items1, "list")


## Popularity model recommends the same book for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book_recomm_model2 <- Recommender(bookrec_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(book_recomm_model2, bookrec_matrix[413:414], n=5)
as(recommended_items2, "list")

