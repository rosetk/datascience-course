

install.packages("xlsx")
library(xlsx)
# for assignment1

#input<-read.xlsx("D:/datascience/assignment/clustering/EastWestAirlines.xlsx",2)
#normalised_data<-scale(input[,2:11])

# for assignment2

input<-read.csv("D:/datascience/assignment/clustering/crime_data.csv",1)
normalised_data<-scale(input[,2:5])




d<-dist(normalised_data,method="euclidean")
#d<-dist(normalised_data,method="maximum")
#d<-dist(normalised_data,method="manhattan")
#d<-dist(normalised_data,method="canberra")
#d<-dist(normalised_data,method="binary")
#d<-dist(normalised_data,method="minkowski")

fit<-hclust(d,method="complete")
#?hclust
plot(fit)
plot(fit,hang=-1 )
groups<-cutree(fit,k=5)
#?cutree
rect.hclust(fit,k=5,border="red")
#?rect.hclust
groups
membership<-as.matrix(groups)
final<-data.frame(input,membership)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
?write.xlsx
write.xlsx(final1,file="final1.xlsx")
getwd()