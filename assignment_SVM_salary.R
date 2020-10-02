
salary_train<-read.csv(file.choose())
salary_test<-read.csv(file.choose())

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

#salary_train_norm<-as.data.frame(lapply(salary_train,normalize))
#salary_test_norm<-as.data.frame(lapply(salary_test,normalize))

#train = cbind(salary_train_norm,salary_train[,14])
#test = cbind(salary_test_norm,salary_test[,14])

library(kernlab)
library(caret)


# kernel = vanilladot
model_vanilla<-ksvm(Salary ~.,data = salary_train_norm,kernel = "polydot")
pred_vanilla<-predict(model_vanilla,newdata=salary_test_norm)
mean(pred_vanilla==salary_test_norm$Salary) 
table(pred_vanilla,salary_test_norm$Salary)
agreement <- pred_vanilla == salary_test_norm$Salary
table(agreement)
prop.table(table(agreement))

