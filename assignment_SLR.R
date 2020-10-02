calories <- read.csv(file.choose()) 

# Exploratory data analysis
summary(calories )

attach(calories)
#Measures of Dispersion
var(Calories.Consumed )
var(Weight.gained..grams.)
sd(Calories.Consumed )
sd(Weight.gained..grams.)


install.packages("moments")
library(moments)

#Measures of skewness
skewness(Calories.Consumed )
skewness(Weight.gained..grams.)


#Measures of Kurtosis 
kurtosis(Calories.Consumed )
kurtosis(Weight.gained..grams.)
#Scatter plot
plot(calories$Weight.gained..grams., calories$Calories.Consumed)  
plot(calories$Calories.,calories$Weight.gained..grams. )
attach(calories)

#boxplot
boxplot(Calories.Consumed ,horizontal= TRUE,xlab="Calories Consumed" )
boxplot(Weight.gained..grams.,horizontal= TRUE , xlab ="Weight gained in grams" )

#histogram
hist(Calories.Consumed)
hist(Weight.gained..grams.)

#Correlation Coefficient (r)
cor(Weight.gained..grams., Calories.Consumed)    


# Simple Linear Regression model
reg <- lm( Calories.Consumed ~Weight.gained..grams.)  

pred<-predict(reg)
summary(reg)
confint(reg,level=0.95)
predict(reg,interval="predict")


reg$residuals
sum(reg$residuals)

mean(reg$residuals)
rmse_linear=sqrt(sum(reg$residuals^2)/nrow(calories)) #RMSE

hist(reg$residuals)


# ggplot for adding regresion line for data
library(ggplot2)


ggplot(data = calories, aes(x = Weight.gained..grams., y = Calories.Consumed)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories, aes(x=Weight.gained..grams., y=pred))

# Regression using logarithmic transformation
reg_log <- lm( Calories.Consumed ~ log(Weight.gained..grams.))

summary(reg_log)
predict(reg_log)

reg_log$residuals
rmse_log=sqrt(sum(reg_log$residuals^2)/nrow(calories))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

hist(reg_log$residuals)

# Exponential Model
reg_exp <- lm(log(Calories.Consumed) ~ Weight.gained..grams.)
summary(reg_exp)
predict(reg_exp)
reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logconsumed <- predict(reg_exp)
consumed <- exp(logconsumed)

error = calories$Calories.Consumed - consumed
error

rmse_expo=sqrt(sum(error^2)/nrow(calories))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

hist(reg_exp$residuals)

#Polynomial model with 2 degree (quadratic model)
plot(Calories.Consumed, Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)

cor(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)

plot(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

cor(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

reg2degree <- lm(log(Weight.gained..grams.) ~ Calories.Consumed + I(Calories.Consumed*Calories.Consumed))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = calories$Calories.Consumed - expy

rmse_Quad=sqrt(sum(err^2)/nrow(calories))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_log"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_log))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

