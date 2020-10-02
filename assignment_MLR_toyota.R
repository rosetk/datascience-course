Corolla <- read.csv(file.choose())
View(Corolla)
attach(Corolla)
Corolla_Pred <- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
Corolla_Pred1 <- as.data.frame(Corolla_Pred)
class(Corolla_Pred1)       

View(Corolla_Pred1)
attach(Corolla_Pred1)

summary(Corolla_Pred1)

#Measures of Dispersion
var(Corolla_Pred1)

sd( Price)
sd(Age_08_04)
sd(KM)
sd(HP)
sd(cc)
sd(Doors)
sd(Gears)
sd(Quarterly_Tax)
sd(Weight)


install.packages("moments")
library(moments)

#Measures of skewness
skewness(Corolla_Pred1 )



#Measures of Kurtosis 
kurtosis(Corolla_Pred1 )



#Find the correlation b/n Output (price) & input parameters -Scatter plot
windows()
pairs(Corolla_Pred1)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Corolla_Pred1)


### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla_Pred1))

# The Linear Model of interest with all the columns
model.Corolla <- lm(Price~.,data=Corolla_Pred1)

summary(model.Corolla)

### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Corolla_Pred1, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

# Deletion Diagnostics for identifying influential observations
influence.measures(model.Corolla)
library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(model.Corolla,id.n=3) # index plots for infuence measures
influencePlot(model.Corolla,id.n=3) # A user friendly representation of the above

# Regression after deleting the 81th observation, which is influential observation
model_2.Corolla <- lm(Price~.,data=Corolla_Pred1[-81,])
summary(model_2.Corolla)

#Variance inflation factor
vif(model.Corolla)

#Added variable plot
avPlots(model.Corolla , id.n=2 ,id.cex=0.7)

## Final model after deleting the factor Doors
model_3.Corolla<-lm(Price~.-Doors,data=Corolla_Pred1[-81,])
summary(model_3.Corolla)

# Evaluate model LINE assumptions 
plot(model_3.Corolla)

hist(residuals(model_3.Corolla))

pred<-predict(model_3.Corolla)
summary(model_3.Corolla)
confint(model_3.Corolla,level=0.95)
predict(model_3.Corolla,interval="predict")


model_3.Corolla$residuals
sum(model_3.Corolla$residuals)

mean(model_3.Corolla$residuals)
rmse_linear=sqrt(sum(model_3.Corolla$residuals^2)/nrow(Corolla_Pred1)) #RMSE

hist(model_3.Corolla$residuals)
qqnorm(model_3.Corolla$residuals)

# Regression using logarithmic transformation
reg_log<-lm(Price~ Age_08_04+KM+HP+cc-Doors+log(Gears)+log(Quarterly_Tax)+Weight,data=Corolla_Pred1[-81,])
summary(reg_log)
predict(reg_log)

reg_log$residuals
rmse_log=sqrt(sum(reg_log$residuals^2)/nrow(Corolla_Pred1))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
qqnorm(reg_log$residuals)

# Exponential Model
reg_exp <- lm(log(Price) ~ Age_08_04+KM+HP+cc-Doors+Gears+Quarterly_Tax+Weight,data=Corolla_Pred1[-81,])
summary(reg_exp)
predict(reg_exp)
reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logprice <- predict(reg_exp)
price <- exp(logprice)

error = Price - price
error

rmse_expo=sqrt(sum(error^2)/nrow(Corolla_Pred1))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")
qqnorm(reg_exp$residuals)

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_log"),c(rmse_linear,rmse_expo,rmse_log))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
