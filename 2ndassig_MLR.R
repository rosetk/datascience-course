library(plyr)

Startups <- read.csv(file.choose())
View(Startups)
class(Startups)

Startups$State <- revalue(Startups$State,
                          c("New York"="0", "California"="1", "Florida"="2")) 
attach(Startups)
Startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)


Startups <- as.data.frame(Startups)

attach(Startups)

summary(Startups)

windows()
pairs(Startups)

cor(Startups)
library(corpcor)
cor2pcor(cor(Startups))

Model.Startups <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(Model.Startups)

Model.Startups1 <- lm(Profit~RD_Spend+log(Administration))
summary(Model.Startups1)


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
pairs(Startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


library(car)
influence.measures(Model.Startups)
windows()
influenceIndexPlot(Model.Startups,id.n=3)
influencePlot(Model.Startups,id.n=3)

Model.Startups_Log<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=Startups[-c(49,50),]) 

summary(Model.Startups_Log) #Adjusted R2 Value = 0.9591  
confint(Model.Startups_Log,level=0.95)
predict(Model.Startups_Log,interval="predict")

Model.Startups_Fin1<-lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=Startups[-c(49,50),])
summary(Model.Startups_Fin1) # Adjusted R2 Value is 0.9567
# Exponential Transformation :
Model.Startups_exp<-lm(log(Profit)~RD_Spend+Administration+Marketing_Spend+State,data=Startups[-c(49,50),])
summary(Model.Startups_exp)  #Adjusted R2 Value is 0.9182
Model.Startups_exp1<-lm(log(Profit)~RD_Spend+Marketing_Spend,data=Startups[-c(49,50),])
summary(Model.Startups_exp1) 

# Quad Model
Model.Startups_Quad <- lm(Profit~RD_Spend+I(RD_Spend^2)+Administration+I(Administration^2)
                          +Marketing_Spend+I(Marketing_Spend^2)+State+I(State^2),data=Startups[-c(49,50),])
summary(Model.Startups_Quad)  #Adjusted R2 value is 0.9567
confint(Model.Startups_Quad,level=0.95)

predict(Model.Startups_Quad,interval="predict")
Model.Startups_Quad1 <- lm(Profit~RD_Spend+I(RD_Spend^2)+Marketing_Spend+I(Marketing_Spend^2)
                           ,data=Startups[-c(49,50),])
summary(Model.Startups_Quad1)  #Adjusted R2 value is 0.9567

# Poly Modal
Model.Startups_Poly <- lm(Profit~RD_Spend+I(RD_Spend^2)+I(RD_Spend^3)+
                            Administration+I(Administration^2)+I(Administration^3)+
                            Marketing_Spend+I(Marketing_Spend^2)+I(Marketing_Spend^3)+
                            State+I(State^2)+I(State^3),data=Startups[-c(49,50),])
summary(Model.Startups_Poly) #Adjusted R Square Value is 0.9569


Model.Startups_Poly1 <- lm(Profit~RD_Spend+I(RD_Spend^2)+I(RD_Spend^3)+
                             Marketing_Spend+I(Marketing_Spend^2)+I(Marketing_Spend^3)
                           ,data=Startups[-c(49,50),])
summary(Model.Startups_Poly1) #Adjusted R Square Value is 0.9601

vif(Model.Startups_Log)
avPlots(Model.Startups_Log, id.n=2, id.cex=0.7) # Added Variable Plots

# Final Model
FinalModel<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+
                 log(State),data=Startups[-c(49,50),])

summary(FinalModel) #Adjusted R2 Value = 0.9591 

Profit_Predict <- predict(FinalModel,interval="predict")

Final <- cbind(Startups$RD_Spend,Startups$Administration,Startups$Marketing_Spend,
               Startups$State,Startups$Profit,Profit_Predict)
View(Final)
plot(FinalModel)


