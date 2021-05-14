install.packages("questionr")
library(questionr)

prsa=read.csv("T:/Program Files/School/Senior/S2/Stat 443/Consulting Project/daily_data.csv",header=TRUE, row.names=1)

#********************************************
# add day of a week

prsa$weekday=weekdays(as.Date(paste(prsa$year, "-",prsa$month,"-",prsa$day, sep='')))
prsa$weekday[which(prsa$weekday=="Monday")]=1
prsa$weekday[which(prsa$weekday=="Tuesday")]=2
prsa$weekday[which(prsa$weekday=="Wednesday")]=3
prsa$weekday[which(prsa$weekday=="Thursday")]=4
prsa$weekday[which(prsa$weekday=="Friday")]=5
prsa$weekday[which(prsa$weekday=="Saturday")]=6
prsa$weekday[which(prsa$weekday=="Sunday")]=7

week_mean=numeric(7)
for (i in 1:7){
  week_mean[i]=mean(prsa$PM2.5[which(prsa$weekday==i)])
}

plot(1:7, week_mean, type='l')

hr=0:23
hour_mean=numeric(24)
for (i in 1:24){
  hour_mean[i]=mean(prsa$PM2.5[which(prsa$hour==hr[i])])
}

plot(0:23, hour_mean, type='l')

#********************************************
# polution and pressure
plot(prsa$PRES, prsa$PM2.5, main="PM2.5 VS Pressure") # no significant relationship
plot(prsa$PRES, prsa$PM10, main="PM10 VS Pressure") # no significant relationship

#*********************************************
# add previous day's PM2.5
for (i in 2:nrow(prsa)){
  prsa$Previous.PM2.5[i]=prsa$PM2.5[i-1]
}

prsa$PM2.5=exp(prsa$PM2.5)
prsa$Previous.PM2.5=exp(prsa$Previous.PM2.5)
prsa=prsa[-1,]
linear.model=lm(PM2.5~PM10 + CO + NO2 + SO2 + WSPM + DEWP + Month + Previous.PM2.5 + Weekday, data=prsa)
linear.model
summary(linear.model) 


# pm10 and CO
model2=lm(PM10~CO, data=prsa)
summary(model2) #significant relationship between PM10 and CO, R-squared=0.5232
plot(prsa$CO, prsa$PM10, main="????????????") # obvious trend - positively correlated
abline(model2, col="red", lwd=2)

#**********************************************
#pollution and SO2
model3=lm(PM2.5~SO2, data=prsa)
summary(model3) #some relationship between so2 and pm2.5, but with low R-squared
plot(prsa$SO2, prsa$PM2.5, main="PM2.5 VS Sulfur Dioxide")
abline(model3, col="red", lwd=2)

# pm10 and SO2
model4=lm(PM10~SO2, data=prsa)
summary(model4) #some relationsip between SO2 and pm10, but with low R-squared
plot(prsa$SO2, prsa$PM10, main="PM10 VS Sulfur Dioxide") 
abline(model4, col="red", lwd=2)

#********************************************************
model5=lm(PM2.5~NO2, data=prsa)
summary(model5) #significant relationship between PM2.5 and NO2
plot(prsa$NO2, prsa$PM2.5, main="PM2.5 VS Nitrogen Dioxide")
abline(model5, col="red", lwd=2)

# pm10 and SO2
model6=lm(PM10~NO2, data=prsa)
summary(model6) #significant relationship between PM2.5 and NO2
plot(prsa$NO2, prsa$PM10, main="PM10 VS Nitrogen Dioxide", col="black")
abline(model6, col="red", lwd=2)

#**********************************************************************
model7=lm(PM2.5~O3, data=prsa)
summary(model7)
plot(prsa$O3, prsa$PM2.5, main= "pm2.5 vs Ozone")
#abline(model7, col="red", lwd=2)

model8=lm(PM10~O3, data=prsa)
summary(model8)
plot(prsa$O3, prsa$PM10, main= "pm10 vs Ozone")
#abline(model7, col="red", lwd=2)

#compare the correlation between different toxic gases and the pollution
library(ggplot2)
r.squared_PM2.5=c(summary(model1)$r.squared,summary(model3)$r.squared,summary(model5)$r.squared,summary(model7)$r.squared)
df_PM2.5=data.frame(ToxicGas=c("CO","SO2","NO2","O3"), R_Squared=r.squared_PM2.5)
ggplot(data=df_PM2.5, aes(x=ToxicGas, y=R_Squared)) +ggtitle("PM2.5 VS Toxic Gas")+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=round(R_Squared,2)),vjust=-0.3, color="white", size=3.5)+
  theme(plot.title = element_text(hjust = 0.5))

r.squared_PM10=c(summary(model2)$r.squared,summary(model4)$r.squared,summary(model6)$r.squared,summary(model8)$r.squared)
df_PM10=data.frame(ToxicGas=c("CO","SO2","NO2","O3"), R_Squared=r.squared_PM10 )
ggplot(data=df_PM10, aes(x=ToxicGas, y=R_Squared)) + ggtitle("PM10 VS Toxic Gas")+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=round(R_Squared,2)),vjust=-0.3, color="white", size=3.5)+
  theme(plot.title = element_text(hjust = 0.5))

####################################################
install.packages("olsrr")
install.packages('MLmetrics')
install.packages('tidyverse')
install.packages('caret')
install.packages('leaps')


library(olsrr)
library(MLmetrics)
library(tidyverse)
library(caret)
library(leaps)

# FULL MODEL r=0.7088
fullmodel=lm(PM2.5~year+month+day+hour+SO2+NO2+CO+O3+TEMP+PRES+DEWP+RAIN+WSPM+season, data=prsa)
summary(fullmodel)

# DETECT RMSE (Root Mean Squared Error) of full model = 45.9312
RMSE(prsa$PM2.5,fullmodel$fitted.values)

######################################################FULL MODEL DIAGNOSTIC
# ACTUAL VS FITTED LINE
plot(fullmodel)
# From the Residuals vs Fitted plot and Scale-Location plot, this is a homoscedastic linear model with normally distributed errors.
# From the Normal Q-Q plot, the dependent variable is normally distributed, except for some outliers in left and right tails.
# From the Residuals vs Leverage point, there is no high leverage point.

# EXAMINE COLLINEARITY -- VIF
#Variance inflation factors measure the inflation 
#in the variances of the parameter estimates due to collinearities 
#that exist among the predictors. It is a measure of how much the variance 
#of the estimated regression coefficient ??k is "inflated" by the existence 
#of correlation among the predictor variables in the model. A VIF of 1 means 
#that there is no correlation among the kth predictor and the remaining 
#predictor variables, and hence the variance of ??k is not inflated at all. 
#The general rule of thumb is that VIFs exceeding 4 warrant further investigation, 
#while VIFs exceeding 10 are signs of serious multicollinearity requiring correction.
ols_vif_tol(fullmodel)

#Results
#VIF of temperature = 9.32; VIF(PRES) = 4.81, VIF(DEWP)=7.13 ???

# detect correlation among variables -- correlation matrix
# correlation threshold:A correlation coefficient of . 10 is thought to 
#represent a weak or small association; a correlation coefficient of . 30 
#is considered a moderate correlation; and a correlation coefficient of
#. 50 or larger is thought to represent a strong or large correlation
cor(model.matrix(fullmodel)[,-1])

#Results
# moderate correlation between SO2 and NO2
# large correlation between CO and SO2
# large correlation between O3 and NO2, o3 and TEMP
# moderate correlation between SO2 and TEMP
# Extremely large correlation between DEWP and TEMP

#-------------------------------------------------------------------
# log transformation of PM2.5 (failed)
model_log=lm(log(PM2.5)~year+month+day+hour+SO2+NO2+CO+O3+TEMP+PRES+DEWP+RAIN+WSPM+season, data=prsa)
summary(model_log) #R2=0.6729
RMSE(prsa$PM2.5, model_log$fitted.values) #RMSE=117.0349 

#-------------------------------------------------------------------
# AIC, BIC of full model
# subset selection -- backward elimination
model_aic=step(fullmodel, direction = "backward")
summary(model_aic) #R2=0.7088
plot(model_aic)
RMSE(prsa$PM2.5,model_aic$fitted.values) #RMSE=45.93198 -- AIC

model_bic=step(fullmodel, k=log(nrow(prsa)), direction='backward')
summary(model_bic) #R2=0.7088
plot(model_bic)
RMSE(prsa$PM2.5,model_bic$fitted.values) #RMSE=45.93519 -- BIC

#-----------------------------------------------------------
# LASSO RIDGE
library(glmnet)
model_ridge=glmnet(model.matrix(fullmodel)[,-1], prsa$PM2.5, alpha=1)
model_lasso=glmnet(model.matrix(fullmodel)[,-1], prsa$PM2.5, alpha=0)
# 10 fold cross validation
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(model.matrix(fullmodel)[,-1], prsa$PM2.5, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}
plot(fit0, main="Ridge")
plot(fit10, main='LASSO')
# MSE on test set
yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=model.matrix(fullmodel)[,-1])
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=model.matrix(fullmodel)[,-1])
mse0 <- sqrt(mean((prsa$PM2.5 - yhat0)^2)) # RMSE=46.98539
mse10 <- sqrt(mean((prsa$PM2.5 - yhat10)^2)) #RMSE=46.47515

#---------------------------------------------------------------
# Random Forest though python
library(randomForest) 
model_random=randomForest(PM2.5~year+month+day+hour+SO2+NO2+CO+O3+TEMP+PRES+DEWP+RAIN+WSPM+season, data=prsa, importance=TRUE, proximity=TRUE)
# RMSE = 9.3662

# Decision Tree through python
# RMSE=3.0032075666594618e-15


########
# add day of a week

prsa$weekday=weekdays(as.Date(paste(prsa$year, "-",prsa$month,"-",prsa$day, sep='')))
prsa$weekday[which(prsa$weekday=="Monday")]=1
prsa$weekday[which(prsa$weekday=="Tuesday")]=2
prsa$weekday[which(prsa$weekday=="Wednesday")]=3
prsa$weekday[which(prsa$weekday=="Thursday")]=4
prsa$weekday[which(prsa$weekday=="Friday")]=5
prsa$weekday[which(prsa$weekday=="Saturday")]=6
prsa$weekday[which(prsa$weekday=="Sunday")]=7

week_mean=numeric(7)
for (i in 1:7){
  week_mean[i]=mean(prsa$PM2.5[which(prsa$weekday==i)])
}

plot(1:7, week_mean, type='l')

hr=0:23
hour_mean=numeric(24)
for (i in 1:24){
  hour_mean[i]=mean(prsa$PM2.5[which(prsa$hour==hr[i])])
}

plot(0:23, hour_mean, type='l')
