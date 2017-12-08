library(tidyverse)
library(ISLR)
library(gvlma) #found this online. Adds additional diagnostic plots
#Question 8-----------
lm_model <-  lm(data = Auto , mpg ~ horsepower)
summary(lm_model)

#60.59% of variation is explained, can see a relationship between response and predictor
#negative relationship
#Predict mpg when horsepower of 98
predict(lm_model , newdata = data_frame(horsepower = 98))
#Prediction of 24.47mpg
#Confidence Interval vs Prediction Interval
#Prediction Interval this is for Y use rmse
predict(lm_model , newdata = data_frame(horsepower = 98) , interval ="prediction")
#Confidence Interval this is for Y_bar threfore use rmse / sqrt(n)
predict(lm_model , newdata = data_frame(horsepower = 98) , interval ="confidence")

#plot results
plot( x = Auto$horsepower , y =Auto$mpg)
abline(lm_model , col = "red")

#Look at diagnostic plots
plot(lm_model)
#Plot1: non linear relatioshiop of residuals to values
#plot2: divergencefrom normality


diagnostics <- gvlma::gvlma(lm_model)
summary(diagnostics)
plot(diagnostics)

#Question 9---------
pairs(Auto[,-9])
cor(Auto[,-9])

lm_fit_2 <- lm(data = Auto , mpg ~ . -name)
summary(lm_fit_2)
#Seems like there is overall model significance to null model by F-statistic and corroborated by R squared
# All predictors but acceleration, cylinders and horsepower seem signifcant for LINEAR model
#Coefficient for year suggests that the more recent the make, the greater the mpg will be

#Remember, a point has to have a mix of high leverage and outlier-ness to be INFLUENTIAL
#Let's look at diagnostic plots
par(mfrow=c(2,2))
plot(lm_fit_2)
diagnostics_2 <- (gvlma(lm_fit_2))
diagnostics_2
#you can see heteroskedasticity
#non linear residuals  and a high leverage point 14 but it is not an outlier

lm_fit_2_interactions <- lm(mpg~cylinders*displacement+displacement*weight , data = Auto)
#By looking at correlation matrix, try these combinations
#Also have a look at the notes on formula notation in R
summary(lm_fit_2_interactions)
