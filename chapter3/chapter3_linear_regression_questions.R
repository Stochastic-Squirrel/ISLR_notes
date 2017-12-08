library(tidyverse)
library(ISLR)
library(gvlma) #found this online. Adds additional diagnostic plots
library(boot)
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


# We can clearly see non-normal relationship betwen mpg and regressors. Let's try a log transformation
lm_fit_2_log <- lm(log(mpg)~cylinders*displacement+displacement*weight , data = Auto)
summary(lm_fit_2_log)


#Is this better than the interactions only model?
#better fit as per adjusted r_squared value
#other common transformations is sqqrt and polynomial


#Question 10---------

carseats_model <- lm(data = Carseats , Sales ~ Price + Urban  + US)
summary(carseats_model)
#Carseats sales decrease on average by 0.054 with a one dollar increase in price
# In urban areas, you sell fewer carseats (although this coefficient is not significant)
#In the US, you sell more carseats than the rest of the world

#from coefficients, only price and US seem siginificant
carseats_improved <-  lm(data = Carseats , Sales ~ Price   + US)
#Get 95% confidence intervals for coefficients 
confint(carseats_improved)
#Compare to bootstrap
boot_fn <- function(data , index){
  return({
    coef(lm(data = data , Sales ~ Price   + US , subset= index))
  })
}
boot(Carseats, boot_fn , R =1000)

#Close results
par('mar')
par(mar=c(1.5,1.5,1.5,1.5)) #reduces margin size
plot(gvlma(carseats_improved))
#Standardised residuals are between -3 and 3 so no sign of major outliers. We would worry if absolute magnitude greater than 3 (proxy to 3 standard deviations for normal dist)
#Question 11 --------

#Investigate the t-statistic for individual beta coefficients
set.seed(1)
x <- rnorm(100)
y <- 2*x + rnorm(100)
dat <- data_frame(x=x,y=y)

y_onto_x <- lm (data = dat , y~x -1) #without intercept
summary(y_onto_x)

x_onto_y<- lm (data = dat , x~y -1) #without intercept
summary(x_onto_y)

#Results show that the same line is being drawn, subject of formula is just different
