library(ISLR)
library(tidyverse)
library(boot)
library(gam)
library(MASS)
library(splines)
#Question6---------

#USE CV to determine appropriate polynomial degree to model wage ~ age
set.seed(1)
cv_errors <- numeric(10)
for(i in 1:10){
poly_fit <- glm(data = Wage , wage ~poly(age,i))
cv_errors[i] <- cv.glm(data = Wage , glmfit = poly_fit , K=10)$delta[2] #second item is bias reduced version
}
#plot errors
plot(x = 1:10 , y=cv_errors , main="CV errors of degree i polynomial regression" , xlab = "degree polynomial", pch=20, lwd=2, ylim=c(1590, 1700))
#get min
which.min(cv_errors)
#degree 4 is the best in this case
#Since they are all very similar though, we use the one standard error rule =  pick simplest model within
#one standard deviation of MIN value
sd_of_cv <- sd(cv_errors)
min_cv <- cv_errors[which.min(cv_errors)]
abline(h= min_cv + 0.2*sd_of_cv , col="red" , lty="dashed")
abline(h= min_cv - .2*sd_of_cv , col="red" , lty="dashed")
legend("topright", "0.2 SD lines from MIN poiint" , lty="dashed",col="red")

#polynomial degree 3 is simplest model within these bands (very narrow bands actually at 0.2 * SD)


#Let's use F-tests

fit_1 <- lm(data = Wage , wage ~poly(age,1))
fit_2 <- lm(data = Wage , wage ~poly(age,2))
fit_3 <- lm(data = Wage , wage ~poly(age,3))
fit_4 <- lm(data = Wage , wage ~poly(age,4))
fit_5 <- lm(data = Wage , wage ~poly(age,5))
fit_6 <- lm(data = Wage , wage ~poly(age,6))
fit_7 <- lm(data = Wage , wage ~poly(age,7))
fit_8 <- lm(data = Wage , wage ~poly(age,8))
fit_9 <- lm(data = Wage , wage ~poly(age,9))
fit_10 <- lm(data = Wage , wage ~poly(age,10))

anova(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6,fit_7,fit_8,fit_9,fit_10)
anova(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6,fit_7,fit_8,fit_9,fit_10 , test = "Cp")

#Plot fit
plot(Wage$age, Wage$wage , col ="darkgrey" , pch = 5 , cex = .2)
age_grid <- seq(range(Wage$age)[1],range(Wage$age)[2] )
predictions <- predict(fit_3,newdata=list(age=age_grid))
lines(age_grid,predictions,col='red')



#Let's try a step function
#Use cross validation to determine number of cuts or steps
set.seed(1)
cv_errors <- numeric(9)
for(i in 2:10){
  Wage$age_cut <- cut(Wage$age , i)
  poly_fit <- glm(data = Wage , wage ~age_cut)
  cv_errors[i] <- cv.glm(data = Wage , glmfit = poly_fit , K=10)$delta[2] #second item is bias reduced version
}
plot(2:10, cv_errors[-1], xlab="Number of cuts", ylab="CV error", type="l", pch=20, lwd=2)
which.min(cv_errors[-1])
# at 8 cuts
#plot function
lm.fit = glm(wage~cut(age, 8), data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.pred = predict(lm.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)

#Question7 ----------
#exlpore other variables of the wage data set such as marital status and job class
glimpse(Wage)
pairs(wage~maritl+race+region+education+health_ins,data=Wage, 
      main="Simple Scatterplot Matrix")
par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)
#all other variables are categorical, so we can try adding them to the gam
par(mfrow=c(1,3))
fit <- gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
plot(fit , se =T)

#Is it better than just smoothing spline?
spline_only <- gam(wage~s(age,4) ,data = Wage)
anova(fit,spline_only , test ="F")
#Substantial difference, therefore those variables improve the fit

deviance(fit)
#look at maritl variable
summary(Wage$maritl)
#only 19 widowed and 55 separated, little info try excluding them
par(mfrow=c(1,3))
subset_vector <- case_when(Wage$maritl=="3. Widowed" ~ FALSE,
                           Wage$maritl=="5. Separate" ~ FALSE,
                           TRUE ~ TRUE)
fit_exclude <- gam(wage ~ maritl + jobclass + s(age, 4), data = Wage , subset = subset_vector)
plot(fit_exclude , se =T)
deviance(fit_exclude)


#Question 8-----
#look at auto data set
plot(Auto)
#look at weight and acceleration rather than acceleration and horsepower as the latter pair has higher colinearity
#Try polynomial
auto_poly <- glm(data = Auto , mpg ~ poly(weight,3))
weight_range <- seq(range(Auto$weight)[1], range(Auto$weight)[2])
poly_predictions <- predict(auto_poly , newdata = list(weight=weight_range),se=T)
par(mfrow=c(1,2))
plot(Auto$weight,Auto$mpg , col="darkgrey" , main="3Rd degree polynomial")
lines(weight_range,poly_predictions$fit,col="red")
lines(weight_range,poly_predictions$fit + 2*poly_predictions$se,col="red",lty="dashed")
lines(weight_range,poly_predictions$fit - 2*poly_predictions$se,col="red",lty="dashed")

#try local regression
auto_loess <- loess(data=Auto,mpg~weight)
loess_predictions <- predict(auto_loess, newdata = data_frame(weight=weight_range),se=T)
plot(Auto$weight,Auto$mpg , col="darkgrey" , main="LOESS")
lines(weight_range,loess_predictions$fit,col="green")
lines(weight_range,loess_predictions$fit + 2*loess_predictions$se,col="green",lty="dashed")
lines(weight_range,loess_predictions$fit - 2*loess_predictions$se,col="green",lty="dashed")


#Try a gam with weight and acceleration
gam_fit <- gam(data = Auto , mpg ~ poly(weight,3) + s(acceleration,5))
plot(gam_fit)
#let's analyse the error of this model via cross validation
cv_errors <- cv.glm(gam_fit,K=10,data=Auto)
#The rmse error is
sqrt(cv_errors$delta[2])
#only slightly lower than just LOESS with weight...

#Question 9 ---------
poly_regression <- lm(dis ~ poly(nox,3) , data = Boston)
poly_regression
summary(poly_regression)
par(mfrow=c(1,1))
plot(Boston$nox,Boston$dis,col="darkgrey",main="3rd degree polynomial")
nox_range <- seq(range(Boston$nox)[1],range(Boston$nox)[2],length.out = 100)
lines(nox_range,predict(poly_regression,newdata = list(nox=nox_range)),col="blue")



#Fit polynomials up to degree 10
poly_fits <- map( 1:10, ~ lm(formula=dis~poly(nox,.),data=Boston) )
plot(Boston$nox,Boston$dis,col="darkgrey",main="1-10Degree Polynomial")
predictions <- map(poly_fits,predict,newdata=list(nox=nox_range))
map(predictions,lines,x=nox_range,col="blue")


#List rss of the polynomial fits
map(poly_fits, function(x) sum(x$residuals^2) ) %>% unlist()

#You can see How RSS decreases as poly goes up, this is to be expected and higher order poly will overfit
#USE aic or bic to choose best mdoel (lower is better)
bic_values <- map(poly_fits,BIC) %>% unlist()
bic_values
which.min(bic_values)
#3rd degree polynomial lowest BIC



#Use a regression spline now instead
spline_regression <- lm(dis~bs(nox,df=4),data=Boston)
summary(spline_regression)
plot(Boston$nox,Boston$dis,col="darkgrey",main="4 Degree Cubic spline")
lines(nox_range , predict(spline_regression,newdata = list(nox=nox_range)), col="red")
#knots at 1,3,12





poly_fits <- map( 1:10, ~ lm(formula=dis~poly(nox,.),data=Boston) )

