library(tidyverse)
library(ISLR)
library(gvlma) #found this online. Adds additional diagnostic plots
library(boot)
library(MASS)
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
#Question 12 --------

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
#Note Y~X same as X~Y without an intercept when sum of squares are the same

set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
#Different coefficient estimates

set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
sum(x^2)
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)

#Same coefficient estimates when the sum of squares are the same
#Question 13 --------
set.seed(1)
x <- rnorm(100)
eps <- rnorm(mean = 0 , sd = sqrt(0.25) , n =100)
y <- -1 + 0.5*x + eps
#True bo would be -1 and b_1 would be 0.5
plot(x,y)
lm_fit <- lm(y~x)
summary(lm_fit)m 
#Estimated coefficients are pretty close to the true values. This is because true relationship is linear so lm does a good job approximating the true funcitonal form f
abline(lm_fit , col =5)
abline(a = -1 , b =0.5 , col =6)
legend(-1,legend = c("model fit", "pop. regression"), col=5:6, lwd=3)

polynomial_fit <- lm(y ~ poly(x , 2))
summary(polynomial_fit)#Adjusted R squares is 46.72 % and polynomial term not significant
#compare against linear model
summary(lm_fit) # r squared same at 46.19%
anova(lm_fit,polynomial_fit) # Anova between models confirms that polynomial model not better


#NOW, let's fit a linear model again, but decrease the noise or epsilon term
eps_low_noise <- rnorm(mean = 0 , sd = sqrt(0.05) , n =100)
y_low_noise <- -1 + 0.5*x + eps_low_noise
eps_high_noise <- rnorm(mean = 0 , sd = sqrt(1.5) , n =100)
y_high_noise <- -1 + 0.5*x + eps_high_noise

lm_fit_low_noise <- lm(y_low_noise~x)
lm_fit_high_noise <- lm(y_high_noise~x)

plot(x,y , main="Comparison of different LM under different irreducible noise")
abline(lm_fit , col =1)
abline(lm_fit_low_noise , col = 2)
abline(lm_fit_high_noise , col = 3)
abline(a = -1 , b =0.5 , col =6)
legend(-1,legend = c("medium_noise","low_noise","high_noise","population_relation"), col=c(1,2,3,6), lwd=3)


#Let's see how noise affects the CI 
models <- list(lm_fit, lm_fit_low_noise , lm_fit_high_noise)
map(models, confint)
#CI for highest noise model much wider, interestingly the upper end is much higher 
#than the others but the lower end is fine.

#Question 14 --------

#LOOK AT COLLINEARITY
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
#Y  = 2 +2x1 +.3x2
cor(x1,x2)
plot(x1~x2)
#Very high collinearity
lm_collinear <- lm(y~x1+x2)
summary(lm_collinear)
# x1 significant x2 not signifincat
# estimates are prettyfar off from true population, betas
#This is because the betas are calculated GIVEN that other variables are held constant
#This is why collinearity is a problem
lm_x2 <- lm(y~x2)
summary(lm_x2)
lm_x1 <- lm(y~x1)
summary(lm_x1)
#They are very siginicant on their own, interstingly still far off from population values
#This is not contradictory
#On there own, they are very good predictors, but since they have high collinearity
#it is difficult to separate the individual affects of them  by themselves.


#Question 15 ---------
summary(Boston)
pairs(Boston)
poly <- lm(crim ~ poly(dis,3) , data = Boston) 
plot(Boston$dis , Boston$crim)
