library(tidyverse)
library(ISLR)
library(boot)
set.seed(1)

#THE VALIDATION SET APPROACH
#sample without replacement from numbers 1:392
train <- sample(392,196)

lm_model <- lm(mpg~horsepower, data = Auto , subset = train)
#Calculate MSE = average of (Actual - predicted)^2
mse_lm_model <-   (Auto[-train,"mpg"] - predict(lm_model , newdata=Auto[-train,]))^2 %>% mean()

#Calculate MSE of polynomial regression
poly2_model <- lm(mpg~poly(horsepower,2), data = Auto , subset = train)
mse_poly2_model <-   (Auto[-train,"mpg"] - predict(poly2_model , newdata=Auto[-train,]))^2 %>% mean()

poly3_model <- lm(mpg~poly(horsepower,3), data = Auto , subset = train)
mse_poly3_model <-   (Auto[-train,"mpg"] - predict(poly3_model , newdata=Auto[-train,]))^2 %>% mean()

#NOTE. It is advisable to use Poly(x , power) rather than I(x^power) or x^power when specifying the model formula
#Read notes on orthogonal polynomials for regression


#LEAVE OUT ONE CROSS VALIDATION (LOOCV)

#note lm ~ glm if no family argument is passed to glm

glm_fit <- glm(mpg~horsepower , data = Auto)

cv_error <-  cv.glm(data = Auto , glm_fit)

#cv.glm allows you to calculate cross validation MSE
# cverror$delta contains the estimated test MSE which is the average of errors for each prediction
#We will explore a case later where those two numbers will differ

cv_error <- numeric(5)
for (i in 1:5){
  
  glm_fit <- glm(mpg~poly(horsepower,i) , data = Auto)
  cv_error[i] <- cv.glm(data = Auto , glm_fit)$delta[1]
}

#NOTE. MSE estimates will ALWAYS be the same because of LOOCV, you are always going to be testing the same
#models against the same points. Therefore no variability in estimates




#K-FOLD CROSS VALIDATION
# cv.glm can also do K-fold, if you don't set the k parameter, it will set K=N , which is LOOCV

set.seed(17)
cv_error <- numeric(10)
for (i in 1:10){
  
  glm_fit <- glm(mpg~poly(horsepower,i) , data = Auto)
  cv_error[i] <- cv.glm(data = Auto , glm_fit, K = 10)$delta[1]
}

#Much faster computation than LOOCV, also has the perks of a more accurate estimate of MSE
# even though it will have slightly more bias, it reduces variance because instead of averaging MSE_i's of one point each
# as in LOOCV , you average MSE_k's which in itself, is an average, so it is an average of averages.

#Note. the two delta values differe slightly, first one is the standard K-fold MSE estimate, the other is a bias corrected estimate



#BOOTSTRAP
alpha_fn <- function( data , index){ #The metric you want to bootstrap
  
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y)- cov(X,Y)) / (var(X)+var(Y)-2*cov(X,Y)) )
  
}

#estimates allpha value or a portfolio allocation percentage to be optimal
set.seed(1)
alpha_fn(Portfolio,sample(100,100,replace = T))


#Now let's bootstrap the above process in order to get a robust CI for alpha
boot(Portfolio , alpha_fn , R=1000)
#SHows SE(alpha) = 0.0886 and estimate for alpha is 0.57583


#Let's apply this thinking to linear regression coefficient estimates compared to the analytical formulae for OLS estimation
boot_fn <- function(data , index){
  return({
    coef(lm(mpg~horsepower, data= data , subset = index))
  })
}

boot_fn(Auto , sample(392,392, replace = TRUE))

#Bootstrap this
boot(data= Auto , boot_fn , R = 1000)

#compare this against estimates calculated from formula in the lm function
summary(lm(mpg~horsepower,data=Auto))$coef

#You can see that there is actually quite a bit of a difference in the standard error estimation. Why?
#Standard formulae rely on the sigma^2 estimate (noise variance) being accurate.It is onyl accurate if the model selection



# of it being linear is correct. In fact, there is a non-linear relationship so this estimate is over-inflated. Therefore the bootstrap is more accurate in this case.



#From textbook questions, the probability that any arbitrary O_j observation will be in a bootstrap resample of size n is
# 1 - (1- 1/n)^n
bootstrap_probs <- data_frame(n=1:100000,probability = numeric(100000))
bootstrap_probs$probability <- 1 - (1- 1/(bootstrap_probs$n) )^(bootstrap_probs$n)
ggplot(bootstrap_probs,aes(x=n , y=probability)) +ylim(0,1)+ geom_point()

#Notice how it quickly approaches the asymptote