library(ISLR)
library(boot)
library(tidyverse)
library(MASS)


#Question 5 --------------
set.seed(1)
glm_fit <- glm(data = Default , default ~ income + balance , family = 'binomial')


#Validation set approach

validation_mse <- function(){
  #define train and test set
  train <- sample(dim(Default)[1] , dim(Default)[1]/2  %>% round(.,0) ) 
  test <- Default[-train, ]
  #train model on training set
  glm_fit <- glm(data = Default , default ~ income + balance , family = 'binomial' , subset = train)
  
  #predict on test set
  glm_predictions <- predict(glm_fit , newdata = test , type = "response")
  glm_predictions <-  case_when(glm_predictions>= 0.5 ~ "Yes",
                              TRUE ~ "No")
  test_error_rate <- mean(glm_predictions!= test$default)
  return(test_error_rate)
  
}

#after running validation_mse multiple times, the test MSE seems to hover around 2.6%

#Will adding another regressor of student help? let's see

validation_mse_student <- function(){
  #define train and test set
  train <- sample(dim(Default)[1] , dim(Default)[1]/2  %>% round(.,0) ) 
  test <- Default[-train, ]
  #train model on training set
  glm_fit <- glm(data = Default , default ~ income + balance + student, family = 'binomial' , subset = train)
  
  #predict on test set
  glm_predictions <- predict(glm_fit , newdata = test , type = "response")
  glm_predictions <-  case_when(glm_predictions>= 0.5 ~ "Yes",
                                TRUE ~ "No")
  test_error_rate <- mean(glm_predictions!= test$default)
  return(test_error_rate)
  
}

#Running the above funciton multiple times, seems like student regressor doesn't add to the model
#You would prefer a simpler model so don't add it into the final model




#Question 6 --------------
set.seed(1)
#Will use resampling to calculate variability of the beta coefficients of the glm model in the previous question
glm_fit <- glm(data = Default , default ~ income + balance , family = 'binomial')
#look at values for coefficients
summary(glm_fit)
boot_fn <- function(data , index){
  glm_fit <- glm(data = data , default ~ income + balance , family = 'binomial' , subset = index)
  return(glm_fit$coefficients)
}
boot_results <- boot(Default, boot_fn , R=1000 ,parallel = "snow")
summary(glm_fit)
#very similar results
#Question 7----------

#The cv.glm function could be replaced by glm , predict() and a for loop with train/test sets

#Doing LOOCV MANUALLY NOW
glm_fit <- glm(data = Weekly , Direction ~ Lag1 + Lag2 , family = "binomial")
glm_fit_less1 <- glm(data = Weekly[-1,] , Direction ~ Lag1 + Lag2 , family = "binomial")

#Use the second model to predict the direction or Y variable for the FIRST observation (This observation was not included in training set)
predict(glm_fit_less1 , newdata = Weekly[1,] , type = "response") > 0.5
#predicts it as UP but true value was DOWn

#Now perform above process for all N points, thus conducting LOOCV regression

n <- dim(Weekly)[1]
#define vector to record if there was a prediction error
error <- numeric(n)
for ( i in 1:n){
  
  glm_fit_less1 <- glm(data = Weekly[-i,] , Direction ~ Lag1 + Lag2 , family = "binomial")
  prediction <- predict(glm_fit_less1 , newdata = Weekly[i,] , type = "response") > 0.5
  actual <- case_when(Weekly[i,"Direction"]=="Up" ~ TRUE,
                      TRUE ~ FALSE)
  if(actual==prediction){
    error[i] <- 0
  }else{
    error[i] <- 1
  }
  
}
mse_loocv <- mean(error)
#Error rate of 45%
#Question 8 -----------

#Simulate data
set.seed(1)
x <- rnorm(100)
y <-  x -2*x^2 + rnorm(100)
dat <- data_frame(x=x,y=y)
#n =100 , p = 2

plot(y~x)
#Seems like a negative, quadratic relationship between y and x

set.seed(200)
#Produce LOOCV  errors for polynomial models up to order 4
n <- dim(dat)[1]
#define vector to record if there was a prediction error
mse_errors <- list()
for (degree in 1:4){
  errors <- numeric(n)
  for ( i in 1:n){
    
    lm_fit_less1 <- lm(data = dat[-i,] , y ~ poly(x,degree) )
    prediction <- predict(lm_fit_less1 , newdata = dat[i,] , type = "response")
    
    errors[i] <- (prediction - dat[i,"y"])^2
   
  }
  mse_errors[degree] <- mean(errors)
}

#Notice. If you change the seed, LOOCV won't change this is because you will always be evaulating
#The same models against the same points no matter the seed

#Can use the cv.glm method in boot
mse_errors <- list()
for (degree in 1:4){
  lm_fit <- glm(data = dat , y ~ poly(x,degree) )
  
  
  mse_errors[degree] <- cv.glm(lm_fit,data = dat )$delta[1]
}
#Question 9 --------------

mu_estimate <- mean(Boston$medv)
X_bar_deviation <- sd(Boston$medv) / sqrt(length(Boston$medv))
#bootstrap the standard error of mu

boot_fn <- function(data, index){
  return(mean(data[index,"medv"]))
}

boot(Boston,boot_fn , R=1000)

#Pretty close in terms of answers



