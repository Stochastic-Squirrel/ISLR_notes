library(tidyverse)
library(ISLR)
library(leaps)# for subset selection
library(glmnet) # for ridge and lasso regression
library(pls) #PCR and PLS regression

#With all of these approaches, we want to perform model size and variable selection on TRAINING data! i.e. cv on training data to figure out which class model is better
#Then use the test set only for working out a test MSE
#Finally, use ALL data when specifying the final model in its full form



#Best subset --------
Hitters <- na.omit(Hitters)

all_subsets <- regsubsets(Salary ~ . , data = Hitters)
summary(all_subsets)
# What it will do is report the details of the best training model for each model SIZE
#default is max size = 8 so we just pass a new max

all_subsets <- regsubsets(Salary ~ . , data = Hitters , nvmax = 19)
summary_of_subsets <- summary(all_subsets)
summary_of_subsets$cp

#Let's plot this information to see which model we shold try
#(remember how Cp is an unbiased estimator for TEST MSE if the estimation of noise variance is unbiased!)

par(mfrow =c(2,2))
plot(summary_of_subsets$rss , xlab ="No of Variables", ylab="RSS" , type ="l")
plot(summary_of_subsets$adjr2 , xlab ="No of Variables", ylab="Adjusted R-squared" , type ="l")

#The points command works like plot except it plots point on an already existing plot
#instead of creating a new plot

#identify point with highest adjusted r squared
which.max(summary_of_subsets$adjr2 )
points(x = 11 , summary_of_subsets$adjr2[11] , col="red" , cex = 2 , pch = 20)

plot(summary_of_subsets$cp , xlab ="No of Variables", ylab="Cp" , type ="l")
points(which.min(summary_of_subsets$cp) ,summary_of_subsets$cp[which.min(summary_of_subsets$cp)] , col="red", cex = 2 , pch=20 )

plot(summary_of_subsets$bic , xlab ="No of Variables", ylab="BIC" , type ="l")
points(which.min(summary_of_subsets$bic) ,summary_of_subsets$bic[which.min(summary_of_subsets$cp)] , col="red", cex = 2 , pch=20 )

par(mfrow=c(1,1))
#also the package has a plotting function
#Change scale argument to change what is on Y-axis. Look at ?plot.regsubsets
plot(all_subsets , scale ="r2")

#Look at linear coefficients of Model with lowest bic
coef(all_subsets , 6)

#Forward/Backward  set selection -----

regfit_forward <- regsubsets(Salary~. , data = Hitters , nvmax=19 ,method = "forward")
summary(regfit_forward)
regfit_backward <- regsubsets(Salary~. , data = Hitters , nvmax=19 ,method = "backward")
summary(regfit_forward)
#How to select best set? 
#Validation Set Approach------
#remember, we do subset selecction on TRAINING data, this is because we need test data
#to confirm the appropriatness. We wouldn't get a good estimate of test MSE 

set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(Hitters) , replace = TRUE)
test <- !train

regfit_best <- regsubsets(Salary ~ . , data = Hitters[train,] , nvmax = 19)

#We can look at "best" models for each size by RSS
#Now we create a MODEL MATRIX to be used as a data matrix for the test data
test_mat <- model.matrix(Salary ~ . , data = Hitters[test,])
test_mse <- c(numeric(19))

for( i in 1:19){
  coeffi <- coef(regfit_best,id = i)
  prediction <- test_mat[,names(coeffi)]%*%coeffi  #multiply data matrix by extracted coefficients
  test_mse[i] <- mean((Hitters$Salary[test] - prediction)^2)
}

#View errors
test_mse
#find mind
which.min(test_mse)
#Model 10 minimises test mse
#let's look at the model specification
coef(regfit_best, id = 10)

#Little tedious because there is no predict function within the leaps package
#Let's write out own one
predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object , id =id)
  xvars <- names(coefi)
  #return this now
  mat[ , xvars]%*%coefi
}

#Only use test and training split when you are trying to figure out which model size /class to use
# Once you have decided that you will use model of size 10, use ALL data to create those models
#This is because you have already validated it and determined that model size 10 is the most accurate

best_model <- regsubsets(Salary ~ . , data = Hitters , nvmax =19) %>% coef(. ,id=10)



#K-Fold cross Validation Approach---------

#Little more complicated. Need to perform best subset selection within each K- fold
k <- 10
set.seed(1)
folds <- sample(1:k , nrow(Hitters), replace = TRUE)  
cv_errors <- matrix(NA,k,19 , dimnames = list(NULL,paste(1:19)))  

#rememnber with k fold , kth fold is test set the other k-1 folds are for training!
#also we will be using our predict function written previously predict.regsubsets
for (j in 1:k){
  best_fit <- regsubsets( Salary ~ . , data = Hitters[folds!=j,] , nvmax =19)
  
  #within each fold, calculate MSE
  for (i in 1:19){
    pred <- predict(best_fit, Hitters[folds==j,] , id =i) #perform predictions for each model size on test data (the kth fold)
    cv_errors[j,i] <- mean((Hitters$Salary[folds==j] - pred)^2)
  }
  
}

#Now lets calculate average MSE across all k folds to estimate test MSE

cv_averages <- apply(cv_errors,2,mean)
par(mfrow=c(1,1))
plot(cv_averages,type = 'b')  
#LOWEST POINTS IS AT 11 variable size
#Therefore, we perform on ALL data, regsubsets to determine what the EXACT size 11 model will look like

coef_size_11 <- regsubsets(Salary ~ . , data = Hitters , nvmax = 19) %>% coef(. , id = 11)


#Shrinkage Method: Ridge Regression -----
  grid <- 10^seq(10,-2,length=100)
#glmnet requries model matrix for X values and a vector for y
  x <- model.matrix(Salary~ . , data = Hitters)[,-1]
  y <- Hitters$Salary
  ridge_regression <- glmnet(x,y,alpha=0, lambda = grid)
  
  #Ridge regression includes ALL coefficients, but will shrink noisy ones to near zero (very small)
  #look at example coefficients
  coef(ridge_regression)[,50] #50th model fit
  
  #Predict function is quite versatile for glmnet object
  #instead of predicted Y values, we can predict the Beta coefficients given a new lambda
  #predict will INTERPOLATE (i.e. make an educated guess) as to the coefficients if that lambda hasn't explicitly been used
  #therefore if you want exact coefficients, set exact = TRUE , glmnet will need to RETRAIN all of those models with the new lambda value inluded
  predict(ridge_regression,s=50 ,type="coefficients")[1:20,]
  
  set.seed(1)
  train <- sample(c(TRUE,FALSE),nrow(x), replace =TRUE)  
  test <- !train  

  test_data <- y[test]  
  
  ridge_regression <- glmnet(x = x[train,] , y = y[train] , alpha = 0 , lambda = grid , thresh = 1e-12)
  ridge_prediction <- predict(ridge_regression, s =4 , newx = x[test,])  
  test_mse <- mean((ridge_prediction-test_data)^2)  
  
  #Use cross validation to choose best Lambda
  set.seed(1)
  #REMEMBER ALWAYS use TRAINING DATA for model selection. Therefore allows us to see how useful it really is
  #when we estimate the test mse
  
  #training data contains our training and validation sets. Test data contains our test set!
  cv.out <- cv.glmnet(x[train,],y[train],alpha =0)
 plot(cv.out) 
  best_lambda <- cv.out$lambda.min
  best_lambda

  #We can make predictions based off of this best lambda , if newx arguent is
  #passed, automattical predicts Y
  prediction <- predict(ridge_regression, s= best_lambda , newx = x[test,])
  mean((prediction - test_data)^2)
  
#Shrinkage Method: Lasso Regression ---------
  
  # We can see that the choice of lambda is quite important
  lasso_regression <- glmnet(x[train,],y[train], alpha = 1 , lambda = grid)
  plot(lasso_regression)  
  
  set.seed(1)  
  cv.out <- cv.glmnet(x[train,],y[train], alpha = 1)  
  plot(cv.out)    
  best_lambda <- cvs.out$lambda.min  
  best_lambda 
  
  lasso_prediction <- predict(lasso_regression , s= best_lambda , newx = x[test,] )
  cat("MSE is now ", mean((lasso_prediction - test_data)^2))
  
  #one advantage of lasso over ridge is that it will give less complicated models
  final_models <- glmnet(x,y,alpha=1,lambda = grid)
  lasso_coefficients <- predict(final_models , type = "coefficients" , s =best_lambda)  
  lasso_coefficients[1:20,]  
  
#Dimension Reduction: Principal Components Regression--------
  set.seed(2)
  pcr_fit <- pcr(Salary ~ . , data = Hitters , scale= T , validation = "CV")  
  summary(pcr_fit)  
  #Outputs MSE for all K folds for each no of components used
  validationplot(pcr_fit, val.type = "MSEP")
  #Huge improvement when 1 component is used (is 0 OLS????)
  summary(pcr_fit)
  
  #NOW lets look at the TEST MSE (error on unseen data)
  set.seed(1)
  pcr_fit <- pcr(Salary ~ . , data = Hitters[train,] , scale= T , validation = "CV")  
  #Obtain model with lowest CV Mse
  validationplot(val.type = "MSEP",pcr_fit)
  #This is when M=7
  pcr_prediction <- predict(pcr_fit, x[test,],ncomp=7)
  mean((pcr_prediction - test_data)^2)  
  
#Dimension Reduction: Partial Least Squares Regression ------
  #TRy partial least squares. Creates components which maximises variance in direction of Y variable
  
  set.seed(1)
  pls_fit <- plsr(Salary ~. , data =Hitters , subset = train , scale = T , validation = "CV")  
  summary(pls_fit)  
  
  #lowest cross-validation set MSE when M=2
  
  #Now get a COMPLETELY unbiased estimate for the model when M=2
  prediction_plsr <- predict(pls_fit, x[test,] , ncomp =2)
  mse_test_pslr <- mean((prediction_plsr-test_data)^2)  

  #OKAY coool we have confirmed that our unbiased test estimate is decent
  #Now we specifiy the FULL production model using ALL data!!!
  
  final_model <- plsr(Salary ~ . , data = Hitters ,ncomp = 2 , scale = T)
  summary(final_model)  
  final_model$loadings  
  #look up what the loading weights are