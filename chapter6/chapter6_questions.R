library(tidyverse)
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
#Note the interesting result from Exercise 5. Ridge assigns similar coefficient estimates to highly correlated variables whereas LASSO could assign completely different betas

#Question 8 --------------

set.seed(1)
X <- rnorm(100)
eps <- rnorm(100)

#Define observed variable Y ( and thus the ACTUAL relationship between Y and X)
beta0 <-  3
beta1 <-  2
beta2 <-  -3
beta3 <-  0.3
Y <-  beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps

#Best Subset
#Find best model for each size up to size 1
data <- data_frame(y=Y,x=X)
all_subsets <- regsubsets(data = data , y ~ poly(x,10 , raw = TRUE) , nvmax=10)
summaries_all <- summary(all_subsets)
#Use measures that estimate test accuracy
#lowest Cp
which.min(summaries_all$cp)
#BIC
which.min(summaries_all$bic)
#adjusted R2
which.max(summaries_all$adjr2)

#plots for best subset selections
plot_data <- data_frame(p = seq(1,10,1),cp = summaries_all$cp , bic = summaries_all$bic, adjr2 = summaries_all$adjr2 ) %>% 
              gather(key = 'Measurement' , value ="value" , -p)
ggplot(plot_data,aes(x = factor(p), y = value)) + geom_point()+xlab("Model Size") + facet_grid(~ Measurement) + 
  geom_point(data = plot_data %>% group_by(Measurement) %>% filter(value == min(value) & Measurement !='adjr2' | ( value == max(value) & Measurement =='adjr2' )  ),col ="red" )

#lowest points are at p = 3 for all


#Forward Only
data <- data_frame(y=Y,x=X)
all_subsets <- regsubsets(data = data , y ~ poly(x,10 , raw = TRUE) , nvmax=10 ,method = "forward")
summaries_all <- summary(all_subsets)
#Use measures that estimate test accuracy
#lowest Cp
which.min(summaries_all$cp)
#BIC
which.min(summaries_all$bic)
#adjusted R2
which.max(summaries_all$adjr2)

#plots for best subset selections
plot_data <- data_frame(p = seq(1,10,1),cp = summaries_all$cp , bic = summaries_all$bic, adjr2 = summaries_all$adjr2 ) %>% 
  gather(key = 'Measurement' , value ="value" , -p)
ggplot(plot_data,aes(x = factor(p), y = value)) + geom_point()+xlab("Model Size") + facet_grid(~ Measurement) + 
  geom_point(data = plot_data %>% group_by(Measurement) %>% filter(value == min(value) & Measurement !='adjr2' | ( value == max(value) & Measurement =='adjr2' )  ),col ="red" )

#Backwards
data <- data_frame(y=Y,x=X)
all_subsets <- regsubsets(data = data , y ~ poly(x,10 , raw = TRUE) , nvmax=10 ,method = "backward")
summaries_all <- summary(all_subsets)
#Use measures that estimate test accuracy
#lowest Cp
which.min(summaries_all$cp)
#BIC
which.min(summaries_all$bic)
#adjusted R2
which.max(summaries_all$adjr2)

#plots for best subset selections
plot_data <- data_frame(p = seq(1,10,1),cp = summaries_all$cp , bic = summaries_all$bic, adjr2 = summaries_all$adjr2 ) %>% 
  gather(key = 'Measurement' , value ="value" , -p)
ggplot(plot_data,aes(x = factor(p), y = value)) + geom_point()+xlab("Model Size") + facet_grid(~ Measurement) + 
  geom_point(data = plot_data %>% group_by(Measurement) %>% filter(value == min(value) & Measurement !='adjr2' | ( value == max(value) & Measurement =='adjr2' )  ),col ="red" )




#LASSO
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data)[, -1]
mod.lasso = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = mod.lasso$lambda.min
best.lambda
plot(mod.lasso)

#Let's attain the best model at the optimal lambda
best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

#Notice the non-zero coefficients, and how some coefficients are squeezed exactly to zero
#Question 9------
#Split data into train and test
set.seed(11)
train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]


lm_fit <- lm(data = College.train , Apps ~ . )
lm_predictions <- predict(lm_fit , newdata = College.test )
test_error <-  mean((lm_predictions - College.test$Apps)^2)
test_error


#Try ridge regression
train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
grid = 10 ^ seq(4, -2, length=100)
mod.ridge = cv.glmnet(train.mat, College.train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best

ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)

#Test error slightly higher than ols



#LASSO
#Pick lamda using training set and report error from test set
mod.lasso = cv.glmnet(train.mat, College.train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best = mod.lasso$lambda.min
lambda.best
lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - lasso.pred)^2)

#Attain the coefficientrs, USE ALL DATA HERE! when finding the final model
mod.lasso = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(mod.lasso, s=lambda.best, type="coefficients")




#Try PCR and PLS
pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred = predict(pcr.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - data.frame(pcr.pred))^2)

pls.fit = plsr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - data.frame(pls.pred))^2)
     