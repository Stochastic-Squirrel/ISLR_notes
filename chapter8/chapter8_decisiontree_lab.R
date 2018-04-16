library(tree)
library(ISLR)
library(tidyverse)

#Classification Trees --------
Carseats$High <- ifelse(Carseats$Sales<=8,"No","Yes") %>% factor()
tree_carseats <- tree(High ~ . - Sales , data = Carseats)
summary(tree_carseats)

#we can see a 9% trainning error
plot(tree_carseats)
text(tree_carseats, pretty = 0)

#Can see that shelve location is the most important variable as the tree splits on this first
tree_carseats$frame %>% summary


#Divide data into test and train sets
set.seed(2)
train <- sample(nrow(Carseats),200)
carseats_test <- Carseats[-train,]

tree_carseats <- tree(High ~ . - Sales , Carseats , subset = train)
tree_predictions <- predict(tree_carseats,carseats_test, type = "class")

#construct confusion matrix
table(tree_predictions,carseats_test$High)
# overall hit rate of 71.5%

#Let's try prunning these trees
set.seed(3)
cv_carseats <- cv.tree(tree_carseats, FUN = prune.misclass)
cv_carseats

#The dev object refers to the number of misclassifications
par(mfrow = c(1,2))
plot(cv_carseats$size,cv_carseats$dev, type = "b" , ylab ="No of misclassifications", xlab ="No of terminal nodes")
plot(cv_carseats$k,cv_carseats$dev, type = "b" , ylab ="No of misclassifications", xlab = "value of reg. param")

#We can apply the prune.misclass() function directly to the tree obuject in order to return the final decision tree

prune_carseats <- prune.misclass(tree_carseats , best = 9)
par(mfrow=c(1,1))
plot(prune_carseats)
text(prune_carseats, pretty = 0)

tree_predictions <- predict(prune_carseats,carseats_test, type = "class")

#construct confusion matrix
table(tree_predictions,carseats_test$High)
# overall hit rate improved to 77%
#Regression Trees ------------------
attach(MASS::Boston)
set.seed(1)
train <- sample(nrow(Boston),nrow(Boston)/2)
tree_boston <- tree(medv ~ . , Boston , subset = train)
summary(tree_boston) #for regression, deviance is simply error sum of squares

plot(tree_boston)
text(tree_boston, pretty = 0)

#see if tree should be pruned
cv_boston <- cv.tree(tree_boston)
cv_boston


#Bagging and Random Forests -------------

#bagging is a special case of a random forest where m = 9; i.e. consider all variables in each tree
library(randomForest)
set.seed(1)
Boston <- MASS::Boston
bag_boston <- randomForest( medv ~ . , data = Boston , subset = train , mtry = 13 , importance = T)
# mtry refers to the number of predicotrs that should be considered for each tree... this was set to ALL predictors
bag_boston

#caclulate test mse
yhat_bag <- predict(bag_boston , newdata = Boston[-train,])
plot(yhat_bag, Boston[-train,]$medv)
abline(0,1 , col = "red")
mean((yhat_bag - Boston[-train,]$medv)^2)
#notice how there is a pattern in the residuals... not good because seems to undersetimate value
#The n.trees argument will change how much bagging or bootstrap aggregating is done

#try a random forest
set.seed(1)
rf_boston <- randomForest(medv ~ . , data = Boston , subset = train , mtry = 6, importance = T)

yhat_rf <- predict(rf_boston , newdata = Boston[-train,])
mean((yhat_rf - Boston[-train,]$medv)^2)

#lower test error

importance(rf_boston)
#two measures to consider when looking at importance, reduction in training MSE and node purity
varImpPlot(rf_boston)

#Boosting ------------------------

library(gbm)
bost_boston <- gbm(medv ~ . , data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)

summary(bost_boston)

#partial dependence plots can also be produced
#illustrates the AVERAGE marginal effect of a variable after integrating out all other variables
par(mfrow = c(1,2))
plot(bost_boston , i ="rm")
plot(bost_boston , i ="lstat")
#increasing effect of rm on price and decreasing effect of lstat on price

#try another boosting tree with different shirnkage parameter

bost_boston <-
  gbm(
    medv ~ . ,
    data = Boston[train, ],
    distribution = "gaussian",
    n.trees = 5000,
    interaction.depth = 4,
    shrinkage = 0.2
  )
#note you need to specify the number of trees to be used in making a prediction
yhat_boost <- mean((predict(bost_boston , newdata = Boston[-train,], n.trees = 5000) - Boston[-train,]$medv)^2)
yhat_boost
