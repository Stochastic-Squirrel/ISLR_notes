library(ISLR)
library(tidyverse)
library(MASS)
library(class)
names(Smarket)
summary(Smarket)
cor(Smarket[,-9])
#You can see strong correlation between year and volume of shares traded


#We fit a logistic GLM to predict Direction categorical variable

glm_model <- glm(data = Smarket, 
                 formula = Direction ~ Lag1 + Lag2 +Lag3 +Lag4 +Lag5 +Volume,
                 family = "binomial")
summary(glm_model)
# Each individual Coefficient not statistically significant from zero

#Use contrast function to check how variable was coded
contrasts(Smarket$Direction)
#Use predict function with no probability argument to reveal probabilities for TRAINED data
predict(glm_model, type = "response")

#Using these probabilities, let's recode them according to a threshold for "Up" and build confusion matrix
predictions <- case_when(predict(glm_model, type = "response") > 0.5 ~ "UP",
                         TRUE ~ "Down")
confusion_matrix <- table(predictions, Smarket$Direction)
#Calculate overall hit rate
overall_hit_rate <- sum(diag(confusion_matrix))/sum(confusion_matrix)
training_error_rate <- 1 - overall_hit_rate

#So let's figure out what our test error rate would be
#Split original data into training and test sets, need a sorting vector of True/False
train <- (Smarket$Year < 2005)
test <-  Smarket %>% filter(Year >= 2005)

#Retrain models
glm_model <- glm(data = Smarket, 
                 formula = Direction ~ Lag1 + Lag2 +Lag3 +Lag4 +Lag5 +Volume,
                 family = "binomial",
                 subset = train)
predictions <- predict(glm_model , newdata = test , type ="response" )
predictions <- case_when(predictions > 0.5 ~ "UP",
                         TRUE ~ "Down")
confusion_matrix <- table(predictions, test$Direction)
#Calculate overall hit rate for 2005 which was not used for training
overall_hit_rate <- sum(diag(confusion_matrix))/sum(confusion_matrix)
training_error_rate <- 1 - overall_hit_rate
#As suspected, test error rate is also bad at 52%. Remember how each coefficient was insignificant?
#Perhaps try taking out some predictors.
#If your predictors aren't meaningfully adding value, tends to just increase variance of models' estimates without actually decreasing bias!




#Let's try to use LDA only using Lag1 , Lag 2
lda_model <-  lda(data = Smarket, 
                  Direction ~ Lag1 + Lag2 ,
                  subset = train)
lda_predictions <- predict(lda_model , newdata = test )$class
confusion_matrix_lda <- table(lda_predictions, test$Direction)
#Calculate overall hit rate for 2005 which was not used for training
overall_hit_rate_lda <- sum(diag(confusion_matrix_lda))/sum(confusion_matrix_lda)
#Results pretty much identical to logistic regression





#Let's have a look at QDA, perhpas we have non linear boundaries
qda_model <-  qda(data = Smarket, 
                  Direction ~ Lag1 + Lag2 ,
                  subset = train)
qda_predictions <- predict(qda_model , newdata = test )$class
confusion_matrix_qda <- table(qda_predictions, test$Direction)
#Calculate overall hit rate for 2005 which was not used for training
overall_hit_rate_qda <- sum(diag(confusion_matrix_qda))/sum(confusion_matrix_qda)
#Overall hit rate here is MUCH better at 60%!
#Perhaps the quadratic form is a better approximation but will need to test this on bigger sets





#Now try the most flexible model, KNN

#knn requires matrix arguments
# matrix of dependednetn classes
#matrix , train , of just the PREDICTORS

knn_train <- cbind(Smarket$Lag1 , Smarket$Lag2)[train,]
knn_test <- cbind(Smarket$Lag1 , Smarket$Lag2)[!train,]
knn_classes <- Smarket$Direction[train]

knn_predictions <- knn(train = knn_train ,
                       test = knn_test ,
                      k =  1 ,
                      knn_classes)
confusion_matrix_knn <- table(knn_predictions, test$Direction)
#Calculate overall hit rate for 2005 which was not used for training
overall_hit_rate_knn <- sum(diag(confusion_matrix_knn))/sum(confusion_matrix_knn)

#Let's plot overall_knn hit rate for different K
#ALWAYS need to compare hit rates against some sort of benchmark
hitrate_knn <- function(k){
  knn_predictions <- knn(train = knn_train ,
                         test = knn_test ,
                         k =  k ,
                         knn_classes)
  confusion_matrix_knn <- table(knn_predictions, test$Direction)
  #Calculate overall hit rate for 2005 which was not used for training
  overall_hit_rate_knn <- sum(diag(confusion_matrix_knn))/sum(confusion_matrix_knn)
  return(overall_hit_rate_knn) 
  
  
  
  
}

plot(map_dbl(1:20 , hitrate_knn))

#We can see that k=3 is a decent guess but QDA still performs the best
#NOTE!!!! When using knn, the scale of your ariables is very important as variables with a
#naturally larger scale will have a dominating effect on calculating distance of nearest NEIGHBOUR!



