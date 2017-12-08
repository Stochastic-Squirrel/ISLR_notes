library(tidyverse)
library(ISLR)
library(MASS)
library(class)

# Question 10 -----------------

#Look at data
summary(Weekly)
plot(Weekly)

#only meaningful trends is between year and total volume traded

#Fit glm to predict Direction
model_glm <- glm(data = Weekly,
                 Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,
                 family = "binomial")
summary(model_glm)
#From looking at coefficients, seems that only Lag2 is statisticall significantly different from zero

#Look at confusion matrix now
#using contrasts() we can see that 0 is down, 1 is up

raw_predictions <-  predict(model_glm , type = "response")
confusion_glm <- table(dnn = c("Predicted","Actual") , 
                       Weekly$Direction , 
                       case_when(raw_predictions >0.5 ~ "Up" , TRUE ~ "Down")
                       )
#overall hit rate
overall_hit_rate <- sum(diag(confusion_glm)) / sum(confusion_glm)
overall_hit_rate
#Prediction accuracy for each class
up_accuracy <- 557 / (557+48)
down_accuracy <- 54 / (54+430)
up_accuracy
down_accuracy
#Up accuracy is huge at 92% and down accuracy is awful at 11% . If glm predicts up, most likely right , very
#low accuracy for down


#Let's look at test and training accuracy for GLM,LDA,QDA KNN
train_boolean <- Weekly$Year < 2009
test_data <- Weekly[!train_boolean,]
overall_hit_rates <- list()
#glm----

model_glm_lag2 <- glm(data = Weekly,
                 Direction ~ Lag2 ,
                 family = "binomial" ,
                 subset = train_boolean)
summary(model_glm_lag2)

raw_predictions_lag2 <-  predict(model_glm_lag2 , type = "response" ,newdata = test_data)

confusion_glm_lag2 <- table(dnn = c("Predicted","Actual") , 
                       test_data$Direction , 
                       case_when(raw_predictions_lag2 >0.5 ~ "Up" , TRUE ~ "Down")
)
overall_hit_rates$glm <- sum(diag(confusion_glm_lag2)) / sum(confusion_glm_lag2)


#lda ----
lda_model_lag2 <- lda(data = Weekly,
                      Direction ~ Lag2 ,
                      subset = train_boolean)
lda_model_lag2
#Notice, how there is only one linear discrimant disc = b0 + 0.44Lag2 , positive values lead to Up classification
lda_prediction <- predict(lda_model_lag2 , newdata = test_data)
confusion_lda_lag2 <- table(dnn = c("Predicted","Actual") , 
                            test_data$Direction , 
                            lda_prediction$class
)
overall_hit_rates$lda <- sum(diag(confusion_lda_lag2)) / sum(confusion_lda_lag2)


#qda ----
qda_model_lag2 <- qda(data = Weekly,
                      Direction ~ Lag2 ,
                      subset = train_boolean)
qda_model_lag2
qda_prediction <- predict(qda_model_lag2 , newdata = test_data)
confusion_qda_lag2 <- table(dnn = c("Predicted","Actual") , 
                            test_data$Direction , 
                            qda_prediction$class
)
overall_hit_rates$qda <- sum(diag(confusion_qda_lag2)) / sum(confusion_qda_lag2)

#knn ------

#Note, you need train and test to be a DF or a matrix
knn_model <- knn(k = 1,
          cl = Weekly$Direction[train_boolean] ,
          train = as.matrix( Weekly$Lag2[train_boolean] ) ,
          test = as.matrix(Weekly$Lag2[!train_boolean] ) )

confusion_knn <- table(dnn = c("Predicted","Actual"), knn_model , test_data$Direction)

overall_hit_rates$knn <-  sum(diag(confusion_knn)) / sum(confusion_knn)





overall_hit_rates
#Seems like lda and glm do the best