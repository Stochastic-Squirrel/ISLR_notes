library(tidyverse)
library(ISLR)
library(MASS)
library(class)

#analysis on Auto dataset
mpg01 <- case_when( Auto$mpg > median(Auto$mpg) ~ 1 ,
                    TRUE ~ 0)
dat <- dplyr::select(Auto,-mpg)
dat$mpg01 <- mpg01

#exploratory analysis
plot(dat)
# note super clear but weight , displacement and horsepower seem to have correlation with mpg01


#Split into train and test set
train = (dat$year%%2 == 0)  # if the year is even
test = !train
train_data = dat[train, ]
test_data = dat[test, ]
mpg01_test = mpg01[test]


lda_model <- lda(data = dat,
                 mpg01~ cylinders + horsepower + weight + acceleration ,
                 subset =train)
lda_predictions <- predict(lda_model , newdata = test_data)$class
error_rate <- mean(lda_predictions != test_data$mpg01)

#preetty much same as Q10 , repeat for Qda, logistic regression and KNN