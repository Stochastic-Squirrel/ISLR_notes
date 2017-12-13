library(tidyverse)
library(ISLR)
library(leaps)# for subset selection


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
#How to select best set? ---------
