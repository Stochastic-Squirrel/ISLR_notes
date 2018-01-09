library(tidyverse)
library(ISLR)
library(boot)
library(splines)
library(gam)

#Polynomial Regression and Step Functions ---------------
polynomial_fit <- lm(wage ~ poly(age,4) , data = Wage)
#exctract coefficients
coefficients(summary(polynomial_fit))

#We can also choose the basis function based on the raw polynomials set Raw = T
#Remember, if you want to type out the polynomials manually, keep it wihtin I(age^2) as the ^ has a special meaning in the R formula notation

#We will recreate figure 7.1
agelims <- range(Wage$age)
age_grid <- seq(agelims[1],agelims[2])
predictions <- predict(polynomial_fit , newdata = list(age=age_grid), se = TRUE)
se_bands <- cbind(predictions$fit - 2*predictions$se.fit ,predictions$fit + 2*predictions$se.fit )


#Now lets plot this 
par(mfrow=(c(1,2)), mar =c(4.5,4.5,1,1) , oma = c(0,0,4,0))
plot(Wage$age, Wage$wage, xlim= agelims , cex =0.5 , col = "darkgrey")
title("Degree -4 Polynomial", outer = T)
lines(age_grid, predictions$fit, lwd=2 , col="blue") #remember, the x coordinates are specificed first
matlines(age_grid,se_bands,lwd=1,col='blue',lty=3)



#What degree polynomial will be the best?

fit_1 <- glm(data = Wage , wage ~ age)
fit_2 <- glm(data = Wage , wage ~ poly(age,2))
fit_3 <- glm(data = Wage , wage ~ poly(age,3))
fit_4 <- glm(data = Wage , wage ~ poly(age,4))
fit_5 <- glm(data = Wage , wage ~ poly(age,5))

anova(fit_1,fit_2,fit_3,fit_4,fit_5 ,test = "F")
#degree 3 or 4 seems to be the best
#Anova method is robust and will work if you have other, non-age variables in the model

#Notice this
coefficients(summary(fit_5))
#P-values of the t-statistics are the same!!
#in fact, if you square the t values, they will equal the f-values

#Can confirm anova results using cross-validation
cv_results <- map(list(fit_1,fit_2,fit_3,fit_4,fit_5),cv.glm, K=10 , data=Wage) %>% transpose(~delta)

#Remember that glm logistic is a type of linear model, so lets try a polynomial glm

glm_fit <- glm(I(wage>250)~poly(age,4),data=Wage, family = "binomial")

#note we use the I function as an indicator function which sets values to 1 when wage>250
glm_predictions <- predict(glm_fit, newdata = list(age=age_grid),se = T,type="response")
#standard error bands
se_bands_glm <- cbind(glm_predictions$fit - 2*glm_predictions$se.fit ,glm_predictions$fit + 2*glm_predictions$se.fit )
plot(Wage$age,I(Wage$wage>250),xlim=agelims,type="n",ylim=c(0,0.2))
points(jitter(Wage$age),I((Wage$wage>250)/5), cex=.5,pch="|",col="darkgrey") #plot actual data, need to transform it to keep it in the range
lines(age_grid,glm_predictions$fit,lwd=2,col="blue")
matlines(age_grid,se_bands_glm,lwd=1,col="blue",lty=3)


#Let's create a step function
table(cut(Wage$age,4))
step_lm <- lm(wage~cut(age,4),data = Wage)
coefficients(summary(step_lm))
#Notice how cut creates factors or categorical variables

#Regression Splines

#bs stands for basis spline, in other words you specify the basis formulae, forms a cubic spline by default , note not a NATURAL cubic spline
regression_spline <- lm(wage ~bs(age,knots=c(25,40,60)),data = Wage)
summary(regression_spline)

spline_prediction <- predict(regression_spline, newdata= list(age=age_grid) , se = T)
plot(Wage$age,Wage$wage, col ="gray")


#natural spline
natural_spline <- lm(wage~ns(age,df=4), data = Wage) #can either specify number of knots or by degrees of FREEDOM
natural_predictions <- predict(natural_spline , newdata = list(age=age_grid),se=T)
title("natural spline")
lines(age_grid,natural_predictions$fit,col="red")


#Smoothing Spline
plot(Wage$age,Wage$wage, col ="gray")
title("Smoothing Spline")
smoothing_specified <- smooth.spline(Wage$age,Wage$wage,df=16)
smoothing_cv_determined <- smooth.spline(Wage$age, Wage$wage, cv = TRUE)
lines(smoothing_specified,col="green")
lines(smoothing_cv_determined,col="black")
#not much difference in this case


#Local Regresion
plot(Wage$age,Wage$wage, col ="gray")
title("LOESS ")
loess_fit <- loess(wage~age,span=0.2,data=Wage)
loess_fit_2 <- loess(wage~age,span=0.5,data=Wage)
lines(age_grid,predict(loess_fit,newdata=data_frame(age=age_grid)),col="red")
lines(age_grid,predict(loess_fit_2,newdata=data_frame(age=age_grid)),col="blue")
#higher the span, greater the smoothness, span of 0.2 means that 20% of data will be within neighbourhood for the regression at each unique x_i

#GAMs
#Additive model framework, create a separate f(x_j)  then use OLS to determine B_j s.t. B_j * f(x_j)
#Note this works because you are performing OLS on the basis functions' outputs. Note with smoothing splines, you aren't using basis functions, you are trying to find a general function g(x), therefore to use smoothing splines in a GAM, you need to be careful
gam_1 <- lm( wage ~ ns(year,4) + ns(age,5) + education , data = Wage)

#Because of the issue stated above, we use the gam library and the gam function instead of the base lm function
gam_2 <- gam(wage~s(year,4) + s(age,5) +education , data = Wage) #s() indicates that we wanta smoothing spline with the desired degrees of freedom


par(mfrow=c(1,3))
plot(gam_2,se=T,col="Blue") #very cool!
#it invokes plot.gam() by default automatically, we can call plot.gam on the "lm" version of the gam (gam_1)
plot.gam(gam_1,se=T,col="red")

#The smoothing spine for year looks linear, lets do an anova on a few models to determine if it should be a linear function rather than a smoothing spline
gam_noyear <- gam(wage~ s(age,5) +education , data = Wage)
gam_linear_year <- gam(wage~ year + s(age,5) +education , data = Wage)

anova(gam_noyear,gam_linear_year , gam_2 , test = "F")
#Having linear year is better than not having year. The smoothing spline is not significantly better than a linear function of year!!
