###########################
#                         #
#   Team Assignment 7     #
#                         #
###########################

# Group 7 | Kennan Grant, Elizabeth Homan, Adrian Mead, Gregory Wert

#################################################################################
## Please submit one set of answers per team.                                  ##
## Your answers should be submitted as a .csv file per the instructions below. ##
## You should also submit your annotated R code per the instructions below.    ##
#################################################################################

library(tidyverse)
library(GGally)
library(car)
library(calibrate)
library(leaps)
library(Metrics)
library(boot)


# For this team assignment you will use the file "teamassign07train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign07test.csv".

test <- read_csv("teamassign07test.csv")
train <- read_csv("teamassign07train.csv")

#Separate training set into training and validation components 

set.seed(2)
sample <- sample_n(train,200)
valid <- setdiff(train,sample)

# going to look at all of the predictors plotted with one another
ggpairs(train)
# So there's clearly some very interesting relationships going on in the data. Things worth looking at. Most of the variables are
# not normally distributed. x7 seems to have a pretty good relationship with y.
# # It looks like x4 and x1 are really anti-correlated


#########################
##  Multicollinearity  ##
#########################
# Beforehand I decide that a vif > 100 is going to mean that we have a large amount of multicollinearity
lm.model <- glm(y ~ ., data = train)
vif(lm.model)
# OUTPUT:
        # x1         x2         x3         x4         x5         x6         x7 
# 130.530613  12.639190  56.282357  69.466889   1.008242   1.090237   1.865263 
# So immediately I will cut x1 for sure.
lm.model <- glm(y ~ . - x1, data = train)
vif(lm.model)
# OUTPUT:
#       x2        x3        x4        x5        x6        x7 
# 7.107647 31.365972 30.441323  1.006367  1.090236  1.796884 
# Multicollinearity looks immediately better

# Normally I'd consider pulling the column, but since we interested primarily in predictive power I keep x1 in.
lm.model <- glm(y ~ ., data = train)

#####################################
##  Outliers / Influential Points  ##
#####################################
summary(lm.model)
ti <- rstudent(lm.model)

## Normal probabilty plot
qqnorm(ti)
qqline(ti)
# This is not a great shape. We can see that at extreme theoretical quantiles, the actual sample quantile for the residuals is much 
# bigger than we would expect (so it's fatter than normal at the tails)

## Residual plot vs. fitted values
yhat <- fitted(lm.model)
plot(yhat,ti)
# I would not describe this as random scatter. It also looks like there's a significant increases in variance of the residuals as yhat 
# increases

## Residual plots vs. explanatory variables
plot(train$x1,ti)
plot(train$x2,ti)
plot(train$x3,ti)
# Very strange behavior here. Big gap between x3 from 160 to 210
plot(train$x4,ti)
plot(train$x5,ti)
plot(train$x6,ti)
plot(train$x7,ti)
# This is the only data that seems to have well-behaved residuals

## A summary of potential leverage and/or influential points
summary(influence.measures(lm.model))
# There aren't any immediately obvious violations on Cook's D or the hat matrix, but we do see several DFFITS issues and a lot of smaller
# covariances (so causing less precision)
inv_pos <- as.numeric(row.names(summary(influence.measures(lm.model))))
plot(yhat,ti)
textxy(yhat[inv_pos],ti[inv_pos],inv_pos)
# You can see immediately that all of the influential points (except for one at the bottom), are the ones that cluster together with 
# large residuals.
train[inv_pos,] # These rows represent points with some of the largest observed y-values.

#######################
##  Model Selection  ##
#######################
## Comparative model selection
bestmod <- regsubsets(y ~ ., data = sample, nbest=10)

## The 10 best models for each number of explanatory variables in the model
summary(bestmod)
best.sum <- as.data.frame(summary(bestmod)$outmat)
best.sum$p <- as.numeric(substr(rownames(best.sum),1,1))+1

## The criterion values corresponding to each model
best.sum$rss <- summary(bestmod)$rss
best.sum$adjr2 <- summary(bestmod)$adjr2
best.sum$cp <- summary(bestmod)$cp
best.sum$bic <- summary(bestmod)$bic

## Determine "best" models
best.sum[order(best.sum$rss),]
best.sum[order(best.sum$adjr2, decreasing = T),]
best.sum[order(best.sum$cp),]
best.sum[order(best.sum$bic),]
# Removing x5 seems to be on one the most highly-recommended things

## Iterative model selection
## Begin by defining the models with no variables (null) and all variables (full)
h.null <- glm(y ~ 1, data = sample)
h.full <- glm(y ~ ., data = sample)

## Forward selection
step(h.null, scope=list(lower=h.null, upper=h.full), direction="forward")

## Backward selection
step(h.full, scope=list(lower=h.null, upper=h.full), direction="backward")

## Stepwise selection
step(h.null, scope=list(lower=h.null, upper=h.full), direction="both")

# Pretty universally, we get that removing x5 gives the best model

###############################
##  Further Model Selection  ##
###############################
## Iterative model selection
## Begin by defining the models with no variables (null) and all variables (full)
h.null <- glm(y ~ 1, data = sample)
h.full <- glm(y ~ . + I(x1^2) + I(x2^2) + I(x3^2) + I(x4^2) + I(x5^2) + I(x6^2) + I(x7^2), data = sample)

## Forward selection
step(h.null, scope=list(lower=h.null, upper=h.full), direction="forward")
# Gives x2, x3, x4, x6, x7, x2^2, x3^2, x7^2

## Backward selection
step(h.full, scope=list(lower=h.null, upper=h.full), direction="backward")
# Gives x1, x2, x3, x4, x5, x6, x1^2, x2^2, x3^2, x5^2, x7^2

## Stepwise selection
step(h.null, scope=list(lower=h.null, upper=h.full), direction="both")
# Gives x2, x3, x4, x6, x7, x2^2, x3^2, x7^2


########################
##  Cross-Validation  ##
########################
set.seed(17)
cv.glm(train, lm1, K = 10)$delta[1]
cv.glm(train, lm2, K = 10)$delta[1]
cv.glm(train, lm3, K = 10)$delta[1]







# Trying this model first
lm1 <- glm(y ~ ., data = train)
mse(valid$y, predict(glm1, newdata = valid)) 
# OUTPUT:
# 16.32582

lm2 <- glm(y ~ . - x5, data = train)
mse(valid$y, predict(glm2, newdata = valid)) 
# OUTPUT:
# 16.35248

lm3 <- glm(y ~ x2 + x3 + x4 + x6 + x7 + I(x2^2) + I(x3^2) + I(x7^2), data = train)
mse(valid$y, predict(glm3, newdata = valid)) 
# OUTPUT:
# 15.9392






#Run an initial linear model using all predictors
lm1 = lm(y~.,data=Sample)
summary(lm1)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 248.3845    25.8347    9.61  < 2e-16 ***
#x1           -0.2864     0.0268  -10.67  < 2e-16 ***
#x2            0.1535     0.0181    8.47  6.5e-15 ***
#x3            0.0952     0.0352    2.70   0.0075 ** 
#x4           -0.6970     0.0803   -8.68  1.7e-15 ***
#x5            0.1557     0.1999    0.78   0.4372    
#x6           15.7673     1.6903    9.33  < 2e-16 ***
#x7            0.1029     0.0220    4.67  5.6e-06 ***

#Residual standard error: 3.06 on 192 degrees of freedom
#Multiple R-squared:  0.89,	Adjusted R-squared:  0.886 
#F-statistic:  223 on 7 and 192 DF,  p-value: <2e-16


#Select out variables shown to be insignificant in previous model one by one...
lm2 = lm(y~x1+x2+x3+x4+x6+x7,data=Sample)
summary(lm2)
#x3 is significant, but significantly less so than the others

lm3 = lm(y~x1+x2+x4+x6+x7,data=Sample)
summary(lm3)

preds.lm3 <- predict(lm3, newdata = Valid)

mse(Valid$y, preds.lm3) #15.8

# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
write.table(predvect, file="teamassign07preds.csv", row.names=F, col.names=F, sep=",")


# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.


# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.


