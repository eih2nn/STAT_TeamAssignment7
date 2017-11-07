
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
library(Metrics)

# For this team assignment you will use the file "teamassign07train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign07test.csv".

Test <- read.csv("teamassign07test.csv", encoding = 'utf-8', stringsAsFactors = FALSE)
Train <- read.csv("teamassign07train.csv", encoding = 'utf-8', stringsAsFactors = FALSE)

#Separate training set into training and validation components 

set.seed(2)
Sample <- sample_n(Train,200)
Valid <- setdiff(Train,Sample)

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


