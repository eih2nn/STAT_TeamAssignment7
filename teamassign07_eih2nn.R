
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
#(Intercept) 250.4781    28.1180    8.91  4.0e-16 ***
#x1           -0.2703     0.0283   -9.55  < 2e-16 ***
#x2            0.1424     0.0200    7.11  2.2e-11 ***
#x3            0.0541     0.0392    1.38   0.1697    
#x4           -0.6987     0.0880   -7.94  1.7e-13 ***
#x5            0.2778     0.2174    1.28   0.2027    
#x6           13.6297     1.7790    7.66  8.8e-13 ***
#x7            0.0675     0.0231    2.92   0.0039 ** 

#Residual standard error: 3.27 on 192 degrees of freedom
#Multiple R-squared:  0.88,	Adjusted R-squared:  0.875 
#F-statistic:  201 on 7 and 192 DF,  p-value: <2e-16

#Select out variables shown to be insignificant in previous model one by one...
lm2 = lm(y~x1+x2+x4+x5+x6+x7,data=Sample)
summary(lm2)
#x5 still insignificant

lm3 = lm(y~x1+x2+x4+x6+x7,data=Sample)
summary(lm3)

preds.lm3 <- predict(lm3, newdata = Valid)

mse(Valid$y, preds.lm3) #13.4

# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
write.table(predvect, file="teamassign07preds.csv", row.names=F, col.names=F, sep=",")


# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.


# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.


