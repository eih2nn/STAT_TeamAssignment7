
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
setwd("~/Documents/GitHub/STAT_TeamAssignment7")

# For this team assignment you will use the file "teamassign07train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign07test.csv".

Test <- read.csv("teamassign07test.csv", encoding = 'utf-8', stringsAsFactors = FALSE)
Train <- read.csv("teamassign07train.csv", encoding = 'utf-8', stringsAsFactors = FALSE)



# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
write.table(predvect, file="teamassign07preds.csv", row.names=F, col.names=F, sep=",")


# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.


# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.


