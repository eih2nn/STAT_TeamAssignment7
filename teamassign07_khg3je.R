
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


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(Metrics)
library(leaps)


# Load Data ---------------------------------------------------------------

Test <- read.csv("teamassign07test.csv", encoding = 'utf-8', stringsAsFactors = FALSE)
Train <- read.csv("teamassign07train.csv", encoding = 'utf-8', stringsAsFactors = FALSE)


# Create Train and Test ---------------------------------------------------

set.seed(2)
Sample <- as_tibble(sample_n(Train,200))
Valid <- as_tibble(setdiff(Train,Sample))


# Model 1 -----------------------------------------------------------------

#Run an initial linear model using all predictors
lm1 = lm(y~.,data=Sample)
summary(lm1)

preds <- predict(lm1, Valid) # make predictions
mse(actual = Valid$y, predicted = preds)# calculate mse: 16.32582


# Exploration and Variable Creation ------------------------------------------

# x1
plot(Sample$x1,Sample$y)

Sample %>% 
  group_by(x1) %>% 
  summarise(mean_y = mean(y)) %>% 
  ggplot(aes(x1, mean_y)) +
  geom_point() # plot relationship with response var

Sample <- Sample %>% 
  mutate(x1_binned = if_else(x1 > 685, 1, 0)) # add x1_binned
Valid <- Valid %>% 
  mutate(x1_binned = if_else(x1 > 685, 1, 0)) # add x1_binned

# x2
plot(Sample$x2,Sample$y)

Sample %>% 
  group_by(x2) %>% 
  summarise(mean_y = mean(y)) %>% 
  ggplot(aes(x2, mean_y)) +
  geom_point()

Sample <- Sample %>% 
  mutate(x2_binLOW = if_else(x2 == 245.0 | x2 == 269.5 | x2 == 367.5, 1, 0)) # add x2_binLOW  This suggests interaction with itself will be helpful.  That will be included later.
Valid <- Valid %>% 
  mutate(x2_binLOW = if_else(x2 == 245.0 | x2 == 269.5 | x2 == 367.5, 1, 0))

# x3
plot(Sample$x3,Sample$y)

Sample %>% 
  group_by(x3) %>% 
  summarise(mean_y = mean(y)) %>% 
  ggplot(aes(x3, mean_y)) +
  geom_point()

Sample <- Sample %>% 
  mutate(x3_binHIGH = if_else(x3 > 175, 1, 0)) # create x3_binHIGH.  
Valid <- Valid %>% 
  mutate(x3_binHIGH = if_else(x3 > 175, 1, 0))

# x4
plot(Sample$x4,Sample$y)

Sample %>% 
  group_by(x4) %>% 
  summarise(mean_y = mean(y)) %>% 
  ggplot(aes(x4, mean_y)) +
  geom_point() # is mirror image of x1 vs. y relationship

Sample <- Sample %>% 
  mutate(x1x4COMBO = x1*x4,
         x1x4_binLOW = if_else(x1x4COMBO < 100800, 1, 0))
Valid <- Valid %>% 
  mutate(x1x4COMBO = x1*x4,
         x1x4_binLOW = if_else(x1x4COMBO < 100800, 1, 0))
# notes:
# x1*x4 looks promising if multiplied, then binned, then interacted

Sample %>% 
  group_by(x1x4COMBO) %>% 
  summarise(mean_y = mean(y)) %>% 
  ggplot(aes(x1x4COMBO, mean_y)) +
  geom_point()

# x5
plot(Sample$x5,Sample$y) # this one might be useless

# x6
plot(Sample$x6,Sample$y)

Sample <- Sample %>% 
  mutate(x6_factor = as.factor(x6))
Valid <- Valid %>% 
  mutate(x6_factor = as.factor(x6))

# x7
plot(Sample$x7,Sample$y) # linear relationship with lots of error.  maybe will perform well with an interaction, who knows.


# Step-Wise Selection -----------------------------------------------------

# create response variable
Sample <- Sample %>% 
  mutate(log_y = log(y))

Sample <- Sample[-86,] # remove this outlier (as identified by a model fit and summary influence measures.  had highest dffit of 1.45)

# null model
null=glm(y ~ 1, data = Sample)
null

col_names <- names(Sample) # extract col names
# remove non-predictors
remove <- c("y","log_y") 
col_names <- col_names[! col_names %in% remove]
# combine into formula
terms_init <-  paste(col_names, collapse="+")

long_formula <- as.formula(sprintf("y ~ (%s)^2", terms_init))

# full model:
full=glm(formula = long_formula, data = Sample)
full

# fit step-wise
my_step <- step(null, scope=list(lower=null, upper=full), direction="both")
# my_step <- step(full, direction="backward")
summary(my_step)
anova(my_step)

# make predictions on validation set
preds <- predict(my_step, newdata = Valid)

# calculate MSE
mse(actual = Valid$y, predicted = preds)# calculate mse: 4.196975


# Make Necessary Transformations to Test Dataset --------------------------

Test <- as_tibble(Test)
Test <- Test %>% 
  mutate(x1_binned = if_else(x1 > 685, 1, 0),
         x2_binLOW = if_else(x2 == 245.0 | x2 == 269.5 | x2 == 367.5, 1, 0),
         x3_binHIGH = if_else(x3 > 175, 1, 0),
         x1x4COMBO = x1*x4,
         x1x4_binLOW = if_else(x1x4COMBO < 100800, 1, 0),
         x6_factor = as.factor(x6))


# Make Necessary Transformations to Training ------------------------------

Train <- as_tibble(Train)
Train <- Train %>% 
  mutate(x1_binned = if_else(x1 > 685, 1, 0),
         x2_binLOW = if_else(x2 == 245.0 | x2 == 269.5 | x2 == 367.5, 1, 0),
         x3_binHIGH = if_else(x3 > 175, 1, 0),
         x1x4COMBO = x1*x4,
         x1x4_binLOW = if_else(x1x4COMBO < 100800, 1, 0),
         x6_factor = as.factor(x6))


# Make Final Predictions --------------------------------------------------

# null model
null=glm(y ~ 1, data = Train)
null

col_names <- names(Train) # extract col names
# remove non-predictors
remove <- c("y","log_y") 
col_names <- col_names[! col_names %in% remove]
# combine into formula
terms_init <-  paste(col_names, collapse="+")

long_formula <- as.formula(sprintf("y ~ (%s)^2", terms_init))

# full model:
full=glm(formula = long_formula, data = Train)
full

# fit step-wise
my_step <- step(null, scope=list(lower=null, upper=full), direction="both")

predvect <- predict(my_step, newdata = Test)

# Write To File -----------------------------------------------------------



# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:

write.table(predvect, file="teamassign07preds.csv", row.names=F, col.names=F, sep=",")


# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.


# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.


