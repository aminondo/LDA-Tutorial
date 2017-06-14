
# ---------------------------------------------------------------------------------------------------------------------
# LINEAR DISCRIMINANT ANALYSIS
# ---------------------------------------------------------------------------------------------------------------------
library(MASS)     
library(ISLR)


# The purpose of discriminant analysis is to classify objects into one of two or more groups
# based on a set of attributes that describe the objects
# Similar to regression analysis in the sense that it attempts to describe one dependent variable
# as a linear combination of other independent variables

# SMARKET ---------------------------------------------------------------------------------------
# We are going to use Smarket data to try and predict whether market will go up or down
head(Smarket)
#our dependent variable will be Direction and this is the variable we are trying to predict
#separate data into train and test data
train.index = sample(c(T, F), nrow(Smarket), replace = TRUE, prob = c(0.8, 0.2))
market_data = split(Smarket, train.index)
names(market_data) = c("test", "train")

#use MASS lda function using train data
lda_fit = lda(Direction ~ .,data=data$train)
lda_fit

#predict using test data
lda_predict = predict(lda_fit, newdata = data$test)
#confusion matrix to establish how accurate our results are
table(lda_predict$class,data$test$Direction)

# DEFAULT--------------------------------------------------------------------------------------------------------------------
#Now we are going to use Default data to predict whether someone will default on a loan like we did in class yesterday
head(Default)

train.index = sample(c(T,F), nrow(Default), replace=T, prob = c(.8,.2))
def_data = split(Default, train.index) 
names(def_data) = c("test", "train")

lda_fit = lda(default ~ ., data = def_data$train)
lda_fit
lda_predict = predict(lda_fit, newdata=def_data$test)
table(lda_predict$class,def_data$test$default)

# ---------------------------------------------------------------------------------------------------------------------
# EXERCISES:
# ---------------------------------------------------------------------------------------------------------------------

library(vcdExtra)
head(Titanicp)
# 1. Divide Titanicp data into training and testing data
# 2. use a linear discriminant analysis to predict whether a person survived or not


# 1. Use a linear discrimant analysis to predict the species of the iris data set


# ---------------------------------------------------------------------------------------------------------------------
# SOLUTIONS:
# ---------------------------------------------------------------------------------------------------------------------

# 1.
train.index = sample(c(T, F), nrow(Titanicp), replace = TRUE, prob = c(0.8, 0.2))

data = split(Titanicp, train.index)
names(data) = c("test", "train")
lda_fit = lda(survived ~ .,data=data$train)
lda_fit

lda_predict = predict(lda_fit, newdata = data$test)
table(lda_predict$class,data$test$survived)
# 2.
lda_fit = lda(Species ~ ., data=iris)
lda_predict = predict(lda_fit, iris)
table(lda_predict$class,iris$Species)
