#R Library for training and testing Binary Spline model

library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(splines)

#Function to receive average Spline intercepts and coefficients and threshold value

#data, data1: Control and Experimenta datasets that have been Linearly Interpolated.
#output: Spline average intercept and coefficient between the two datasets and threshold value
spline_coefficient_train <- function(data, data1){
  coef <- c()
  #Runs through datasets to get average Spline intercept and coefficient values
  for (i in 1:length(data)){
    x <- seq(0:(length(unlist(data[1]))-1))
    y <- unlist(data[i])
    fit2 <- lm(y~bs(x,4))
    coef <- c(coef,fit2$coefficients)
  }
  for (i in 1:length(data1)){
    x <- seq(0:(length(unlist(data1[1]))-1))
    y <- unlist(data1[i])
    fit2 <- lm(y~bs(x,4))
    coef <- c(coef,fit2$coefficients)
  }
  matrix <- matrix(coef,nrow = 2000,ncol = 5, byrow = TRUE)
  n=0
  #Goes through datasets to compare organisms Spline coefficients to average coefficient to get threshold value.
  for (i in 1:length(data)){
    x <- seq(0:(length(unlist(data[1]))-1))
    y <- unlist(data[i])
    fit2 <- lm(y~bs(x,4))
    if (fit2$coefficients[1] > mean(matrix[,1]) & fit2$coefficients[2] > mean(matrix[,2]) & fit2$coefficients[3] > mean(matrix[,3]) & fit2$coefficients[4] > mean(matrix[,4]) & fit2$coefficients[5] > mean(matrix[,5])){
      n = n+1
    }
  }
  for (i in 1:length(data1)){
    x <- seq(0:(length(unlist(data1[1]))-1))
    y <- unlist(data1[i])
    fit2 <- lm(y~bs(x,4))
    if (fit2$coefficients[1] > mean(matrix[,1]) & fit2$coefficients[2] > mean(matrix[,2]) & fit2$coefficients[3] > mean(matrix[,3]) & fit2$coefficients[4] > mean(matrix[,4]) & fit2$coefficients[5] > mean(matrix[,5])){
      n = n+1
    }
  }
  results <- list("int" = mean(matrix[,1]), "df1" = mean(matrix[,2]), "df2" = mean(matrix[,3]), "df3" = mean(matrix[,4]), "df4" = mean(matrix[,5]), "thresh" = n/(length(data)*2))
  return(results)
}

# Spline model to determine binary classifier without parameters in order to fit Team's accuracy tests.

# data - Simulated dataset
# output - Binary output of 1 - trait affects survival, 0 - trait does not affect survival
spline_coefficient_test <- function(data){
  data <- x_transform_data(data)
  int <- results$int
  df1 <- results$df1
  df2 <- results$df2
  df3 <- results$df3
  df4 <- results$df4
  thresh <- results$thresh
  n = 0
  #Compares organisms to average coefficient to see if organism was affected by trait.
  for (i in 1:length(data)){
    x <- seq(0:(length(unlist(data[1]))-1))
    y <- unlist(data[i])
    fit2 <- lm(y~bs(x,4))
    if (fit2$coefficients[1] > int & fit2$coefficients[2] > df1 & fit2$coefficients[3] > df2 & fit2$coefficients[4] > df3 & fit2$coefficients[5] > df4){
      n = n+1
    }
  }
  #Compares number of organism to classifier to determine classifier
  if (n > length(data)*thresh){
    return(0)
  }
  return(1)
}

# Spline model consisting of intercept, coefficients, and threshold parameters. Function won't be applicable for Accuracy tests.

# data - Simulated dataset
# int - Spline intercept
# df1 - First coefficient
# df2 - Second coefficient
# df3 - Third coefficient
# df4 - Fourth coefficient
# thresh - Threshold value
# output - Binary output of 1 - trait affects survival, 0 - trait does not affect survival
spline_coefficient_test_params <- function(data, int, df1, df2, df3, df4, thresh){
  n = 0
  #Compares organisms to average coefficient to see if organism was affected by trait.
  for (i in 1:length(data)){
    x <- seq(0:(length(unlist(data[1]))-1))
    y <- unlist(data[i])
    fit2 <- lm(y~bs(x,4))
    if (fit2$coefficients[1] > int & fit2$coefficients[2] > df1 & fit2$coefficients[3] > df2 & fit2$coefficients[4] > df3 & fit2$coefficients[5] > df4){
      n = n+1
    }
  }
  #Compares number of organism to classifier to determine classifier
  if (n > length(data)*thresh){
    return(0)
  }
  return(1)
}
