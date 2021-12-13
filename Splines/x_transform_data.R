#Function that applies Linear Interpolation on dataset

library(survival)
library(dplyr)
source("convert_list.R")

#data: dataset prior to convert_list function
#output: list of Linear Interpolated dataset with a constant x-axis length of 50.
x_transform_data <- function(data){
  data <- convert_list(data)
  time = unlist(data$time)
  trait = unlist(data$trait)
  lst <- split(time, cumsum(time==1))
  lst1 <- split(trait, cumsum(time==1))
  #Finding max x axis length for x axis transformation
  max = length(unlist(lst1[1]))
  for (i in lst1){
    if (max < length(unlist(i))){
      max = length(unlist(i))
    }
  }
  library(caret)
  library(pracma)
  trial = vector()
  for (i in 1:length(lst)){
    x <- unlist(lst[i])
    x <- c(0,x)
    y <- unlist(lst1[i])
    y <- c(0,y)
    process <- preProcess(as.data.frame(x), method=c("range"))
    x <- unlist(predict(process, as.data.frame(x)))
    xi <- seq(0, 1, len = 50)
    trial <- c(trial,interp1(x, y, xi, method = "linear"))
  }
  data <- split(trial, ceiling(seq_along(trial)/(50)))
  return(data)
}
