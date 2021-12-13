#Function that plots updated Linear Interpolated dataset

library(ggplot2)
library(dplyr)
library(ggfortify)

#data: Linear Interpolated data
#output: Plot of x-axis transformed dataset
x_transform_plot <- function(data){
  require(reshape2)
  library(viridis)
  h <- do.call(cbind, data)
  h.melt <- melt(h)
  names(h.melt) <- c("Time","N","Trait") 
  p<-ggplot(h.melt, aes(x=Time, y=Trait,group=N))+ theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12))+  ggtitle(gsub("_", " ",deparse(substitute(data))))+ 
    xlab("Time") + ylab("Function-Valued Trait") +
    geom_line(aes(color=N))+scale_color_gradientn(colours = rainbow(5),guide=FALSE) 
  p
}