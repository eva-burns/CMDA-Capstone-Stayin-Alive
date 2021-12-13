#Average Spline fit over Linear Interpolated dataset as well as Spline intercept and coefficient values of average Spline. 

#data: Linear Interpolated dataset
#output: Plot of Spline fit and Spline intercept and coefficient values.
spline_fits <- function(data){
  x <- rep(0:(length(unlist(data[1]))-1), length(data))
  y <- unlist(data)
  library(splines)
  fit2 <- lm(y~bs(x,4))
  summary <- summary(fit2)
  plot(x,y, "p",
       xlab="Time",
       ylab="Function-Valued Trait",cex.lab=1.0, cex.axis=1.0, cex.main=1.0)
  title(deparse(substitute(data)))
  xx <- seq(min(x),max(x), length.out=250)
  yy <- predict(fit2, data.frame(x=xx))
  lines(xx,yy, col='blue')
  return(summary)
}
