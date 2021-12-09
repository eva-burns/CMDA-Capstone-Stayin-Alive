convert_list <- function(data){
  time <- vector()
  status<- vector()
  trait <- vector()
  ids <- vector()
  for (row in 1:nrow(data)) {
    row_data <- data[row,][!is.na(data[row,])]
    row_data <- row_data[2:length(row_data)]
    n <- length(row_data)
    
    time <- c(time, 1:n)
    
    surv <- rep(0, n)
    surv[n] = 1
    status <- c(status, surv)
    
    trait <- c(trait, row_data)
    ids <- c(ids, rep(row, length=n))
  }
  
  new_data <- data.frame(time=time, status=status, trait=trait, ids=ids)
  return(new_data)
}