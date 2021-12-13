
# Cox and Aalen's regressions need to have data in a readable format for them.
# This takes data where there is one organism per row and converts it to be 
# one measurement per row.

# data: dataset before being converted.  Each row represents an organism's 
# trait measurements until they die
convert_list <- function(data){
  time <- vector() # times where each trait value was measured
  status<- vector() # status: 0 alive, 1 dead
  trait <- vector() # value of trait at a certain time
  ids <- vector() # organism id used for clustering
  
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