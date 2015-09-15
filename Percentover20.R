Percentover20 <- function(x){
  100*length((which(x > 20))) / length(x) 
  }