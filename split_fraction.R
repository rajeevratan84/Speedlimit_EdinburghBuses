split_fraction <- function(x){
  library(MASS)
  
  Frac = 0
  alpha <- x
  alpha <- as.fractions(alpha)
  alpha <- as.character(alpha)
  alpha <- strsplit(alpha, "/")
  alpha <- as.data.frame(alpha)
  a1 <- alpha[1,]
  a2 <- alpha[2,]
  a1 <- as.numeric(levels(a1))[a1]
  a2 <- as.numeric(levels(a2))[a2]
  Frac[1] <- a1
  Frac[2] <- a2
  Frac < as.data.frame(Frac)
  Frac
}