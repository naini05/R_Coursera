add2 <- function(x,y){
  x+y
}

above10 <- function(x)
{
  x[x>10] # use <- x>10 ; x[use]
}

above <- function(x,y)
{
  x[x>y]
}

columnMean <- function(m, removeNA = TRUE)
{
  nc <- ncol(m)
  means <- numeric(nc)
  for(i in 1:nc)
  {
    means[i] <- mean(m[,i], na.rm = removeNA)
  }
  means
}