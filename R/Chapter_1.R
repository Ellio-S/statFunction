#Chapter 1

#Variance

#Notation: V, \sigma^2 - population variance, s_n^2 - sample variance
#n is the sample size
#Takes data set as parameter
#Returns the variance
#' @export
variance <- function(data)
{
  val <- mean(data^2) - (mean(data)^2)
  return(val)
} 