#Chapter 1


#Standard deviation
#Notation: \sigma - population standard deviation
#Takes data set as parameter
#Returns the standard deviation

#' @export
std_dev <- function(data)
{
  val <- mean(data^2) - (mean(data)^2)
  val <- sqrt(val)
  return(val)
}



#' Variance of a dataset
#' @description
#' Notation: V, sigma^2 - population variance, s_n^2 - sample variance, n is the sample size
#' @param data dataset to be analised
#' @return The variance of the dataset data
#' @export
variance <- function(data)
{
  val <- mean(data^2) - (mean(data)^2)
  return(val)
} 