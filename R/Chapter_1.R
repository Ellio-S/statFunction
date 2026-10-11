#Chapter 1


#' Standard deviation
#' @description Notation: sigma - population standard deviation
#' @param data dataset
#' @return The stadard deviation
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


#' Pearson's coefficient of variation
#' @description Given a dataset the functions finds the coefficient
#' @param data dataset
#' @return Pearson's coefficient of variation
#' @export
CV <- function(data)
{
  m <- mean(data)
  if(m == 0)
  {
    print("The mean is zero, coefficient undefined")
  }
  else
  {
    val <- std_dev(data)/abs(m)
    return(val)
  }
}

