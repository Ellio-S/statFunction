#Chapter 1

#' Mean deviation
#' @description Mean deviation with respect to a parameter p
#' @param data dataset
#' @param p a parameter of central tendency
#' @return The mean deviation
#' @export
MD <- function(data,p)
{
  sum <- 0
  n <- length(data)
  for(i in 1:n)
  {
    sum <- sum + abs(data[i]-p)
  }
  val <- sum/n
  return(val)
}

#'Mean Square Error - parameter
#' @description Calculates the normal mean square error with respect to a parameter p 
#' @param data dataset
#' @param p parameter usually of central tendency
#' @return Mean Square Error - MSE
#' @export
MSE_p <- function(data,p)
{
  sum <- 0
  n <- length(data)
  for(i in 1:n)
  {
    sum <- sum + (data[i]-p)^2
  }
  val <- sum/n
  return(val)
}


#' Standard deviation
#' @description 
#' Notation: sigma - population standard deviation
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

#' Moment of order 1 dimension
#' @description Computes the moment of order with respect to a point c
#' if c = 0 or c is not given then we have an ordinary moment of order and if c = mean, then we have a central moment of order
#' @param data dataset
#' @param r power of/order something
#' @param c point in data (default c=0)
#' @return moment of order
#' @export
m_r <- function(data,r,c=0)
{
  sum <- 0
  n <- length(data)
  for(i in 1:n)
  {
    sum <- sum + (data[i]-c)^r
  }
  val <- sum/n
  return(val)
}

#' Fisher's skewness coefficient
#' @description
#' calculates fisher's skewness coefficient
#' @param data dataset
#' @return g1 fisher's skewness coefficient
#' @export
g1 <- function(data)
{
  m <- mean(data)
  mu3 <-m_r(data,3,m)
  val <- mu3/(std_dev(data)^3)
  return(val)
}

#'Fisher's Kurtosis coefficient
#' @description
#' Finds the kurtosis of a dataset
#' @param data dataset
#' @return g2
#' @export
g2 <- function(data)
{
  m <- mean(data)
  mu4 <- m_r(data,4,m)
  val <- mu4/(std_dev(data)^4) -3
  return(val)
}
