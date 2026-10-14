#Chapter 2


#' Raw vectors
#' @description
#' Use when working with joint distribution tables 
#' where the X and Y vector components of the bivariate variable 
#' have different lengths to obtain a raw vector of one of the components
#' @param x X component of bivariate variable (X,Y)
#' @param y Y component of bivariate variable (X,Y)
#' @param freq_mat absolute frequency matrix of (X,Y)
#' @return row matrix of the raw vectors x and y
#' @export
raw_vectors <- function(x,y,freq_mat)
{
  x_raw <- c()
  y_raw <- c()
  for(i in 1:nrow(freq_mat))
  {
    for(j in 1:ncol(freq_mat))
    {
      t <- freq_mat[i,j]
      a <- rep(x[i],times=t)
      b <- rep(y[j],times=t)
      
      x_raw <- append(x_raw,a)
      y_raw <- append(y_raw,b)
    }
  }
  z <- append(x_raw,y_raw)
  res <- matrix(z,nrow=2,byrow=TRUE)
  return(res)
}

#'Independence of X and Y
#' @description
#' Function that tests whether X and Y are independent
#' @param X independent variable
#' @param Y dependent variable
#' @param freq_mat relative frequency matrix of bivariate variable (X,Y)
#' @return Boolean TRUE or FALSE
#' @export
isIndependent <- function(freq_mat)
{
  bool <- TRUE
  n <- nrow(freq_mat)
  k <- ncol(freq_mat)
  i <- 1
  while(i <= n && bool)
  {
    j <- 1
    while(j <= k && bool)
    {
      fi <- sum(freq_mat[i,])
      fj <- sum(freq_mat[,j])
      if(freq_mat[i,j] != fi*fj)
      {
        bool <- FALSE
      }
      j <- j+1
    }
    i <- i+1
  }
  return(bool)
}

#' Moment of order 2 dimensions
#' @description
#' computes the moment of order with respect to a point (a,b)
#' if (a,b) = (0,0) then we have an ordinary moment and if (a,b) = (xmean,ymean) then we have a central moment of order
#' @param x vector containing the classmarks of X
#' @param y vector containing the classmarks of Y
#' @param freq matrix containing the relative frequency table for bivariate var (X,Y)
#' @param r order of x
#' @param s order of y
#' @param a x coordinate
#' @param b y coordinate
#' @return moment of order of a dataset with respect to a point (a,b)
#' @export
m_rs <- function(x,y,freq,r,s,a=0,b=0)
{
  sum <- 0
  k <- length(x)
  p <- length(y)
  for(i in 1:k)
  {
    for(j in 1:p)
    {
      sum <- sum + ((x[i]-a)^r)*((y[j]-b)^s)*freq[i,j]
    }
  }
  return(sum)
}

#'Covariance
#' @description
#' Computes the covarinace of X and Y for simple tables and joint distribution of relative freqcuencies
#' @param X classmarks of X
#' @param Y classmarks of Y
#' @param data given as a vector or a matrix depending on dimension parameter, all data is given as relative frequencies
#' @param dimension is the dimension of the data 1 for data vector and 2 for data matrix, default (dimension=2)
#' @return The covariance of X and Y
#' @export
Cov <- function(X,Y,data,dimension=2)
{
  val <- 0
  if(dimension == 2)
  {
    mx <- m_rs(X,Y,data,1,0)
    my <- m_rs(X,Y,data,0,1)
    val <- m_rs(X,Y,data,1,1,mx,my)
  }
  else if(dimension == 1)
  {
    n <- length(data)
    mx <- mean(X)
    my <- mean(Y)
    for(i in 1:n)
    {
      val <- val + (X[i]-mx)*(Y[i]-my)*data[i]
    }
  }
  return(val)
}

#' Mean square error - regression
#' @description Calculates the mean square error for when working with regression lines
#' @param y is the observed data however the error vector can be given directly
#' @param y_est is an optional that is the obtained data through regression and is not necessary if y is the error vector
#Parameters: Can take one or two parameters: (y, y_est) or (err)
#The error- err || the dataset, y, and the estimated set, y_est
#' @return The mean square error when working with regression lines
#' @export
MSE_r <- function(y, y_est=NULL)
{
  err <- 0
  if(is.null(y_est))
  {
    err <- y
  }
  else
  {
    err <- y-y_est
  }
  val <- mean(err^2)
  return(val)
}

#' Coefficient of determination - R^2
#' @description
#' Calculates R^2, not to be confused with r - the linear correlation coefficient r
#' @param y observed dataset
#' @param y_est estimated values of the observed dataset
#' @return numerical R^2 coefficient of determination
#' @export
R2 <- function(y,y_est)
{
  mse <- MSE_r(y,y_est)
  v <- variance(y)
  val <- 1 - mse/v
  return(val)
}

#' Linear correlation coefficient - r
#' @description
#' Calculates r for the bivariate variable (X,Y)
#' @param X independent dataset
#' @param Y dependent dataset
#' @param data data given as a vector or a matrix depending on dimension parameter, all data is given as relative frequencies
#' @param dimension is the dimension of the data 1 for data vector and 2 for data matrix, default (dimension=2)
#' @return r numeric value of linear correlation coefficient
#' @export
r <- function(X,Y,data,dimension=2)
{
  sx <- std_dev(X)
  sy <- std_dev(Y)
  c <- Cov(X,Y,data,dimension)
  val <- c/(sx*sy)
  return(val)
}
