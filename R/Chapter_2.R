#Chapter 2

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