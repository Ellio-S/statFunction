#Chapter 2

#Mean square error - MSE
#Parameters: Can take one or two parameters: (y, y_est) or (err)
#The error- err || the dataset, y, and the estimated set, y_est
#Return the mean square error
#' @export
MSE <- function(y, y_est=NULL)
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