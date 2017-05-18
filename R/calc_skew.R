#' Calculate skew of a catch distribution

#' Function to calculate the skew of a distribution (third moment)

#' @param input Vector of values 

#' @export
#' @examples
#' calc_skew(input = rnorm(1000))

calc_skew <- function(input){
  #Remove NAs
# browser()  
  nn <- length(input)
  
  xhat <- mean(input)
  num <- nn * sum((input - xhat) ^ 3)
  denom <- (nn - 1) * (nn - 2) * sd(input) ^ 3
  skew <- num / denom
  #Equation is from this website...
  #http://www.real-statistics.com/descriptive-statistics/symmetry-skewness-kurtosis/

  return(skew)
}
