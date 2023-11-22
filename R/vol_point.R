#' Estimates the volatility based on the smile curve estimated parameters
#'
#' @param strike Strike Price.
#' @param under_price Underlying asset price.
#' @param du_mat Time to maturity.
#' @param vol_coef Estimated coefficients for the volatility smile curve.
#'
#' @return
#' @export
#'
#' @examples
#' 
vol_point <- function(strike, under_price, du_mat, vol_coef){
  
  assertthat::assertthat(assertthat::is.number(strike))
  
  assertthat::assertthat(assertthat::is.number(under_price))
  
  assertthat::assertthat(assertthat::is.number(du_mat))
  
  assertthat::assertthat(assertthat::is.number(implicit_vol))
  
  v3 <- log(strike/under_price)/sqrt(du_mat/252)
  
  v4 <- base::ifelse(strike < under_price, 1.1, 1)
  
  v5 <- 10*v4*atan(v3/v4)
  
  v6 <- v5^2
  
  vol <- vol_coef[1] + vol_coef[2]*v5 + vol_coef[3]*v6
  
  return(vol)
}