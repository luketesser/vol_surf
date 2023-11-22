#' Estimates the coefficients for the volatility smile
#'
#' @param strike Strike Price.
#' @param under_price Underlying asset price.
#' @param du_mat Time to maturity.
#' @param implicit_vol Observed volatility from trading derivatives of the underlying asset.
#'
#' @return Three Coefficients that represent the ATM vol, skew and kurtosis of the smile curve respectively.
#' @export
#'
#' @examples
vol_coef <- function(strike, under_price, du_mat, implicit_vol){
  
  assertthat::assertthat(assertthat::is.number(strike))
  
  assertthat::assertthat(assertthat::is.number(under_price))
  
  assertthat::assertthat(assertthat::is.number(du_mat))
  
  assertthat::assertthat(assertthat::is.number(implicit_vol))
  
  v1 <- 10 * log(strike/under_price)
  
  v2 <- v1^2
  
  v3 <- log(strike / under_price) / sqrt(du_mat / 252)
  
  v4 <- base::ifelse(strike < under_price, 1.1, 1)
  
  v5 <- 10 * v4 * atan(v3 / v4)
  
  v6 <- v5^2
  
  v7 <- stats::lm(implicit_vol ~ v5 + v6)$coefficients
  
  return(v7)
}