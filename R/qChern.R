#' Quantile function of Chernoff's distribution
#'
#' Computes the quantiles of Chernoff's distribution.
#'
#' @param p evaluation point of the quantile function.
#' @return The function returns Chernoff's quantile function evaluated at p.
#' @export
#' @examples
#' qChern(0.5)
qChern <- function(p){
  return(stats::uniroot(function(x){pChern(x)-p},c(stats::qnorm(p,sd=0.52)-0.1,stats::qnorm(p,sd=0.52)+0.1),tol=1e-10)$root)
}
