#' Cumulative distribution function of Chernoff's distribution
#'
#' Computes the CDF of Chernoff's distribution.
#'
#' @param q evaluation point of the distribution function.
#' @return The function returns Chernoff's distribution function evaluated at q.
#' @export
#' @examples
#' pChern(0)
pChern <- function(q){

  if (q >=0){
  return(0.5+stats::integrate(dChern,lower=0,upper=q)$value)}
  else{return(0.5-stats::integrate(dChern,lower=0,upper=-q)$value)}

}
