#' Draw random vectors from multivariate Gaussian in canonical form
#'
#' @param b b parameter (canonical mean)
#' @param P Precision matrix
#'
#' @return Vector of draw from N(P^{-1}b,P^{-1})
#' @export
#'
#' @examples rmvnormcanon(c(0,0),diag(2))
rmvnormcanon <- function(b, P){
  L <- chol(P) 
  u <- solve(t(L), b) 
  v <- solve(L, u) 
  z <- rnorm(n = length(b), mean = 0, sd = 1)
  m <- solve(L, z) 
  Gamma <- v + m
  return(as.vector(Gamma))
}
