#' Gibbs-Sampler für Poisson-Bruchpunktmodell
#'
#' @param size Anzahl Iterationen
#' @param y Daten
#'
#' @return Ziehungen aus der Posteriori von (lambda, alpha, theta)
#' @export
#'
#' @author Michael Höhle (?)
#' 
breakpoint.gibbs <- function(size, y) {
  # Time points
  n <- length(y)
  time <- 1:n
  # Allocate result matrix
  samples <- matrix(NA, nrow = size, ncol = 4)
  dimnames(samples) <- list(NULL, c("lambda1", "lambda2", "alpha", "theta"))
  
  # Start values 
  lambda <- c(1,1)
  alpha <- 1
  theta <- round(n/2)
  # store in result matrix
  samples[1,] <- c(lambda, alpha, theta)
  
  # Gibbs sampler
  for (i in 2:size) {
    # Draw from the full conditionals for lambda[1], lambda[2] and alpha
    lambda[1] <- rgamma(1, 3 + sum(y[time[time <= theta]]), alpha + theta)
    lambda[2] <- rgamma(1, 3 + sum(y[time[time>theta]]), alpha + n - theta)
    alpha     <- rgamma(1, 16, 10 + sum(lambda))
    # Calculate the probabilities for the full conditional of theta
    # (for all timepoints)
    theta.prob <- exp(time * (lambda[2]-lambda[1])) *
      exp(cumsum(y) * log(lambda[1]/lambda[2]) )
    # Draw a value from the full conditional for theta (with calculated probabilities)
    theta <- sample(time, size = 1, prob = theta.prob)
    
    #Save into result matrix
    samples[i,] <- c(lambda, alpha, theta)
  }
  return(samples)
}
