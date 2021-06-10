#' Calculate weight of each particle
#'
#' @param weights A vector of weights
#' @param particles A list of parameter combinations
#' @param current A vector of current parameter combination to determine the
#'                weight.
#' @param sigma Standard deviation of the pertubation.
#' @param prior_density_function Function to calculate the prior probability.
#'
#' @return A numeric as the estimated weight
#' @author Shu Xie
#' @export


calc_weight <- function(weights, particles,
                        current, sigma, prior_density_function) {
  vals <- c()
  for (i in seq_along(particles)) {
    vals[i] <- weights[i]
    for (j in seq_along(current)) {
      diff <- log(current[j]) - log(particles[[i]][j])
      vals[i] <- vals[i] * stats::dnorm(diff, mean = 0, sd = sigma)
    }
  }

  numerator <- prior_density_function(current)

  return(numerator / sum(vals))
}