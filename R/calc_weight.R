#' Calculate weight of each particle
#'
#' @inheritParams default_params_doc
#'
#' @return A numeric as the estimated weight
#' @export

calc_weight <- function(weights, particles,
                        current, sigma, prior_density_function,idparsopt) {
  vals <- c()
  for (i in seq_along(particles)) {
    vals[i] <- weights[i]
    for (j in idparsopt) {
      diff <- log(current[j]) - log(particles[[i]][j])
      vals[i] <- vals[i] * stats::dnorm(diff, mean = 0, sd = sigma)
    }
  }
  # current_opt <- current[idparsopt]
  numerator <- prior_density_function(current,idparsopt)

  return(numerator / sum(vals))
}
