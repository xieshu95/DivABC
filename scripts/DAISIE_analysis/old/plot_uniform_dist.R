plotunif <- function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {

  # Grid of X-axis values
  if (missing(x)) {
    x <- seq(min - 0.5, max + 0.5, 0.01)
  }

  if(max < min) {
    stop("'min' must be lower than 'max'")
  }

  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.005, max + 0.005), type = "l",  ##0.25
       ylim = c(0,1),
       xlab = "Colonization",
       ylab = "Density",
       lty = 0, ...)
  segments(min, 1/(max - min), max, 1/(max - min), col = col, lwd = lwd)
  segments(min - 2, 0, min, 0, lwd = lwd, col = col)
  segments(max, 0, max + 2, 0, lwd = lwd, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = lwd)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = lwd)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}
plotunif(min = 0, max = 2, lwd = 2, col = "black", main = "Prior distribution")



unif_area <- function(min = 0, max = 1, lb, ub, col = 1,
                      acolor = "lightgray", ...) {
  x <- seq(min - 0.25 * max, max + 0.25 * max, 0.00001)

  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  if(max < min) {
    stop("'min' must be lower than 'max'")
  }

  x2 <- seq(lb, ub, length = 1000)
  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.05 * max, max + 0.05 * max), type = "l",
       ylim = c(0,1.0),
       xlab = expression(italic(theta)),
       ylab = "Density", lty = 0, ...)

  y <- dunif(x2, min = min, max = max)
  # polygon(c(lb, x2, ub), c(0, y, 0), col = acolor, lty = 0)
  segments(min, 1/(max - min), max, 1/(max - min), lwd = 2, col = col)
  segments(min - 2 * max, 0, min, 0, lwd = 2, col = col)
  segments(max, 0, max + 2 * max, 0, lwd = 2, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = 2)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = 2)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}
unif_area(min = 0, max = 2.0, lb = 0, ub = 2,
          main = "Prior distribution", acolor = "lightgray")

## normal distribution
normal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(lb, ub, length = 1000)

  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }

  x2 <- seq(lb, ub, length = 1000)
  plot(x, dnorm(x, mean, sd), type = "n",
       xlim = c(0,2),
       # ylim = c(0,1.8),
       xlab = expression(italic(theta)),
       ylab = "Density",...)

  y <- dnorm(x2, mean, sd)
  # polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
}
normal_area(mean = 1, sd = 0.1, lb = 0, ub = 2, lwd = 2,main = "Posterior distribution")

# lognormal
lognormal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(lb, ub , 0.00001)

  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }

  x2 <- seq(lb, ub, 0.00001)
  plot(x, dlnorm(x, mean, sd), type = "n",
       xlim = c(0,2),
       # ylim = c(0,1.0),
       xlab = expression(italic(theta)),
       ylab = "Density",...)

  y <- dlnorm(x2, mean, sd)
  # polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dlnorm(x, mean, sd), type = "l", ...)
}
lognormal_area(mean = 0, sd = 0.3, lb = 0, ub = 2, lwd = 2,main = "Posterior distribution")

# rbinorm
weibull_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(lb, ub , 0.00001)

  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }

  x2 <- seq(lb, ub, 0.00001)
  plot(x, dweibull(x, shape=1, scale = 1), type = "n",
       xlim = c(0,2),
       # ylim = c(0,2.5),
       xlab = expression(italic(theta)),
       ylab = "Density",...)

  y <- dweibull(x, shape=1, scale = 1)
  # polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dweibull(x, shape=1, scale = 1), type = "l", ...)
}
weibull_area(mean = 0, sd = 0.25, lb = 0, ub = 2, lwd = 2,main = "Posterior distribution")

# install.packages("FamilyRank")
library(FamilyRank)
samples <- rbinorm(n=1000, mean1=0.6, mean2=1.2, sd1=0.18, sd2=0.18, prop=.7)
plot(density(samples),
     xlim = c(0,2),
     # ylim = c(0,2.5),
     xlab = expression(italic(theta)),
     ylab = "Density",
     main = NA,
     lwd = 2)

### plot species richness distribution
# lognormal
lognormal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(lb, ub , 0.1)

  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }

  x2 <- seq(lb, ub, 0.1)
  plot(x, dlnorm(x, mean, sd), type = "n",
       xlim = c(0,200),
       # ylim = c(0,1.0),
       xlab = expression(italic(theta)),
       ylab = "Density",...)

  y <- dlnorm(x2, mean, sd)
  # polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dlnorm(x, mean, sd), type = "l", ...)
}
lognormal_area(mean = 3.5, sd = 0.8, lb = 0, ub = 200, lwd = 2,main = "Posterior distribution")
