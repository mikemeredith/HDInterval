# This file has the S3 generic 'hpdi' function and a series of methods.

hpdi <- function(object, credMass=0.95, ...) UseMethod("hpdi")

hpdi.default <- function(object, credMass=0.95, ...) {
  if(!is.numeric(object))
    stop(paste("No applicable method for class", class(object)))
  checkCredMass(credMass)
  if(is.na(credMass) || length(credMass) != 1 || credMass <= 0 || credMass >= 1)
    stop("credMass must be in 0 < credMass < 1")
  if(all(is.na(object)))
    return(c(lower = NA_real_, upper = NA_real_))
  # This is Mike's code from way back:
  x <- sort(object)  # also removes NAs
  n <- length(x)
  # exclude <- ceiling(n * (1 - credMass)) # Not always the same as...
  exclude <- n - floor(n * credMass)       # Number of values to exclude
  low.poss <- x[1:exclude]             # Possible lower limits...
  upp.poss <- x[(n - exclude + 1):n]   # ... and corresponding upper limits
  best <- which.min(upp.poss - low.poss)      # Combination giving the narrowest interval
  result <- c(lower = low.poss[best], upper = upp.poss[best])

  attr(result, "credMass") <- credMass
  return(result)
}

hpdi.matrix <- function(object, credMass=0.95, ...) {
  result <- apply(object, 2, hpdi.default, credMass=credMass, ...)
  attr(result, "credMass") <- credMass
  return(result)
}

hpdi.data.frame <- function(object, credMass=0.95, ...)
  hpdi.matrix(as.matrix(object), credMass=credMass, ...)


hpdi.mcmc.list <- function(object, credMass=0.95, ...)
  hpdi.matrix(as.matrix(object), credMass=credMass, ...)

hpdi.mcmc <- function(object, credMass=0.95, ...)
  hpdi.matrix(as.matrix(object), credMass=credMass, ...)

hpdi.bugs <- function(object, credMass=0.95, ...)
  hpdi.matrix(object$sims.matrix, credMass=credMass, ...)

hpdi.rjags <- function(object, credMass=0.95, ...)
  hpdi.matrix(object$BUGSoutput$sims.matrix, credMass=credMass, ...)

hpdi.runjags <- function(object, credMass=0.95, ...)
  hpdi.mcmc.list(as.mcmc.list(object), credMass=credMass, ...)

hpdi.jagsUI <- function(object, credMass=0.95, ...)
  hpdi.mcmc.list(object$samples, credMass=credMass, ...)




