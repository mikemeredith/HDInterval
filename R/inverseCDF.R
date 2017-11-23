

# Function to invert a CDF, ie, to get qxxx from pxxx

inverseCDF <- function(p, CDF, ...) {
  if(!is.null(list(...)$log.p))
    stop("'log.p' not yet implemented (ie, always FALSE)")
  if(!is.null(list(...)$lower.tail))
    stop("'lower.tail' not yet implemented (ie, always TRUE)")
  if(p < 0 || p > 1)
    stop("'p' must be a probability in the interval (0, 1)")
  too <- function(x, p, ...) CDF(x, ...) - p
  q <- numeric(length(p))
  for(i in seq_along(p))
    q[i] <- uniroot(too, interval=c(-10,10), extendInt = 'yes', p=p[i], ...)$root
  return(q)
}
