\name{hpdi}
\alias{hpdi}
\alias{hpdi.default}
\alias{hpdi.function}
\alias{hpdi.matrix}
\alias{hpdi.data.frame}
\alias{hpdi.density}
\alias{hpdi.mcmc.list}
\alias{hpdi.bugs}
\alias{hpdi.rjags}
\alias{hpdi.runjags}

\title{
Highest (Posterior) Density Interval
}
\description{
Calculate the highest density interval (HDI) for a given probability mass, see Details. The function is generic, with methods for a range of input objects.
}
\usage{
hpdi(object, credMass = 0.95, ...)

\method{hpdi}{default}(object, credMass = 0.95, ...)

\method{hpdi}{function}(object, credMass = 0.95, tol, ...)

\method{hpdi}{matrix}(object, credMass = 0.95, ...)

\method{hpdi}{data.frame}(object, credMass = 0.95, ...)

\method{hpdi}{density}(object, credMass = 0.95, allowSplit=FALSE, ...)

\method{hpdi}{mcmc.list}(object, credMass = 0.95, ...)

\method{hpdi}{bugs}(object, credMass = 0.95, ...)

\method{hpdi}{rjags}(object, credMass = 0.95, ...)

\method{hpdi}{runjags}(object, credMass = 0.95, ...)

}
\arguments{
  \item{object}{
an object specifying the target distribution; see Details.
}
  \item{credMass}{
a scalar [0, 1] specifying the mass within the credible interval.
}
  \item{tol}{
the desired accuracy; see \code{\link{optimize}}; default is 1e-8.
}
  \item{allowSplit}{
if FALSE and the proper HDI is discontinuous, a single credible interval is returned, but this is not HDI; see Value.
}
  \item{\dots}{
named parameters to be passed to other methods; see Examples.
}
}
\details{
The HDI is the interval which contains the required mass such that all points within the interval have a higher probability density than points outside the interval. When applied to a posterior probability density, it is often known as the Highest Posterior Density (HPD).

\figure{HDIskew.jpg}

In contrast, a symmetric density interval defined by (eg.) the 10\% and 90\% quantiles may include values with lower probability than those excluded.

For a distribution that is not severely multimodal, the HDI is the narrowest interval containing the specified mass, and the \code{hpdi} function actually returns the narrowest interval. This does not always work properly for multimodel densities, where the HDI may be discontinuous (the horizontal black line in the  Figure below). The single interval returned by \code{hpdi} (the blue line) may incorrectly include values between the modes with low probability density. The \code{density} method with \code{combine = FALSE} gives separate limits for discontinuous HDIs.

\figure{HDIbimodal.jpg}


The default method expects a vector representing draws from the target distribution, such as is produced by an MCMC process. Missing values are silently ignored; if the vector has no non-missing values, NAs are returned.

The matrix and data frame methods expect an object with vectors of the above type for each parameter in columns. The result is a matrix with parameters in columns and rows with the upper and lower limits of the HDI.

The mcmc.list method expects an object of type mcmc.list as defined in package coda.

None of the above use interpolation: the values returned correspond to specific values in the data object, and should be conservative (ie, too wide rather than too narrow). Results thus depend on the random draws, and will be unstable if few values are provided. For a 95\% HDI, 10,000 independent draws are recommended; a smaller number will be adequate for a 80\% HDI, many more for a 99\% HDI.

The function method requires the name for the inverse cumulative density function (ICDF) of the distribution; standard R functions for this have a \code{q-} prefix, eg. \code{qbeta}. Arguments required by the ICDF must be specified by their (abbreviated) names; see the examples.
}
\value{
a vector of length 2 or a 2-row matrix with the lower and upper limits of the HDI, with an attribute "credMass". 

The \code{density} method with \code{allowSplit=TRUE} produces a matrix with a row for each component of a discontinuous HDI and columns for begin and end. It has an additional attribute "height" giving the probability density at the limits of the HDI.
}
\references{
Kruschke, J. K. 2011. \emph{Doing Bayesian data analysis: a tutorial with R and BUGS.} Elsevier, Amsterdam, section 3.3.5.
}
\author{
Mike Meredith. Code for \code{hpdi.function} based on \code{hpd} by Greg Snow, corrected by John Kruschke.
}

\examples{
# for a vector:
tst <- rgamma(1e5, 2.5, 2)
hpdi(tst)
hpdi(tst, credMass=0.8)
# For comparison, the symmetrical 80% CrI:
quantile(tst, c(0.1,0.9))

# for a density:
dens <- density(tst)
hpdi(dens, credMass=0.8)

# Now a data frame:
tst <- data.frame(mu = rnorm(1e4, 4, 1), sigma = rlnorm(1e4))
hpdi(tst, 0.8)
apply(tst, 2, quantile, c(0.1,0.9))

# For a function:
hpdi(qgamma, 0.8, shape=2.5, rate=2)
# and the symmetrical 80% CrI:
qgamma(c(0.1, 0.9), 2.5, 2)

# A severely bimodal distribution:
tst2 <- c(rnorm(1e5), rnorm(5e4, 7))
hist(tst2, freq=FALSE)
(hdiMC <- hpdi(tst2))
segments(hdiMC[1], 0, hdiMC[2], 0, lwd=3, col='red')
# This is a valid 95% CrI, but not a Highest Density Interval

dens2 <- density(tst2)
lines(dens2, lwd=2, col='blue')
(hdiD1 <- hpdi(dens2))  # default combine = TRUE; note the warning
(ht <- attr(hdiD1, "height"))
segments(hdiD1[1], ht, hdiD1[2], ht, lty=3, col='blue')
# This is not a valid 95% CrI
(hdiD2 <- hpdi(dens2, allowSplit=TRUE))
segments(hdiD2[, 1], ht, hdiD2[, 2], ht, lwd=3, col='blue')
# This is the correct 95% HDI.
}
\keyword{methods}
\keyword{htest}
