\name{hdi}
\alias{HDInterval}
\alias{hdi}
\alias{hdi.default}
\alias{hdi.function}
\alias{hdi.matrix}
\alias{hdi.data.frame}
\alias{hdi.list}
\alias{hdi.density}
\alias{hdi.mcmc}
\alias{hdi.mcarray}
\alias{hdi.mcmc.list}
\alias{hdi.bugs}
\alias{hdi.jagsUI}
\alias{hdi.rjags}
\alias{hdi.runjags}

\title{
Highest (Posterior) Density Interval
}
\description{
Calculate the highest density interval (HDI) for a probability distribution for a given probability mass. This is often applied to a Bayesian posterior distribution and is then termed "highest posterior density interval", but can be applied to any distribution, including priors.

The function is an S3 generic, with methods for a range of input objects.
}
\usage{
hdi(object, credMass = 0.95, ...)

\method{hdi}{default}(object, credMass = 0.95, ...)

\method{hdi}{function}(object, credMass = 0.95, tol, ...)

\method{hdi}{matrix}(object, credMass = 0.95, ...)

\method{hdi}{data.frame}(object, credMass = 0.95, ...)

\method{hdi}{list}(object, credMass = 0.95, ...)

\method{hdi}{density}(object, credMass = 0.95, allowSplit=FALSE, ...)

\method{hdi}{mcmc}(object, credMass = 0.95, ...)  % from 'coda'

\method{hdi}{mcmc.list}(object, credMass = 0.95, ...)  % from 'coda'

\method{hdi}{mcarray}(object, credMass = 0.95, ...)  % from 'coda'

\method{hdi}{bugs}(object, credMass = 0.95, ...)  % from 'R2winbugs' or 'R2openbugs'

\method{hdi}{jagsUI}(object, credMass = 0.95, ...)  % from 'jagsUI'

\method{hdi}{rjags}(object, credMass = 0.95, ...)  % from 'R2jags'

\method{hdi}{runjags}(object, credMass = 0.95, ...)  % from 'runjags'

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
only available for objects of class \code{density}; if FALSE and the proper HDI is discontinuous, a single credible interval is returned, but this is not HDI; see Value.
}
  \item{\dots}{
named parameters to be passed to other methods; see Examples.
}
}
\details{
The HDI is the interval which contains the required mass such that all points within the interval have a higher probability density than points outside the interval.

\figure{HDIskew.jpg}

In contrast, a symmetric density interval defined by (eg.) the 10\% and 90\% quantiles may include values with lower probability than those excluded.

For a distribution that is not severely multimodal, the HDI is the narrowest interval containing the specified mass, and the \code{hdi} function actually returns the narrowest interval. This does not always work properly for multimodel densities, where the HDI may be discontinuous (the horizontal black line in the  Figure below). The single interval returned by \code{hdi} (the blue line) may incorrectly include values between the modes with low probability density. The \code{density} method with \code{allowSplit = TRUE} gives separate limits for discontinuous HDIs.

\figure{HDIbimodal.jpg}


The default method expects a vector representing draws from the target distribution, such as is produced by an MCMC process. Missing values are silently ignored; if the vector has no non-missing values, NAs are returned.

The \code{matrix} and \code{data.frame} methods expect an object with vectors of the above type for each parameter in columns. The result is a matrix with parameters in columns and rows with the upper and lower limits of the HDI.

The \code{list} method is a wrapper for \code{lapply(object, hdi, ...)}. It is intended for output from \code{rjags::jags.samples} which returns a list of \code{mcarray} objects.

The \code{mcmc.list}, \code{mcmc} and \code{mcarray} methods expect objects of the respective types as defined in package \pkg{coda}.

The packages \pkg{R2winbugs} and \pkg{R2openbugs} produce \code{bugs} objects; \pkg{R2jags} produces \code{rjags}; \pkg{jagsUI} produces \code{jagsUI}; \pkg{runjags} produces \code{runjags}. For the \pkg{rjags} package, \code{rjags::coda.samples} produces a \code{mcmc.list} object and \code{rjags::jags.samples} produces a list of \code{mcarray} objects.

None of the above use interpolation: the values returned correspond to specific values in the data object, and will be conservative (ie, too wide rather than too narrow). Results thus depend on the random draws, and will be unstable if few values are provided. For a 95\% HDI, 10,000 independent draws are recommended; a smaller number will be adequate for a 80\% HDI, many more for a 99\% HDI.

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
Mike Meredith and John Kruschke. Code for \code{hdi.function} based on \code{hpd} by Greg Snow, corrected by John Kruschke.
}

\examples{
# for a vector:
tst <- rgamma(1e5, 2.5, 2)
hdi(tst)
hdi(tst, credMass=0.8)
# For comparison, the symmetrical 80% CrI:
quantile(tst, c(0.1,0.9))

# for a density:
dens <- density(tst)
hdi(dens, credMass=0.8)

# Now a data frame:
tst <- data.frame(mu = rnorm(1e4, 4, 1), sigma = rlnorm(1e4))
hdi(tst, 0.8)
apply(tst, 2, quantile, c(0.1,0.9))
tst$txt <- LETTERS[1:25]
hdi(tst, 0.8)

# For a function:
hdi(qgamma, 0.8, shape=2.5, rate=2)
# and the symmetrical 80% CrI:
qgamma(c(0.1, 0.9), 2.5, 2)

# A multimodal distribution:
set.seed(2020)
tst2 <- c(rnorm(500, 30, 3), rnorm(200, 50, 1.5), 60 + rexp(200, 0.1))
hist(tst2, breaks = 100, freq=FALSE)
(hdiMC <- hdi(tst2))
segments(hdiMC[1], 0, hdiMC[2], 0, lwd=4, col='red', lend='butt')
# This is a valid 95% CrI, but not a Highest Density Interval

dens2 <- density(tst2, bw="SJ") # not the default for 'bw', see ?density
lines(dens2, lwd=2, col='blue')
(hdiD <- hdi(dens2, allowSplit=TRUE))
(ht <- attr(hdiD, "height"))
segments(hdiD[, 1], ht, hdiD[, 2], ht, lwd=4, col='blue', lend='butt')
# This is the correct 95% HDI.
}
\keyword{methods}
\keyword{htest}
