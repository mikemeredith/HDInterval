\name{inverseCDF}
\alias{inverseCDF}

\title{
Inverse Cumulative Density Function
}
\description{
Given a cumulative density function, calculates the quantiles coresponding to given probabilities, ie, "converts" a CDF to an ICDF. The function method for \code{hdi} requires an ICDF, which is not always available for custom distributions.
}
\usage{
inverseCDF(p, CDF, ...)
}
\arguments{
  \item{p}{
a vector of probabilities
}
  \item{CDF}{
a cummulative density function; standard CDFs in R begin with \code{p-}, eg, \code{pnorm}.
}
  \item{\dots}{
named parameters to be passed to the CDF function; see Examples; \code{log.p} and \code{lower.tail} are not supported and generate an error.
}
}
\details{
The function uses a search algorithm to find the value of \code{q} which corresponds to \code{p}. This suffers from imprecision, especially for sections of the CDF which are relatively flat, as is usually the case close to p = 0 or 1.
}
\value{
a vector of the same length as \code{p} with the corresponding quantiles.
}
\author{
Mike Meredith
}

\examples{
# Try with pgamma/qgamma
inverseCDF(c(0.025, 0.975), pgamma, shape=2.5, rate=2) # 95\% interval
qgamma(c(0.025, 0.975), shape=2.5, rate=2) # for comparison

# Plug inverseCDF into hdi, need to specify the CDF
hdi(inverseCDF, CDF=pgamma, shape=2.5, rate=2)
hdi(qgamma, shape=2.5, rate=2) # for comparison

# for a custom density, here a mixture of normals
# the PDF
dmixg <- function(x)
  0.6 * dnorm(x, 0, 1) + 0.4 * dnorm(x, 4, 2^0.5)
curve(dmixg, -5, 10)
# and the CDF
pmixg <- function(q)
  0.6 * pnorm(q, 0, 1) + 0.4 * pnorm(q, 4, 2^0.5)
curve(pmixg, -5, 10)
# Now plug into inverseCDF and hdi
inverseCDF(c(0.025, 0.975), pmixg)
hdi(inverseCDF, CDF=pmixg)
}
\keyword{distribution}
