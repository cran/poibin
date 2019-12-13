\name{poibin}
\alias{ppoibin}
\alias{dpoibin}
\alias{qpoibin}
\alias{rpoibin}
\title{
The Poisson Binomial Distribution.
}
\description{
The cdf, pmf, quantile function, and random number generation for the Poisson binomial distribution.
}
\usage{
ppoibin(kk, pp, method = "DFT-CF",wts=NULL)
dpoibin(kk, pp,wts=NULL)
qpoibin(qq, pp,wts=NULL)
rpoibin(m, pp,wts=NULL)
}
\arguments{
  \item{kk}{
   The values where the cdf or pmf to be evaluated.
}
  \item{pp}{ The vector for \eqn{p_j}'s which are the sucess probabilities for indicators.
}
  \item{method}{ "DFT-CF" for the DFT-CF method, "RF" for the recursive formula, "RNA" for the refined normal approximation,
    "NA" for the normal approximation, and "PA" for the Poisson approximation.
}
  \item{wts}{ The weights for \eqn{p_j}'s.
}
  \item{qq}{ The values where the quantile function to be evaluated.
}
  \item{m}{ The number of random numbers to be generated.
}
}
\details{
See the reference for computational details.
}
\value{
      Returns the entire cdf, pmf, quantiles, and random numbers.
}
\references{
Hong, Y. (2013). On computing the distribution function for the Poisson binomial distribution. Computational Statistics & Data Analysis, Vol. 59, pp. 41-51.
}
\author{

Yili Hong

}


\examples{
   kk=0:10
   pp=c(.1,.2,.3,.4,.5)
   ppoibin(kk=kk, pp=pp, method = "DFT-CF",wts=rep(2,5))
   ppoibin(kk=kk, pp=pp, method = "RF",wts=rep(2,5))
   ppoibin(kk=kk, pp=pp, method = "RNA",wts=rep(2,5))
   ppoibin(kk=kk, pp=pp, method = "NA",wts=rep(2,5))
   ppoibin(kk=kk, pp=pp, method = "PA",wts=rep(2,5))
   dpoibin(kk=kk, pp=pp,wts=rep(2,5))
   qpoibin(qq=0:10/10,pp=pp,wts=rep(2,5))
   rpoibin(m=2,pp=pp,wts=rep(2,5))
}


