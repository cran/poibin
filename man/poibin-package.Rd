\name{poibin-package}
\alias{poibin-package}
\alias{poibin}
\docType{package}
\title{
The Poisson Binomial Distribution.

}
\description{
The cdf, pmf, quantile function, and random number generation for the Poisson binomial distribution.

}
\details{
\tabular{ll}{
Package: \tab poibin\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2012-11-07\cr
License: \tab GPL-2 \cr
LazyLoad: \tab yes\cr
}

This package implements both the exact and approximation methods for computing the cdf of the Poisson binomial distribution. It also provides the pmf, quantile function, and random number generation for the Poisson binomial distribution.

}
\author{
Yili Hong

Maintainer: Yili Hong <yilihong@vt.edu>

}
\references{
Hong, Y. (2012). On computing the distribution function for the Poisson binomial distribution. Computational Statistics & Data Analysis. Available online, http://dx.doi.org/10.1016/j.csda.2012.10.006.
}
\keyword{ package }

\examples{
   kk=0:10
   pp=c(.1,.2,.3,.4,.5)
   ppoibin(kk=kk, pp=pp, method = "DFT-CF",wts=rep(2,5))
   ppoibin(kk=kk, pp=pp, method = "RF",wts=rep(2,5))
   ppoibin(kk=kk, pp=pp, method = "RNA",wts=rep(2,5))
   ppoibin(kk=kk, pp=pp, method = "NA",wts=rep(2,5))
   dpoibin(kk=kk, pp=pp,wts=rep(2,5))
   qpoibin(qq=0:10/10,pp=pp,wts=rep(2,5))
   rpoibin(m=2,pp=pp,wts=rep(2,5))
}
