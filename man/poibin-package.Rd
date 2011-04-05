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
Version: \tab 1.0\cr
Date: \tab 2011-04-05\cr
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
Hong, Y. (2011). On computing the distribution function for the sum of independent and
  non-identical random indicators. Technical Reports No. 11-2: Department of Statitics, Virginia Tech,
  Blacksburg, VA 24061. Available at http://www.stat.vt.edu/tech_reports/index.html.
}
\keyword{ package }
\seealso{

}
\examples{
   kk=0:5
   pp=c(.1,.2,.3,.4,.5)
   ppoibin(kk=kk, pp=pp, method = "DFT-CF")
   ppoibin(kk=kk, pp=pp, method = "RF")
   ppoibin(kk=kk, pp=pp, method = "RNA")
   ppoibin(kk=kk, pp=pp, method = "NA")
   dpoibin(kk=kk, pp=pp)
   qpoibin(qq=0:10/10,pp=rep(.2,10))
   rpoibin(2,c(.9,.9))
}
