\name{poibin-package}
\alias{poibin-package}
\alias{poibin}
\docType{package}
\title{
\packageTitle{poibin}
}
\description{
\packageDescription{poibin}
}
\details{

\packageDESCRIPTION{poibin}
\packageIndices{poibin}

}
\author{
\packageAuthor{poibin}

Maintainer: \packageMaintainer{poibin}
}
\references{
Hong, Y. (2013). On computing the distribution function for the Poisson binomial distribution. Computational Statistics & Data Analysis, Vol. 59, pp. 41-51.
}

\keyword{ package }

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
