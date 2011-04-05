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
ppoibin(kk, pp, method = "DFT-CF")
dpoibin(kk, pp)
qpoibin(qq, pp)
rpoibin(m, pp)
}
\arguments{
  \item{kk}{
   The values where the cdf or pmf to be evaluated.
}
  \item{pp}{ The vector for \eqn{p_j}'s which are the sucess probabilities for indicators.
}
  \item{method}{ "DFT-CF" for the DFT-CF method, "RF" for the recursive formula, "RNA" for the refined normal approximation,
    "NA" for the normal approximation.
}
  \item{qq}{ The values where the quantile function to be evaluated.
}
  \item{m}{ The number of random numbers to be generated.
}
}
\details{
See the reference for computational details.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
      Returns the entire cdf, pmf, quantiles, and random numbers.
}
\references{
Hong, Y. (2011). On computing the distribution function for the sum of independent and
  non-identical random indicators. Technical Reports No. 11-2: Department of Statitics, Virginia Tech,
  Blacksburg, VA 24061. Available at http://www.stat.vt.edu/tech_reports/index.html.
  
}
\author{

Yili Hong

}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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


