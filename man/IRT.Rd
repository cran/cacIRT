\name{Useful IRT Functions}
\alias{Useful IRT Functions}
\alias{iif}
\alias{irf}
\alias{MLE}
\alias{normal.qu}
\alias{SEM}
\alias{sim}
\alias{tif}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A collection of useful IRT functions.
}
\description{
Most are modified from the package irtoys, using a scaling of 1.7 is used (so that logistic models approximate the normal ogive).
}
\usage{
iif(ip, x)
irf(ip, x)
MLE(resp, ip)
normal.qu(n = 15, lower = -4, upper = 4, mu = 0, sigma = 1)
SEM(ip, x)
sim(ip, x)
tif(ip, x)
}



\arguments{
  \item{ip}{
A Jx3 matrix of item parameters. Columns are discrimination, difficulty, and guessing}
  \item{x}{Vector of theta points}
\item{resp}{Response data matrix, subjects by items}
\item{n}{Number of quadrature points wanted}
\item{lower, upper}{Range of points wanted}
\item{mu, sigma}{The normal distribution from which points and weights are taken}
}
\details{
\code{iif} give item information, \code{irf} gives item response function, \code{MLE} returns maximum likelihood estimates of theta (perfect scores get +-4), \code{normal.qu} returns a list length 2 of quadrature points and weights, \code{SEM} gives the inverse of the test information, \code{sim} returns simulated response matrix, \code{tif} gives the test information function.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
see R package \code{irtoys} for further details
}
\author{
Quinn Lathrop
}
\note{
see R package \code{irtoys} for further details
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
params<-matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
rdm<-sim(params, rnorm(100))

theta.hat <- MLE(rdm, params)
theta.se  <- SEM(rdm, params)

## transform a cut score of theta = 0 to the expected true score scale

	t.cut <- 0
	x.cut <- sum(irf(params, t.cut)$f)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IRT }
