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
Modified from the package \code{irtoys}.
}
\usage{
iif(ip, x, D = 1.7)
irf(ip, x, D = 1.7)
MLE(resp, ip, D = 1.7, min= -4, max = 4)
normal.qu(n = 15, lower = -4, upper = 4, mu = 0, sigma = 1)
SEM(ip, x, D = 1.7)
sim(ip, x, D = 1.7)
tif(ip, x, D = 1.7)
}



\arguments{
  \item{ip}{
A Jx3 matrix of item parameters. Columns are discrimination, difficulty, and guessing}
  \item{x}{Vector of theta points}
\item{resp}{Response data matrix, subjects by items}
\item{min, max}{MLE is undefined for perfect scores. These parameters define the range in which to search for the MLE, if the score is perfect, the min or max will be returned.}
\item{n}{Number of quadrature points wanted}
\item{lower, upper}{Range of points wanted}
\item{mu, sigma}{The normal distribution from which points and weights are taken}
\item{D}{
The scaling constant for the IRT parameters, defaults to 1.7, alternatively often set to 1.}
}
\details{
\code{iif} gives item information, \code{irf} gives item response function, \code{MLE} returns maximum likelihood estimates of theta (perfect scores get +-4), \code{normal.qu} returns a list length 2 of normal quadrature points and weights, \code{SEM} gives the standard error of measurement at the given ability points, \code{sim} returns simulated response matrix, \code{tif} gives the test information function.
}

\references{
Partchev, I. (2014) irtoys: Simple interface to the estimation and plotting of IRT models. R package version 0.1.7.

}
\author{
Quinn N. Lathrop
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


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
