\name{Nonparametric Approach to CA and CC}
\alias{Lee.pnr}
\alias{pnr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

Computes classification accuracy and consistency using Lathrop and Cheng's (2014) nonparametric approach.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes classification accuracy and consistency with Lathrop & Cheng's (2014) approach. First, the kernel-smoothed estimate of the probability of a correct response, conditional on observed total score, is found with \code{pnr()}. Then, the method proceeds similar to \code{class.Lee()}. Using the nonparametric approach does not require a parametric IRT model, keeps the problem on the total score scale, and can produce more accurate CA and CC estimates when the IRT model's assumptions are violated (see Lathrop & Cheng, 2014).

}
\usage{
Lee.pnr(cutscore, pnr.out)
pnr(resp, bw.g = NULL, alpha = .5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cutscore}{
%%     ~~Describe \code{cutscore} here~~
A scalar or vector of cut scores on the total score scale. Should not include 0 or the max total score, as the end points are added internally.
}
\item{pnr.out}{
The output from \code{pnr()}. It is a list of length 3 where

\code{pnr.out[[1]]} is a vector of T evaluation points on the total score scale (integers from 0 to the max total score)

\code{pnr.out[[2]]} is a vector of the observed density at each evaluation point

\code{pnr.out[[3]]} is a TxMxJ array. Each slice is an item. Within a slice, rows are for evaluation points and columns are for the probability of the score category. This has a similar structure to \code{Pij} seen in \code{Lee.poly()}

}
\item{resp}{
The response data matrix with rows as subjects and columns as items. Because the method is based on total score, the method is not robust to missing data. Any \code{NA} in \code{resp} will propogate to the output.
}

\item{bw.g}{
The global bandwidth parameter. The default of NULL will estimate the global bandwidth with the optimal (in terms of MSE) estimate of the bandwidth for normally distributed variables. The default is generally a good starting point.
}

\item{alpha}{
The adaptivity of the bandwidth parameter. A value of 0 means no adaptation and each evaluation point uses the value in \code{bw.g}. For, other values (up to and including 1), the bandwidth parameter will shrink if the evaluation point is in an area of high density and grow when the evaluation point is in an area of low density. A value of 0.5 is default and generally recommended.
}


%%I am stopping here. Still need to fix this.
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Marginal}{A matrix with two columns of marginal accuracy and consistency per cut score (and simultaneous if multiple cutscores are given)}
\item{Conditional}{A list of two matrixes, one for conditional accuracy and  one for conditional consistency. Each matrix has one row per evaluation point.}
%% ...
}

\note{
The function \code{pnr()} is modified from Ramsay's (1991) kernel-smoothed response functions, specifically because they occur conditional total score (and not conditional on a latent trait) and the addition of an adaptive bandwidth (which helps performance when the distribution of total scores is not normal.)

There is no "D" method of marginalization (as there is for \code{class.Rud} and \code{class.Lee}). But if there is a theoretical distribution of total scores, the \code{pnr.out[[2]]} can be adjusted to match this theoretical distribution.
}

\references{
Lathrop, Q. N., & Cheng, Y. (2014). A Nonparametric Approach to Estimate Classification Accuracy and Consistency. Journal of Educational Measurement, 51(3), 318-334.

Lee, W. (2010) Classification consistency and accuracy for complex assessments using item response theory. Journal of Educational Measurement, 47, 1-17.

Ramsay, J. O. (1991). Kernel Smoothing Approaches to Item Characteristic Curve Estimation. Psychometrika, 56(4), 611-630.
}

\author{
Quinn N. Lathrop}


\examples{
#Simulate simple response data

params <- matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
theta <- rnorm(100)
rdm <- sim(params, theta)

pnr.out <- pnr(rdm)

resultsNP <- Lee.pnr(3, pnr.out)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT }

