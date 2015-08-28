\name{class.Lee}
\alias{class.Lee}
\alias{Lee.D}
\alias{Lee.P}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

Computes classification accuracy and consistency with Lee's approach.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes classification accuracy and consistency with Lee's approach. The probability of each possible total score conditional on ability is found with  \code{recursive.raw}. Those probabilities are grouped according to the cut scores and used to estimate the indices. See references or code for details.

}
\usage{
class.Lee(cutscore, ip, ability = NULL, rdm = NULL, quadrature = NULL, D = 1.7)
Lee.D(cutscore,  ip, quadrature, D = 1.7)
Lee.P(cutscore,  ip, theta, D = 1.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cutscore}{
%%     ~~Describe \code{cutscore} here~~
A scalar or vector of cut scores on the True Score scale. If you have cut scores on the theta scale, you can transform them with \code{irf} (See example for \code{irf}). Should not include 0 or the max total score, as the end points are added internally.
}
  \item{ip}{
%%     ~~Describe \code{ip} here~~
Matrix of item parameters, columns are discrimination, difficultly, guessing, respectively. For 1PL and 2PL, still give a Jx3 matrix, with \code{ip[,1] = 1} and \code{ip[,3] = 0} for the 1PL for example.
}
  \item{ability, theta}{
%%     ~~Describe \code{ability} here~~
Ability estimates for each subject.
}
  \item{rdm}{
%%     ~~Describe \code{rdm} here~~
The response data matrix with rows as subjects and columns as items
}
  \item{quadrature}{
%%     ~~Describe \code{quadrature} here~~
A list containing 1) The quadrature points and 2) Their corresponding weights
}

\item{D}{
Scaling constant for IRT parameters, defaults to 1.7, alternatively often set to 1.
}
 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
 Must give only one ability, rdm, or quadrature. If ability is given, those scores are used for the P method. If rdm is given, ability is estimated with MLE (perfect response patterns given a -4 or 4) and used for the P method. If quadrature, the D method is used. \code{class.Lee} calls \code{Lee.D} or \code{Lee.P}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Marginal}{A matrix with two columns of marginal accuracy and consistency per cut score (and simultaneous if multiple cutscores are given)}
\item{Conditional}{A list of two matrixes, one for conditional accuracy and  one for conditional consistency. Each matrix has one row per subject (or quadrature point).}
%% ...
}

\references{
Lee, W. (2010) Classification consistency and accuracy for complex assessments using item response theory. Journal of Educational Measurement, 47, 1--17.
}

\author{
Quinn N. Lathrop}
\note{
In order to score above a cut, an examinee must score at or above the cut score. Since we are working on the total score scale, be aware that if a cut score is given with a decimal (like 2.4), the examinee must have a total score at the next integer or more (so 3 or more) to score above the cut.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##from rdm, item parameters denote 4 item 1PL test, cut score at x=2
##only print marginal indices

params<-matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
rdm<-sim(params, rnorm(100))

class.Lee(2, params, rdm = rdm)$Marginal

##or from 40 quadrature points and weights, 2 cut scores

quad <- normal.qu(40)

class.Lee(c(2,3), params, quadrature = quad, D = 1)$Marginal


}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT }

