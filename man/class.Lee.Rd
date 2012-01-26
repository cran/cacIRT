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
Computes classification accuracy and consistency with Lee's approach. Basically, the probability of each possible total score conditional on ability is found with a recursive algorithm (see \code{recursive.raw}). Those probabilities are grouped according to the cut scores and used to estimate the indices. See references for details.

}
\usage{
class.Lee(cutscore, ip, ability = NULL, rdm = NULL, quadrature = NULL)
Lee.D(cutscore, quadrature, ip)
Lee.P(cutscore, theta, ip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cutscore}{
%%     ~~Describe \code{cutscore} here~~
A scalar or vector of cut scores on the True Score scale. If you have cut scores on the theta scale, you can transform them with \code{irf} (See example for \code{irf}). Should not include 0 or the max total score, the function will include them.
}
  \item{ip}{
%%     ~~Describe \code{ip} here~~
Matrix of item parameters, columns are discrimination, difficultly, guessing. For 1PL and 2PL, still give a Jx3 matrix, with ip[,1] = 1 and ip[,3] = 0 for example.
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
 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
 Must give only one ability, rdm, or quadrature. If ability is given, those scores are used for the P method. If rdm is given, ability is estimated with MLE (perfect response patterns given a -4 or 4) and used for the P method. If quadrature, the D method is used. \code{class.Lee} calls \code{Lee.D} or \code{Lee.P}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Marginal }{A matrix with two columns of marginal accuracy and consistency per cut score and/or simultaneous}
\item{Conditional}{A matrix of conditional accuracy and conditional consistency returned in the order of subjects if ability or rdm is given}
%% ...
}

\references{
Lee, W. (2010) Classification consistency and accuracy for complex assessments using item response theory. Journal of Educational Measurement, \bold{47}, 1--17.
}

\author{
Quinn Lathrop}
\note{
To expand the code  to other IRT models or to estimate classification indices for weighted total score, the code within Lee.P or Lee.D needs to be modified. Also the recursive algorithm recursive.raw() will need to be adapted for polytomous IRT models.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##from rdm, item parameters denote 4 item 1PL test, cut score at x=2
##only return marginal indices

params<-matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
rdm<-sim(params, rnorm(100))

class.Lee(2, params, rdm = rdm)$Marginal

##or from 40 quadrature points and weights, 2 cut scores

quad <- normal.qu(40)

class.Lee(c(2,3), params, quadrature = quad)$Marginal


}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT }

