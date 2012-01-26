\name{class.Rud}
\alias{class.Rud}
\alias{Rud.P}
\alias{Rud.D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Computes classification accuracy and consistency with Rudner's approach.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes classification accuracy and consistency with Rudner's approach. For each examinee, a normal distribution is created with mean at the ability estimate and standard deviation equal to the standard error of the ability estimate. Rudner's method assumes the standard error is conditionally normally distributed. The area under this normal curve between cut scores is used to estimate the indices. See references.

}
\usage{
class.Rud(cutscore, ip, ability = NULL, se = NULL, rdm = NULL, quadrature = NULL)
Rud.D(cutscore, quadrature, sem)
Rud.P(cutscore, os, sem)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cutscore}{
%%     ~~Describe \code{cutscore} here~~
A scalar or vector of cut scores on the theta scale. Should not include +- inf, the function will include them.
}
  \item{ip}{
%%     ~~Describe \code{ip} here~~
Matrix of item parameters, columns are discrimination, difficultly, guessing. For 1PL and 2PL, still give a Jx3 matrix, with \code{ip[,1] = 1} and \code{ip[,3] = 0} for example.
}
  \item{ability, os}{
%%     ~~Describe \code{ability} here~~
Ability estimates for each subject.
}
  \item{se, sem}{
%%     ~~Describe \code{se} here~~
Standard errors of ability estimates
}
  \item{rdm}{
%%     ~~Describe \code{rdm} here~~
The response data matrix with rows as subjects and columns as items
}
  \item{quadrature}{
%%     ~~Describe \code{quadrature} here~~
A list containing 1) The quadrature points and 2) Their corresponding weigths
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Must give only ability and se, rdm, or quadrature. If ability and se are given, those scores are used for the P method. If rdm is given, ability and se are estimated with MLE (perfect response patterns given a -4 or 4) and used for the P method. If quadrature, the D method is used. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Marginal }{A matrix with two columns of marginal accuracy and consistency per cut score and/or simultaneous}
\item{Conditional }{A matrix of conditional accuracy and conditional consistency returned in the order of subjects if ability or rdm is given}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
Rudner, L. M. (2001) Computing the expected proportions of misclassified examinees. PracticalAssessment, Research & Evaluation, \bold{7(14)}, 1--5. 

Rudner, L. M. (2005) Expected classification accuracy. Practical Assessment Research & Evaluation, \bold{10(13)}, 1--4.
}
\author{
 Quinn Lathrop
}
\note{
%%  ~~further notes~~
\code{class.Rud} is a wrapper for \code{Rud.P} and \code{Rud.D}. 

To expand the code  to other IRT models for example, the code within \code{Rud.P} or \code{Rud.D} may need to be modified. If you can supply the theta estimates and their se's (which are conditionally normal), then the function should work as is.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##from rdm, item parameters denote 4 item 1PL test, cut score at theta=.5
##only return marginal indices

params<-matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
rdm<-sim(params, rnorm(100))

class.Rud(.5, params, rdm = rdm)$Marginal

##or from 40 quadrature points and weights, 2 cut scores

quad <- normal.qu(40)

class.Rud(c(-.5,1.5), params, quadrature = quad)$Marginal

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT }

