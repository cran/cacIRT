\name{class.Rud}
\alias{class.Rud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Computes classification accuracy and consistency with Rudner's approach.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes classification accuracy and consistency with Rudner's approach.

}
\usage{
class.Rud(cutscore, ip, irt.model = "dich", ability = NULL, se = NULL, rdm = NULL, quadrature = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cutscore}{
%%     ~~Describe \code{cutscore} here~~
A scalar or vector of cut scores on the theta scale
}
  \item{ip}{
%%     ~~Describe \code{ip} here~~
Matrix of item parameters, for 3PL columns assumed to be discrimination, difficultly, guessing.
}
  \item{irt.model}{
%%     ~~Describe \code{irt.model} here~~
Currently must by dich for dichotomous
}
  \item{ability}{
%%     ~~Describe \code{ability} here~~
Ability estimates for each subject.
}
  \item{se}{
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
\item{Marginal }{A matrix with two columns of marginal accuracy and consistency per cut score}
\item{Conditional }{A list of conditional accuracy and conditional consistency returned in the order of subjects if ability or rdm is given}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  Quinn Lathrop
}
\note{
%%  ~~further notes~~
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

class.Rud(2, params, rdm = rdm)$Marginal

##or from 40 quadrature points and weights

quad <- normal.qu(40)

class.Rud(2, params, quadrature = quad)$Marginal

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT }

