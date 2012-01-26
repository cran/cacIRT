\name{recursive.raw}
\alias{recursive.raw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recursive computation of conditional total score
}
\description{
Returns probabilities of each possible total score conditional on theta}
\usage{
recursive.raw(theta, ip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Vector of ability estimates or points to condition on.
}
  \item{ip}{
Jx3 matrix of item parameters, columns are discrimination, difficulty, and guessing.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A matrix of theta points by possible score 0,1, . . . ,J.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Quinn Lathrop
}
\note{
As described in Huynh 1990
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
params<-matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
rec.mat <- recursive.raw(c(-1,0,1), params)}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT}

