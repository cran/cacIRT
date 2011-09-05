\name{sim}
\alias{sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate response data matrix from 3PL model
}
\description{
modified from irtoys}
\usage{
sim(ip, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ip}{
%%     ~~Describe \code{ip} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
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
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(ip,x){
	    i = irf(ip, x)
    	d = dim(i$f)
   		u = runif(d[1] * d[2])
    	dim(u) = d
    	return(ifelse(i$f > u, 1, 0))}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
