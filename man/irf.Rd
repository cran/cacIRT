\name{irf}
\alias{irf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Item response function
}
\description{
Modified from irtoys
}
\usage{
irf(ip, x)
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
	    ni = dim(ip)[1]
	    f = (sapply(1:ni, function(i) {ip[i, 3] + (1 - ip[i, 3])/(1 + exp(-1.7*ip[i, 1] * (x-ip[i, 2])))}))
	    r = list(x = x, f = f)
	    return(r)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
