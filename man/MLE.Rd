\name{MLE}
\alias{MLE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Maximum likelihood estimates of ability
}
\description{
Computes maximum likelihood estimates of ability. Modified from irtoys mlebme()}
\usage{
MLE(resp, ip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resp}{
%%     ~~Describe \code{resp} here~~
}
  \item{ip}{
%%     ~~Describe \code{ip} here~~
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
function(resp,ip)
	{
		np = nrow(resp)
		logf<-function (x, r, p) {
	    	pr = p[,3] + (1 - p[,3])/(1 + exp(-1.7*p[, 1] * (x-p[, 2])))
	    	ll = r * log(pr) + (1 - r) * log(1 - pr)
	    	lf = sum(ll)
	    	return(lf)}    
		esti<-function(x,resp,ip) optimize(logf, lower = -4, upper = 4, maximum = TRUE, r = resp, p = ip)$maximum
	
	    o = sapply(1:np, function(i) esti(resp = resp[i, ],ip=ip))
	    return((o))
	 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
