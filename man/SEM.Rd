\name{SEM}
\alias{SEM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard error of measurement of MLE estimates of ability 
}
\description{
modified from irtoys, within mlebme I beleive}
\usage{
SEM(ip, x)
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
function(ip,x)
	{
		ti<-tif(ip, x)$f	
   		sem <- sqrt(1/ti)
   		return(sem)
   	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
