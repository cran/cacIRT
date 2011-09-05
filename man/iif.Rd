\name{iif}
\alias{iif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Item information function
}
\description{
Modified from irtoys
}
\usage{
iif(ip, x)
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
function (ip, x) 
	{
	    if (is.null(dim(ip))) 
	        dim(ip) = c(1, 3)
        p3 = irf(ip, x)$f
        q3 = 1 - p3
        
        f<-matrix(NA,nrow = length(x),ncol = dim(ip)[1])
        
        for(i in 1:length(x)){ for(j in 1:dim(ip)[1]){
        		
        f[i,j] = (1.7*ip[j, 1])^2*q3[i,j]/p3[i,j]*((p3[i,j]-ip[j,3])^2/(1-ip[j,3]^2))  }}
       
	    r = list(x = x, f = f)
	    return(r)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
