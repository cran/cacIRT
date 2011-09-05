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
vector of ability estimates
}
  \item{ip}{
matrix of item parameters, columns assumed to be discrimination, difficulty, and guessing
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(theta, ip){
	
	ut <- theta
	if (is.null(dim(ip))) 
	        dim(ip) = c(1, 3)
	if(dim(ip)[2]==2)
		ip[,3]<-0        
	ni <- dim(ip)[1]
	nn <- length(ut)
	sc <- ni+1
	
			irf<-function(ip,x){
	   			
	   			if(dim(ip)[2]==2)
						ip[,3]<-0 
				ni = dim(ip)[1]		 
	    		f = (sapply(1:ni, function(i) {
	    			ip[i, 3] + (1 - ip[i, 3])/
	    			(1 + exp(-1.7*ip[i, 1] * (x-ip[i, 2])))}))
	    		r = list(x = x, f = f)
	    			return(r)}
	 	   
		Pjt <-  irf(ip,ut)$f
		Qjt <-  1-Pjt
		H <- apply(Qjt,1,prod)
		Zjt	<- Pjt/Qjt

	zm <- array(0, dim = c(nn, sc, ni))	
		zm[,1,] <- 1
		zm[,2,1] <- Zjt[,1]

	for(i in 2: ni){ # The recursive part, I have it set up as a matrix
			for(s in 2:sc){
				zm[,s,i] <- zm[,s,i-1] + Zjt[,i]*zm[,s-1,i-1]}}	
	
		out<- zm[,,ni]*H
				rownames(out) <- paste("theta = ",round(ut,3))
				colnames(out) <-paste("x = " , 0:ni)
	return(out)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
