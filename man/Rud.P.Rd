\name{Rud.P}
\alias{Rud.P}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rudner's approach with the P methods
}
\description{
Called by class.Rud to compute marginal indices with P method}
\usage{
Rud.P(cutscore, os, sem)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cutscore}{
%%     ~~Describe \code{cutscore} here~~
}
  \item{os}{
%%     ~~Describe \code{os} here~~
}
  \item{sem}{
%%     ~~Describe \code{sem} here~~
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
function(cutscore, os, sem) 
	 { 
	 	nn<-length(os)
	 	
	 	if(nn != length(sem)) stop("Ability and se of different length")
	
		esacc<-matrix(NA,length(cutscore), nn, dimnames = list(paste("cut at",cutscore), round(os,3)))
		escon <-esacc
		for(j in 1:length(cutscore)){
			cuts<-c(-Inf, cutscore[j], Inf)		 	
	 		categ<-cut(os,cuts,labels=F,right=F)
			
		for(i in 1:nn) {
			esacc[j,i]<-(pnorm(cuts[categ[i]+1],os[i],sem[i])-pnorm(cuts[categ[i]],os[i],sem[i]))
			escon[j,i]<-((pnorm(cuts[2], os[i],sem[i]) - pnorm(cuts[1],os[i],sem[i]))^2	+ (pnorm(cuts[3], os[i],sem[i]) - pnorm(cuts[2],os[i],sem[i]))^2	 )
			}}
		
		ans<- (list("Marginal" = cbind("Accuracy" = rowMeans(esacc), "Consistency" = rowMeans(escon)), "Conditional" = list("Accuracy" =t(esacc), "Consistency" = t(escon))))
				
			
			#print(list("Marinal Accuracy" = round(rowMeans(esacc),4), "Marginal Consistency"=round(rowMeans(escon),4)))
 
 ans
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
