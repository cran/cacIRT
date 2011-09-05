\name{TOtable.F}
\alias{TOtable.F}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classification table for computing True accuracy or consistency
}
\description{
Creates a 2x2 table (for a single cut score), to compute the proportion of correctly classified, or consistently classified, subjects.  Code is already generalized to simultaneous multiple cut offs}
\usage{
TOtable.F(theta, os, theta.cutoff, os.cutoff)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
a vector of scores, could be True scores, or simply the first test's scores
}
  \item{os}{
a vector of scores with elements corresponding to those given for theta. Could be observed scores, or the second test's scores
}
  \item{theta.cutoff}{
the cut score on the same scale as the scores given in theta
}
  \item{os.cutoff}{
the cut score on the same scale as the scores given in os
}
}
\details{
The cut scores need to be on the same scale as their subject scores, but do not need to match each other. For example, we had one test scored with theta.hat and a theta scaled cutoff, and the second score on total score and a number correct cutoff.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{comp1 }{classification table}
\item{comp2 }{P or proportion of subject on the diagonal}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Quinn Lathrop}
\note{
This is useful in simulation studies when we have unknowable knowledge, either a second test (for consistency), or True scores or Theta (not theta.hat) (for accuracy).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

##with simple 4 item 1PL simulated test:
	## find True classification accuracy of theta.hat and total score
	## use a cut score of theta = 0 (assume true score for total score cut score)

	params<-matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
	theta <-rnorm(100)
	rdm<-sim(params, theta)	
	theta.hat <- MLE(rdm,params)
	total.score <- rowSums(rdm)
	t.cut<-0
	x.cut <- sum(irf(params, t.cut)$f)

TOtable.F(theta, theta.hat, t.cut, t.cut)$P
TOtable.F(theta, total.score, t.cut, x.cut)$P 

# for 1PL model total score is sufficient for theta.hat, repeat with 2PL or 3PL items...
}