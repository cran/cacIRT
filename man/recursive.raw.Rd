\name{recursive.raw}
\alias{recursive.raw}
\alias{gen.rec.raw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recursive computation of conditional total score
}
\description{
Returns probabilities of each possible total score conditional on theta}
\usage{
recursive.raw(theta, ip, D = 1.7)
gen.rec.raw(Pij, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Vector of ability estimates or points to condition on.
}
  \item{ip}{
Jx3 matrix of item parameters, columns are discrimination, difficulty, and guessing; in that order.
}
\item{D}{
The scaling constant for the IRT parameters, defaults to 1.7, alternatively often set to 1.
}

\item{Pij}{
An array with N rows, nk (maximum number of options) columns, and I slices giving the probabilites Prob(X_i = x_i | theta_p) Each element is the probability that person p produces that column response for the item i. For binary response the dimensions would be Nx2xI, for a 5 category responses (coded 0,1,2,3,4), it would be Nx5xI). For mixed tests, have the number of columns equal the max number of categories and 0 for any impossible probabilities.
}

}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A matrix of theta points by possible score 0,1, . . . ,J.
}

\author{
Quinn Lathrop
}
\note{
As described in Huynh 1990. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
params <- matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
x <- c(-1,0,1)

rec.mat <- recursive.raw(x, params)

#same results with gen.rec.raw
Pij <- array(NA,dim = c(3,2,4))

Pi1 <- irf(params, x)$f
Pij[,2,] <- Pi1
Pij[,1,] <- 1-Pi1

rec.arr <- gen.rec.raw(Pij,x)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT}

