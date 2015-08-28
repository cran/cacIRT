\name{recursive.raw}
\alias{recursive.raw}
\alias{gen.rec.raw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recursive computation of conditional total score
}
\description{
Recursively computes the probabilities of each possible total score conditional on ability.}
\usage{
recursive.raw(ip, theta, D = 1.7)
gen.rec.raw(Pij, theta.names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{ip}{
Jx3 matrix of item parameters, columns are discrimination, difficulty, and guessing; in that order.
}
  \item{theta}{
Vector of abilities or points to condition on.
}
\item{D}{
The scaling constant for the IRT parameters, defaults to 1.7, alternatively often set to 1.
}
\item{Pij}{Either: 

(1) an NxJ matrix of probabilities of correct response, where each row corresponds to the respective element in \code{theta} and each column represents an item (as in the result of \code{irf()$f})

or

(2) an NxMxJ array of probabilities. Each slice of the array represents an item. Within a slice, each row corresponds to the respective element in \code{theta} and each column represents a response category from 0, 1, ..., M. At a minimum, M=1, in which case the array is Nx2xJ and represents the dichotomous item case. 

}

  \item{theta.names}{
Optional vector to use as row.names in the output matrix. Should correspond to the first dimension of \code{Pij}
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A matrix of theta points by possible total score 0,1, . . . ,J.
}

\author{
Quinn Lathrop
}
\note{
As described in Huynh 1990. 

If the test is mixed format (some dichotomous, some polytomous items), to use \code{gen.rec.raw()}, \code{Pij} must be of an appropriate size for the item with the most response categories. The response categories that do no appear in other items can be filled with zeros. Note also that the function assumes response categories are scored as 0,1,2,3,...,M
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
theta <- c(-1,0, 1)
params<-matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)

#using IRT model and item parameters
rec.mat <- recursive.raw(params, theta)

#using user supplied probability array
Pij.flat <- irf(params, theta)$f

#through matrix input
rec.mat2 <- gen.rec.raw(Pij.flat, theta)

#through array input (this can be generalized to polytomous tests)
Pij.array <- array(NA, dim = c(length(theta), 2, nrow(params)))

Pij.array[,1,] <- 1 - Pij.flat #P(X_j = 0 | theta_i)
Pij.array[,2,] <- Pij.flat     #P(X_j = 1 | theta_i)

rec.mat3 <- gen.rec.raw(Pij.array, theta)

#same results
max(c(rec.mat-rec.mat3, rec.mat2-rec.mat3))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT}

