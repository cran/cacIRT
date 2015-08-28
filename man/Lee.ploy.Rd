\name{Lee.poly}
\alias{Lee.poly.D}
\alias{Lee.poly.P}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

Computes classification accuracy and consistency with Lee's approach for polytomous IRT models.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes classification accuracy and consistency with Lee's approach for polytomous tests. The probability of each possible total score conditional on ability is found with  \code{gen.rec.raw()}. Those probabilities are grouped according to the cut scores and used to estimate the indices.

}
\usage{
Lee.poly.D(cutscore, Pij, quadrature)
Lee.poly.P(cutscore, Pij, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cutscore}{
%%     ~~Describe \code{cutscore} here~~
A scalar or vector of cut scores on the True Score scale. If you have cut scores on the theta scale, you can transform them with \code{irf} (See example for \code{irf}). Should not include 0 or the max total score, as the end points are added internally.
}
\item{Pij}{An NxMxJ array of probabilities. Each slice of the array represents an item. Within a slice, each row corresponds to the respective element in \code{theta} and each column represents a response category from 0, 1, ..., M. At a minimum, M=1, in which case the array is Nx2xJ and represents the dichotomous item case. 

}
  \item{theta}{
%%     ~~Describe \code{ability} here~~
Ability estimates for each subject. Must correspond to the first dimension of \code{Pij}.
}
  \item{quadrature}{
%%     ~~Describe \code{quadrature} here~~
A list containing 1) The quadrature points and 2) Their corresponding weights. Must correspond to the first dimension of \code{Pij}.
}

 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The polytomous generalization to \code{class.Lee}. Requires the user build the \code{Pij} array.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Marginal}{A matrix with two columns of marginal accuracy and consistency per cut score (and simultaneous if multiple cutscores are given)}
\item{Conditional}{A matrix of conditional accuracy and conditional consistency}
%% ...
}

\references{
Lee, W. (2010) Classification consistency and accuracy for complex assessments using item response theory. Journal of Educational Measurement, 47, 1--17.
}

\author{
Quinn N. Lathrop}
\note{
In order to score above a cut, an examinee must score at or above the cut score. Since we are working on the total score scale, be aware that if a cut score is given with a decimal (like 2.4), the examinee must have a total score at the next integer or more (so 3 or more) to score above the cut.

If the test is mixed format (some dichotomous, some polytomous items), \code{Pij} must be of an appropriate size for the item with the most response categories. The response categories that do no appear in other items can be filled with zeros. Note also that the function assumes response categories are scored as 0,1,2,3,...,M
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\note{While this function is needed for polytomous tests for the Lee approach, \code{class.Rud()} works directly with polytomous tests when given the ability estimate and the standard error and so does not need an analogous set of functions.}

\examples{
#Same example as \code{class.Lee()}, 
  #build \code{Pij} the same as in the example for \code{gen.rec.raw()}.

params <- matrix(c(1,1,1,1,-2,1,0,1,0,0,0,0),4,3)
theta <- rnorm(100)

Pij.flat <- irf(params, theta)$f
Pij.array <- array(NA, dim = c(length(theta), 2, nrow(params)))
Pij.array[,1,] <- 1 - Pij.flat #P(X_j = 0 | theta_i)
Pij.array[,2,] <- Pij.flat     #P(X_j = 1 | theta_i)

Lee.poly.P(2, Pij.array, theta)$Marginal

#in the dichotomous case, identical to \code{Lee.P()}
Lee.P(2, params, theta)$Marginal

#For Rudner and polytomous tests, compute the theta estimate and se and use those as input
theta.est <- theta 
#just for example

theta.se <- SEM(params, theta.est) 
#also for example, SEM() assumes 3PL model, 
#but if you use mirt or similar package, 
#the theta estimates and their se will be available

Rud.P(.5, theta.est, theta.se)$Marginal

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~IRT }

