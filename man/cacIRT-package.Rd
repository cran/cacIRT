\name{cacIRT-package}
\alias{cacIRT-package}
\alias{cacIRT}
\docType{package}
\title{
Classification accuracy and consistency under Item Response Theory
}
\description{
Computes classification accuracy and consistency under Item Response Theory by the approach proposed by Lee, Hanson & Brennen (2002) and Lee (2010) or the approach proposed by Rudner (2001, 2005).
}
\details{
\tabular{ll}{
Package: \tab cacIRT\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-09-05\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
This packages computes classification accuracy and consistency with two recent approaches proposed by Lee, Hanson & Brennan (2002) and Lee (2010) or by Rudner (2001, 2005). The two functions class.Lee() and class.Rud() are the wrapper functions for the respective approaches. They can accept a range of inputs: ability estimates, quadrature points, or response data matrix and item parameters. Marginal indices are computed with either the D or P method (see Lee (2010)).
}
\author{
Quinn Lathrop

Maintainer: <qlathrop@nd.edu>

}
\references{




Rudner, L. M. (2001) Computing the expected proportions of misclassified examinees. PracticalAssessment, Research & Evaluation, \bold{7(14)}, 1--5. 

Rudner, L. M. (2005) Expected classification accuracy. Practical Assessment Research & Evaluation, \bold{10(13)}, 1--4.

}
\keyword{ package }
\seealso{
}
\examples{
}