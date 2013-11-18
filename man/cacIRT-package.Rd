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
Version: \tab 1.2\cr
Date: \tab 2011-01-26\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
This packages computes classification accuracy and consistency with two recent approaches proposed by Lee, Hanson & Brennan (2002) and Lee (2010) or by Rudner (2001, 2005), for dichotomous IRT models. The two functions \code{class.Lee} and \code{class.Rud} are the wrapper functions for the respective approaches. They accept a range of inputs: ability estimates, quadrature points, or response data matrix and item parameters. Marginal indices are computed with either the D (given distribution) or P (sample mean) method (see Lee (2010)). The function \code{recursive.raw} computes the probabilities of total scores given ability and item parameters and may be of interest outside of classification.
}
\author{
Quinn Lathrop

Maintainer: <qlathrop @ nd.edu>

}
\references{

Lathrop, Q. N. & Cheng, Y. (2013) Two Approaches to Estimation of Classification Accuracy Rate Under Item Response Theory. Applied Psychological Measurement, \bold{37}, 226--241.
Lee, W. (2010) Classification consistency and accuracy for complex assessments using item response theory. Journal of Educational Measurement, \bold{47}, 1--17.
Lee, W., Hanson, B. A., & Brennan, R. L. (2002) Estimating consistency and accuracy indices for multiple classifications. Applied Psychological Measurement, \bold{26}, 412--432.
Lee, W., & Kolen, M. J. (2008) Irt-class: Irt classification consistency and accuracy (version 2.0). 

Rudner, L. M. (2001) Computing the expected proportions of misclassified examinees. PracticalAssessment, Research & Evaluation, \bold{7(14)}, 1--5. 

Rudner, L. M. (2005) Expected classification accuracy. Practical Assessment Research & Evaluation, \bold{10(13)}, 1--4.

}
\keyword{~Item Response Theory}
\keyword{~IRT}
\keyword{~Classification}


