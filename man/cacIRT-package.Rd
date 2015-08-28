\name{cacIRT-package}
\alias{cacIRT-package}
\alias{cacIRT}
\docType{package}
\title{
Classification accuracy and consistency under Item Response Theory
}
\description{
Computes classification accuracy and consistency under Item Response Theory by the approach proposed by Lee, Hanson & Brennen (2002) and Lee (2010), the approach proposed by Rudner (2001, 2005), and the approach proposed by Lathrop & Cheng (2014).
}
\details{
\tabular{ll}{
Package: \tab cacIRT\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2015-08-15\cr
License: \tab GPL (>= 2)\cr
}
This packages computes classification accuracy and consistency indices with two approaches proposed by Lee, Hanson & Brennan (2002) and Lee (2010) or by Rudner (2001, 2005). The two functions \code{class.Lee()} and \code{class.Rud()} are the wrapper functions for the most common implementations of the respective approaches. They accept a range of inputs: ability estimates, quadrature points, or response data matrix and item parameters. Marginal indices are computed with either the D (using a theoretical or simulated distribution) or P (using the sample directly) method (see Lee (2010)). The function \code{recursive.raw()} computes the probabilities of total scores given ability and item parameters and may be of interest outside of classification.

The major difference between the Lee approach and the Rudner approach is the scale that the classification occurs on. The Lee approach uses the total score scale, and finds the probability of each total score given an examinee's latent ability estimate and the item parameters. The cut score is also given as a total score. The Rudner approach occurs on the latent trait scale, and is given a cut score on the latent trait scale. Dispite their similarities, the two estimators generally do not estimate the same index, see Lathrop & Cheng (2013) and Lathrop (2015) for discussion and simulation studies.

A new nonparametric approach is also provided with \code{pnr()} and \code{Lee.pnr()}. It is a nonparametric extension to the Lee approach and is explained and tested in Lathrop & Cheng (2014). This approach does not require an assumption of a parametric IRT model or a parametric ability distribution and is often more accurate when those assumptions are violated compared to parametric approaches.

Polytomous tests (where item responses are in more categories than two ordered categories) are easily computed with \code{Lee.pnr()} and \code{class.Rud}. To use Lee's (2010) approach with polytomous or mixed format tests, use \code{Lee.poly.P()}, \code{Lee.poly.D()}, and/or \code{gen.rec.raw()}.
}
\author{
Quinn N. Lathrop

Maintainer: <quinn.lathrop @ gmail.edu>

}
\references{

Lathrop, Q. N., & Cheng, Y. (2013) Two Approaches to Estimation of Classification Accuracy Rate Under Item Response Theory. Applied Psychological Measurement, 37, 226-241.

Lathrop, Q. N., & Cheng, Y. (2014). A Nonparametric Approach to Estimate Classification Accuracy and Consistency. Journal of Educational Measurement, 51(3), 318-334.

Lee, W. (2010) Classification consistency and accuracy for complex assessments using item response theory. Journal of Educational Measurement, 47, 1-17.

Lee, W., Hanson, B. A., & Brennan, R. L. (2002) Estimating consistency and accuracy indices for multiple classifications. Applied Psychological Measurement, 26, 412-432.

Lee, W., & Kolen, M. J. (2008) IRT-class: IRT classification consistency and accuracy (version 2.0). 

Rudner, L. M. (2001) Computing the expected proportions of misclassified examinees. Practical Assessment, Research & Evaluation, 7(14), 1-5. 

Rudner, L. M. (2005) Expected classification accuracy. Practical Assessment Research & Evaluation, 10(13), 1-4.

}
\keyword{~Item Response Theory}
\keyword{~IRT}
\keyword{~Classification}


