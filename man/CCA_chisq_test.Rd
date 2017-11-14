\name{CCA_chisq_test}
\alias{CCA_chisq_test}
\title{
    Chi-square Test for Canonical Correlation Analysis
}
\description{
    Calculate the chi-square test for canonical correlation analysis.
}
\usage{
CCA_chisq_test(rho, n, p, q)
}
\arguments{
  \item{rho}{
     a vector of correlation coefficients.
}
  \item{n}{
     The number of individuals.
}
  \item{p}{
     The number of variables in the first set of variables.
}
  \item{q}{
     The number of variables in the second set of variables.
}
}
\details{
     Calculate the p-value of the chi-square test for canonical correlation analysis.
}
\value{
     p-values between 0 and 1.
}

\seealso{
    \code{\link{cca}}
}
\examples{
     rlt = CCA_chisq_test(c(0.9,0.6,0.02),100,4,3);  
}
