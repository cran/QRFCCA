\name{cca}
\alias{cca}
\title{
    Canonical Correlation Analysis
}
\description{
    Conduct the canonical correlation anlaysis between two sets of variables.
}
\usage{
cca(A, B)
}
\arguments{
  \item{A}{
   The input data matrix of the first set of the variables.  Each row of A represents the observations of a single individual.  Each column of A represents the variables.
}
  \item{B}{
   The input data matrix of the second set of the variables.  Each row of B represents the observations of a single individual.  Each column of B represents the variables.
}
}
\value{
    The output is a list.
    \item{rho}{a numeric vector of canonical correlation coefficients
}
    \item{chisq_p}{p_value between 0 and 1 by using chi-square test}
    \item{df}{degree of freedom}
}

\seealso{
           \code{\link{CCA_chisq_test}},\code{\link{qcca}}
}
\examples{
    data(phe_data);
    data(snp_data);
    rlt = cca(phe_data,snp_data);
}
