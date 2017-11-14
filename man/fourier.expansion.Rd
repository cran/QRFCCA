\name{fourier.expansion}
\alias{fourier.expansion}
\title{
    Fourier Expansion
}
\description{
   Calculate the fourier coefficients 
}
\usage{
fourier.expansion(x, n_of_basis, pos)
}
\arguments{
  \item{x}{
        Input data matrix.  Each row represents the observations of a single individual.  Each column represents the variables.
}
  \item{n_of_basis}{
        Number of basis function for Fourier expansion and it should be an odd number.
}
  \item{pos}{
        The location or time information for each variables.
}
}
\value{
    The output is a list.
    \item{fbasis}{The basis function}
    \item{phi}{The phase of the basis function based on the location or time information}
    \item{coef}{The calculated Fourier Coefficients}
}
\references{
Lin N, Zhu Y, Fan R, Xiong M. A quadratically regularized functional canonical correlation analysis for identifying the global structure of pleiotropy with NGS data. PLOS Computational Biology. 2017;13(10):e1005788. doi: 10.1371/journal.pcbi.1005788.
}
\seealso{
          \code{\link{fourier.expansion.smoothed}}
}
\examples{
    data(snp_data);
    \dontrun{
     rlt = fourier.expansion(snp_data,n_of_basis=37,pos=as.numeric(colnames(snp_data));)
}
}
