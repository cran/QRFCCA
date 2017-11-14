\name{qcca}
\alias{qcca}
\title{
  Quadratically Regularized Canonical Correlation Analysis
}
\description{
  Conduct quadratically regularized canonical correlation analysis.
}
\usage{
qcca(A, A_thres, B, B_thres, Z = NULL)
}
\arguments{
  \item{A}{
    The first input data matrix.
}
  \item{A_thres}{
    The cut-off threshold for the low rank approximation of the first input data matrix.
}
  \item{B}{
   The second input data matrix.
}
  \item{B_thres}{
   The cut-off threshold for the low rank approximation of the second input data matrix.
}
  \item{Z}{
   The potential covariates for the canonical correaltion analysis.  The default value for Z is NULL.
}
}
\value{
The output is a list.
\item{rho}{a numeric vector of canonical correlation coefficients
}
\item{chisq_p}{p_value between 0 and 1 by using chi-square test}
}
\references{
Lin N, Zhu Y, Fan R, Xiong M. A quadratically regularized functional canonical correlation analysis for identifying the global structure of pleiotropy with NGS data. PLOS Computational Biology. 2017;13(10):e1005788. doi: 10.1371/journal.pcbi.1005788.
}
\seealso{
           \code{\link{CCA_chisq_test}},\code{\link{cca}},\code{\link{qcca_p}}
}
\examples{
data(snp_data)
data(phe_data)
\dontrun{
#obtain snp position
sp = as.numeric(colnames(snp_data));
fs = fpca.score(snp_data,pos=sp,gename="Gene",percentage = 0.9,nbasis=45);
rlt = qcca(phe_data,1,fs$score,0.01,Z=NULL);
}
}
