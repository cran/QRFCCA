\name{qcca_p}
\alias{qcca_p}
\title{
  Quadratically Regularized Canonical Correlation Analysis with Low Rank Proportion
}
\description{
Conduct quadratically regularized canonical correlation analysis by specifying the proportion of variance that the low rank approximation can explain.
}
\usage{
qcca_p(A, A_prop, B, B_prop, Z = NULL)
}
\arguments{
  \item{A}{
The first input data matrix.
}
  \item{A_prop}{
The proportion of variance that the low rank approximation can explain in matrix A.
}
  \item{B}{
The second input data matrix.
}
  \item{B_prop}{
The proportion of variance that the low rank approximation can explain
in matrix B.
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
\item{A_thres}{The corresponding cut-off point for A_prop}
\item{B_thres}{The corresponding cut-off point for B_prop}
}
\references{
Lin N, Zhu Y, Fan R, Xiong M. A quadratically regularized functional canonical correlation analysis for identifying the global structure of pleiotropy with NGS data. PLOS Computational Biology. 2017;13(10):e1005788. doi: 10.1371/journal.pcbi.1005788.
}

\seealso{
\code{\link{CCA_chisq_test}},\code{\link{cca}},\code{\link{qcca}}
}
\examples{
data(snp_data)
data(phe_data)
\dontrun{
#obtain the snp position
sp = as.numeric(colnames(snp_data));
fs = fpca.score(snp_data,pos=sp,gename="Gene",percentage = 0.9,nbasis=45);
rlt = qcca_p(phe_data,0.8,fs$score,0.8,Z=NULL);
}
}

