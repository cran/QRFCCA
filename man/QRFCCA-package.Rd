\name{QRFCCA-package}
\alias{QRFCCA-package}
\alias{QRFCCA}
\docType{package}
\title{
\packageTitle{QRFCCA}
}
\description{
Comduct quadratically regularized functional canonical correlation analysis (QRFCCA)
}

\details{
The DESCRIPTION file:
\packageDESCRIPTION{QRFCCA}
\packageIndices{QRFCCA}
The QRFCCA can be divided into three layers.  First, we need to calculate the functional representation of the original data, the functional principal component score (FPCS) in our case.  Second, we seek to obtain the low rank approximation of the FPCS.  Then, the canonical correlation analysis is utilized as a tool to study the deep structure between two sets of variables. 
}
\author{
\packageAuthor{QRFCCA}

Maintainer: \packageMaintainer{QRFCCA}
}
\references{
Lin N, Zhu Y, Fan R, Xiong M. A quadratically regularized functional canonical correlation analysis for identifying the global structure of pleiotropy with NGS data. PLOS Computational Biology. 2017;13(10):e1005788. doi: 10.1371/journal.pcbi.1005788.
}
\keyword{ package }
\examples{
#load the library
library(QRFCCA);
#load the snp data set
data(snp_data);
#load the phenotype data set
data(phe_data);
\dontrun{
#obtain the snp location
sp=as.numeric(colnames(snp_data));
#calculate the fpc scores;
fs = fpca.score(snp_data,pos=sp,gename="Gene",percentage = 0.9,nbasis=45);
#quadratically regularized CCA;
rlt = qcca_p(phe_data,0.8,fs$score,0.8,Z=NULL); 
}
}
