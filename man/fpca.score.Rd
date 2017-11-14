\name{fpca.score}
\alias{fpca.score}
\title{
Functional Principal Component Scores
}
\description{
   Calcualate the functional principal component scores
}
\usage{
fpca.score(x, pos = NULL, gename, percentage, nbasis)
}
\arguments{
  \item{x}{
Input data matrix.  Each row represents the observations of a single individual.  Each column represents the variables.
}
  \item{pos}{
 Number of basis function for Fourier expansion and it should be an odd number.
}
  \item{gename}{
 The name of the gene that the snp data belongs.
}
  \item{percentage}{
The propotion of the variance that the functional principal component scores can explain in the functional domain.
}
  \item{nbasis}{
The location or time information for each variables.
}
}
\value{
The output is a list.
\item{score}{The calculated functional principal component scores}
\item{prop}{The proportion of variance that the corresponding principal component scores can explain in the functional domain.}
\item{eigen}{The calculated eigen value when calculating the functional principal component scores.}
}
\references{
Lin N, Zhu Y, Fan R, Xiong M. A quadratically regularized functional canonical correlation analysis for identifying the global structure of pleiotropy with NGS data. PLOS Computational Biology. 2017;13(10):e1005788. doi: 10.1371/journal.pcbi.1005788.
}
\seealso{
  \code{\link{fourier.expansion}},\code{\link{fourier.expansion.smoothed}}
}
\examples{
data(snp_data);
\dontrun{
#obtain the snp position
sp = as.numeric(colnames(snp_data));   
rlt = fpca.score(snp_data,pos=sp,gename="Gene",percentage = 0.9,nbasis=45);
}
}
