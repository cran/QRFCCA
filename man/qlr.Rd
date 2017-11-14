\name{qlr}
\alias{qlr}
\title{
   Quadratically Low Rank Matrix
}
\description{
   Calcualte the quadratically regularized low rank matrix of the input data.
}
\usage{
qlr(X, thres)
}
\arguments{
  \item{X}{
    The input data matrix.  Each row represents the observations from a single individual and each column represents a single variable.
}
  \item{thres}{
    The cut off point for quadratically regularized low rank matrix.
}
}
\details{
    Compute the qudratically regularized low rank approximation of the input matrix.
}
\value{
    The output of the function is a list.
    \item{lr}{
         The low rank approximation of the input data matrix.
}
    \item{rank}{
          The rank of the output low rank matrix.
}
}
\references{
Lin N, Zhu Y, Fan R, Xiong M. A quadratically regularized functional canonical correlation analysis for identifying the global structure of pleiotropy with NGS data. PLOS Computational Biology. 2017;13(10):e1005788. doi: 10.1371/journal.pcbi.1005788.
}
\seealso{
      \code{\link{qcca}}
}
\examples{
     data(phe_data);
     rlt = qlr(phe_data,1);
}
