\name{mrank}
\alias{mrank}
\title{
   Matrix rank
}
\description{
   Calcualte the rank of a input matrix
}
\usage{
mrank(X)
}
\arguments{
  \item{X}{
      X is the input data matrix.
}
}
\details{
    Calculate the rank of a input matrix by using singular value decompositon.
}
\value{
    The rank of the input matrix.
}

\seealso{
   \code{\link{mrank_sq}}
}
\examples{
   X = matrix(rnorm(25,0,1),nrow=5,ncol=5);
   rlt = mrank(X);
}
