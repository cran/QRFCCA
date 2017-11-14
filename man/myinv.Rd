\name{myinv}
\alias{myinv}
\title{
   Generailized inverse
}
\description{
   Calculate the generalized inverse of a matrix
}
\usage{
myinv(A)
}
\arguments{
  \item{A}{
   A is the input data matrix.
}
}
\details{
  Calculate the generalized inverse of input matrix A by using singular value decomposition.
}
\value{
  A matrix which is the generalized inverse of the input matrix.
}

\seealso{
   \code{\link{p_ginv}}
}
\examples{
 X = matrix(rnorm(25,0,1),nrow=5,ncol=5);
 X_inv = myinv(X);
}
