\name{mrank_sq}
\alias{mrank_sq}
\title{
    Matrix rank of symetrix matrix
}
\description{
    Calcualte the matrix rank of a symetric matrix.
}
\usage{
mrank_sq(X)
}
\arguments{
  \item{X}{
    X is the input data matrix and it should be a symetrix matrix.
}
}
\details{
    Calculate the rank of the input data matrix by using eigen decomposition.
}
\value{
    The rank of the symetrix matrix.
}
\seealso{
      \code{\link{mrank}}
}
\examples{
    X = matrix(rnorm(25,0,1),nrow=5,ncol=5);
    X_in = t(X) %*% X;
    rlt = mrank_sq(X_in);
}
