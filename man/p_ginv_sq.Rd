\name{p_ginv_sq}
\alias{p_ginv_sq}
\title{
   Power generalized inverse of a symetric matrix
}
\description{
   Calcualte the pwoer generated inverse of a symetric matrix by using eigen dicompostion.
}
\usage{
p_ginv_sq(X, p)
}
\arguments{
  \item{X}{
    X is the data input matrix and it should be symetrix.
}
  \item{p}{
    p is the power.  For example, to calculate the inverse of X, p is set to be 1.
}
}
\details{
   p_ginv is use to calcuate X^(-p).
}
\value{
   The power inverse of the input matrix.
}

\seealso{
   \code{\link{p_ginv}}
}
\examples{
   X = matrix(rnorm(20,0,1),nrow=5,ncol=4);
   X_in = t(X) %*% X;
   pinv_X_in = p_ginv(X_in,2);
}
