\name{p_ginv}
\alias{p_ginv}
\title{
   Power Inverse	
}
\description{
   Calculate the power inverse of the input matrix
}
\usage{
p_ginv(X, p)
}
\arguments{
  \item{X}{
     The input data matrix.
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
    \code{\link{p_ginv_sq}}
}
\examples{
     X = matrix(rnorm(25,0,1),nrow=5,ncol=5);
     pinv_X = p_ginv(X,2);
}
