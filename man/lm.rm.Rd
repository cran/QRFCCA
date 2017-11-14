\name{lm.rm}
\alias{lm.rm}
\title{
   Remove the linear main effect 
}
\description{
   Remove the linear main effect for adjustment
}
\usage{
lm.rm(Y, X)
}
\arguments{
  \item{Y}{
    Y is the data matrix for response variables.  Each row represents the observations of a individual and each column represents a variable.
}
  \item{X}{
    X is the data matrix for adjustment.  Each colum represents a variable for adjustment.i
}
}
\details{
   Remove the main effect by using lienar regression for adjustment of the covariates.
}
\value{
   The update matrix Y with adjustment for the covariates.
}
\examples{
    Y = matrix(rnorm(50,1,2),nrow=10);
    X = matrix(rnorm(20,0,1),nrow=10);
   rlt = lm.rm(Y,X);
}
