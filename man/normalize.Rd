\name{normalize}
\alias{normalize}
\title{
Normalize values}
\description{
Normalizing values. Used as a helper function}
\usage{
normalize(x)
}
\arguments{
  \item{x}{numerical vector}
}
\details{
Normalizes the values withtin the inout vector between 0 and 1}
\value{
Normalized numerical vector.
}
\examples{

## The function is currently defined as
function (x) 
{
    if (length(unique(x)) == 1) {
        return(rep(0.5, length(x)))
    }
    return((x - min(x))/(max(x) - min(x)))
  }
}
