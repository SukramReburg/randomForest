\name{combineDS}
\alias{combineDS}

\title{Combine Ensembles of Trees with different predictor set}

\description{
Combine two random forest objects to a joint one. In oposit to the \code{combine(...)} 
function, the \code{combineDS(...)} function allows for the second random forest a 
subset of predictors from the first object. This function is used within the 
optimization algorithm for tree size, and shall outside only be used with caution.
The function is a slight modification of Liaws \code{combine(...)} function.
}

\usage{
combineDS(...)
}

\arguments{
  \item{...}{two objects of class \code{randomForest}, to be
    combined into one.}
}

\value{
An object of class \code{randomForest}.
}

\note{
The \code{confusion}, \code{err.rate}, \code{omega}
components will be carried over.
}

\seealso{\code{\link{randomForest}}, \code{\link{combine}}}

\author{Markus Gruber \email{markus.gruber@tuval.at}}

\examples{
data(iris)
rf1 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf2 <- randomForest(Species ~ Sepal.Length + Petal.Length, 
                    iris, ntree=50, norm.votes=FALSE)
rf.all <- combineDS(rf1, rf2)
print(rf.all)
}

