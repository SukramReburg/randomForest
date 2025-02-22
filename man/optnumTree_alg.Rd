\name{optnumTree_alg}
\alias{optnumTree_alg}

\title{Optimize Number of Trees in Random Forest Algorithm}

\description{
This function iteratively optimizes the number of trees in a random forest model by adding trees based on feature importance and other criteria.
}

\usage{
optnumTree_alg(rf_start, data, reconstruct_all = T)
}

\arguments{
  \item{rf_start}{
An initial random forest model object of class \code{randomForest}.
}
  \item{data}{
A data frame containing the dataset used to train the random forest model.
}
  \item{reconstruct_all}{
A logical value indicating whether to reconstruct the entire forest (default is \code{TRUE}). If \code{FALSE}, only the additional trees are added to the existing forest.
}
}

\details{
The function starts with an initial random forest model and iteratively adds trees based on feature importance, optimizing the model. If \code{reconstruct_all} is \code{TRUE}, the entire forest is reconstructed; otherwise, only the specified number of trees are added. The process continues until the number of unimportant features is less than the square root of the total number of features, or a maximum of 20 iterations is reached.
}

\value{
An optimized random forest model object of class \code{randomForest}.
}

\references{
Liaw, A. and Wiener, M. (2002). Classification and Regression by randomForest. R News, 2(3), 18--22.
}

\author{
Markus Gruber \email{markus.gruber@tuval.at}
}

\note{
This function is designed to optimize the number of trees in a random forest model. It may abort the addition of trees if the number of trees exceeds 600 to avoid long computation times.
}

\seealso{
\code{\link{randomForest}}, \code{\link{find_numberofTrees}}, \code{\link{addTrees}}
}

\examples{
data(iris)
rf_start <- randomForest(Species ~ ., data=iris, ntree=100, isSizeopt = TRUE)
optimized_rf <- optnumTree_alg(rf_start, data=iris, reconstruct_all=TRUE)
print(optimized_rf)
}

\keyword{classification}
