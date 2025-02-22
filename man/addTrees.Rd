\name{addTrees}
\alias{addTrees}

\title{Add Trees to Random Forest Model}

\description{
This function adds a specified number of trees to a random forest model using a given set of features and target variable.
}

\usage{
addTrees(data, ntree, features, target)
}

\arguments{
  \item{data}{
A data frame containing the dataset used to train the random forest model.
}
  \item{ntree}{
An integer specifying the number of trees to be added to the random forest model.
}
  \item{features}{
A vector of predictor features to be used in the random forest model.
}
  \item{target}{
A string specifying the target variable for the random forest model.
}
}

\details{
This function creates a random forest model with a specified number of trees using the provided dataset, features, and target variable. It formulates the model using the specified features and target, and returns the resulting random forest model.
}

\value{
An object of class \code{randomForest} representing the random forest model with the added trees.
}

\references{
Liaw, A. and Wiener, M. (2002). Classification and Regression by randomForest. R News, 2(3), 18--22.
}

\author{
Markus Gruber \email{markus.gruber@tuval.at}
}

\note{
This function is designed to add trees to an existing random forest model. Ensure that the features and target variable are correctly specified.
}

\seealso{
\code{\link{randomForest}}
}

\examples{
data(iris)
features <- names(iris)[1:4]
target <- "Species"
rf <- addTrees(iris, ntree=100, features, target)
print(rf)
}

\keyword{classification}
