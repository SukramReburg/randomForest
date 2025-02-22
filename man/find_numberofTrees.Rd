\name{find_numberofTrees}
\alias{find_numberofTrees}

\title{Determine Optimal Number of Trees in Random Forest}

\description{
This function calculates the optimal number of trees for a random forest model 
based on the importance of predictors and error rates. It can also reconstruct 
the model with the determined optimal number of trees.
}

\usage{
find_numberofTrees(rf, data,
                   features.next = NULL,
                   imp_f.next = NULL,
                   unimp_f.next = NULL,
                   reconstruct_all)
}

\arguments{
  \item{rf}{
A random forest model object of class \code{randomForest}.
}
  \item{data}{
A data frame containing the dataset used to train the random forest model.
}
  \item{features.next}{
An optional vector of predictor features for the next iteration. Default is \code{NULL}.
}
  \item{imp_f.next}{
An optional vector of important features for the next iteration. Default is \code{NULL}.
}
  \item{unimp_f.next}{
An optional vector of unimportant features for the next iteration. Default is \code{NULL}.
}
  \item{reconstruct_all}{
A logical value indicating whether to reconstruct the entire model with the new number of trees. Default is \code{FALSE}.
}
}

\details{
This function uses a combination of predictor importance and error rates from a 
random forest model to determine the optimal number of trees. It calculates weights 
for predictors, identifies important and unimportant features, and computes the number 
of additional trees needed. Optionally, it can reconstruct the entire model with the 
determined number of trees.
}

\value{
A list with the following components:
\item{number of trees }{The optimal number of trees for the random forest model.}
\item{features }{The updated vector of predictor features.}
\item{target }{The target variable.}
\item{important }{The vector of important features for the next iteration.}
\item{unimportant }{The vector of unimportant features for the next iteration.}
}

\references{
Liaw, A. and Wiener, M. (2002). Classification and Regression by randomForest. R News, 2(3), 18--22.
}

\author{
Markus Gruber \email{markus.gruber@tuval.at}
}

\note{
Use this function with caution as it modifies the structure of the random forest model.
}

\seealso{
\code{\link{randomForest}}, \code{\link{combine}}
}

\examples{
data(iris)
rf <- randomForest(Species ~ ., data=iris, ntree=50, isSizeopt = TRUE)
result <- find_numberofTrees(rf, iris, reconstruct_all = FALSE)
print(result)
}

\keyword{classification}
