\name{traverse_tree}
\alias{traverse_tree}
\title{Traverse Tree}
\description{
Helper function for weigthed voting.
}
\usage{
traverse_tree(rf, tree, data_row)
}
\arguments{
  \item{rf}{a \code{randomForest object}}
  \item{tree}{a \code{getTree(randomForest, k) object}}
  \item{data_row}{One row of the data imputed my the random Forest object}
}
\details{
Normalizes the values withtin the inout vector between 0 and 1}
\value{
Prediction of input data with decision Tree.
}
