\name{weighted_voting}
\alias{weighted_voting}
\title{Weighted Voting}
\description{
  This function implements a weighted voting approach for combining predictions from a random forest ensemble.
}
\usage{
weighted_voting(rf, dt_in, t = 100, size = 500, weight.target = c(1, 1))
}
\arguments{
  \item{rf}{
    A random forest model object.
  }
  \item{dt_in}{
    The dataset for which predictions are to be made.
  }
  \item{t}{
    The number of top similar instances to be considered for voting.
  }
  \item{size}{
    The size of the sample to be drawn for each class.
  }
  \item{weight.target}{
    The weights for each class in the target variable.
  }
}
\details{
  This function combines predictions from individual trees in a random forest model using a weighted voting approach. It calculates weights for each tree based on its contribution to predicting similar instances in the dataset. Currently only implemented for two class problems.
}
\value{
  A numeric vector of weights assigned to each tree in the random forest model.
}
\seealso{
  \code{\link{predict.randomForest}}
}
\author{
Markus Gruber \email{markus.gruber@tuval.at}
%%  ~~who you are~~
}\examples{
data("iris")
iris <- iris[1:100, ]
iris$Species <- as.character(iris$Species)
iris$Species <- as.factor(iris$Species)
rf_model <- randomForest(Species ~ ., data = iris, keep.inbag = TRUE)
# Predict using weighted voting
weights <- weighted_voting(rf_model, dt_in = iris)
}
