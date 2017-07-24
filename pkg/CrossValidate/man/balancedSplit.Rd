\name{balancedSplit}
\Rdversion{1.1}
\alias{balancedSplit}
\title{
  Split a dataset into training and testing sets, balancing a factor
}
\description{
  When performing cross-validation on a dataset, it often becomes
  necessary to split the data into training and test sets that are
  balanced for a factor.  This function implements such a
  balanced split.
}
\usage{
balancedSplit(fac, size)
}
\arguments{
  \item{fac}{A factor that should be balanced between the two subsets.}
  \item{size}{
    A number between 0 and 1 indicating the fraction of the dataset
    to be used for training.}
}
\details{
This function randomly samples the same fraction of items from each
level of a factor to include in a training set.  In most cases, this
will be a binary factor (and might even be the outcome that one wants to
predict).  However, the implementation works for factors with an
arbitrary number of levels. 
}
\value{
Returns a logical vector with length equal to the length of
\code{fac}. TRUE values designate samples selected for the training
set.
}
\author{
Kevin R. Coombes <krc@silicovore.com>
}
\seealso{
  \code{\link{CrossValidate}}, \code{\link{CrossValidate-class}},
  \code{\link{CrossValidate-package}}.
}
\examples{
nFeatures <- 40
nSamples <- 2*10
dataset <- matrix(rnorm(nSamples*nFeatures), ncol=nSamples)
groups <- factor(rep(c("A", "B"), each=10))
balancedSplit(dataset, groups)
}
\keyword{ classif }
