\name{CrossValidate}
\alias{CrossValidate}
\title{Creating CrossValidate objects}
\description{
  Given a model classifier and a data set, this function performs
  cross-validation by repeatedly splitting the data into training and
  testing subsets in order to estimate the performance of this kind of
  classifer on new data.
}
\usage{
CrossValidate(model, data, status, frac, nLoop, prune=keepAll, verbose=TRUE)
}
\arguments{
  \item{model}{
An element of the \code{\link{Modeler}} class.
}
  \item{data}{
A matrix containing the data to be used for cross-validation. As with
most gene expression data, columns are the independent samples or
observations and rows are the measured features.
}
  \item{status}{
A binary-valued factor with the classes to be predicted.
}
  \item{frac}{
A number between 0 and 1; the fraction of the data that should be used
in each iteration to train the model. 
}
  \item{nLoop}{
An integer; the number of times to split the data into training and test
sets. 
}
  \item{prune}{
A function  that takes two inoputs, a data matrix and a factor with
two levels, and rteturns a logical vector whose length equals the
number of rows in the data matrix.
}
  \item{verbose}{
A logical value; should the cross-validation routine report interim
progress.
}
}
\details{
  The \code{CrossValidate} package provides generic tools for performing
  cross-validation on classificaiton methods in the context of
  high-throughput data sets such as those produced by gene expression
  microarrays. In order to use a classifier with this implementaiton of
  cross-validation, you must first prepare a pair of functions (one for
  learning models from training data, and one for making predictions on
  test data). These functions, along with any required meta-parameters,
  are used to create an object of the \code{\link{Modeler-class}}. That
  object is then passed to the \code{\link{CrossValidate}} function along
  with the full training data set. The full data set is then repeatedly
  split into its own training and test sets; you can specify the
  fraction to be used for training and the number of iterations. The
  result is a detailed look at the accuracy, sensitivity, specificity,
  and positive and negative predictive value of the model, as estimated
  by cross-validation.
}
\value{
  An object of the \code{\link{CrossValidate-class}}.
}
\references{
  See the manual page for the \code{\link{CrossValidate-class}} for a list
  of related references.
}
\author{
  Kevin R. Coombes \email{krcoombes@mdanderson.org}
}
\seealso{
  See \code{\link{CrossValidate-class}} for a description of the slots in
  the object created by this function.
}
\examples{
dataset <- matrix(rnorm(50*100), nrow=50)
pseudoclass <- factor(rep(c("A", "B"), each=50))
model <- modelerCCP # obviously, other models can be used
numTimes <- 10 # and more is probably better
cv <- CrossValidate(model, dataset, pseudoclass, 0.5, numTimes)
summary(cv)
}
\keyword{ classif }
\keyword{ models }
