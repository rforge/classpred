\name{CrossValidate}
\alias{CrossValidate}
\title{Creating CrossValidate objects}
\description{
  Given a model classifier and a data set, this function performs
  cross-valiadtion by repeatedly splitting the data into training and
  testing subsets in order to estimate the performance of this kind of
  classifer on new data.
}
\usage{
CrossValidate(model, data, status, frac, nLoop, verbose=TRUE)
}
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{status}{
%%     ~~Describe \code{status} here~~
}
  \item{frac}{
%%     ~~Describe \code{frac} here~~
}
  \item{nLoop}{
%%     ~~Describe \code{nLoop} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
  See the manual page for the \code{\link{CrossValidate-package}} for a list
  of related references.
}
\author{
  Kevin R. Coombes \email{krcoombes@mdanderson.org}
}
\seealso{
  See the manual page for the \code{\link{CrossValidate-package}} for a list
  of classifiers that have been adapted to work with this
  cross-validation mechanism.

  See \code{\link{CrossValidate-class}} for a description of the slots in
  the object created by this function.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(model, data, status, frac, nLoop, verbose=TRUE) {
  if (length(status) != ncol(data)) {
    stop("The length of the status vector must match the size of the data set.")
  }
  
  temp <- balancedSplit(status, frac) # just to compute sizes
  nTrain <- sum(temp)
  nTest <- sum(!temp)

  # allocate space to hold the results
  trainOutcome <- data.frame(matrix(NA, ncol=nLoop, nrow=nTrain))
  validOutcome <- data.frame(matrix(NA, ncol=nLoop, nrow=nTest))
  trainPredict <- data.frame(matrix(NA, ncol=nLoop, nrow=nTrain))
  validPredict <- data.frame(matrix(NA, ncol=nLoop, nrow=nTest))
  extras <- list()
  
  for (i in 1:nLoop) {
    # show that we are still alive
    if(verbose) print(i)
    # split into training and test
    tr <- balancedSplit(status, frac)
    # record the true status for each split so we can get
    # statistics on the performance later
    trainOutcome[,i] <- status[tr]
    validOutcome[,i] <- status[!tr]
    # train the model
    thisModel <- learn(model, data[,tr], status[tr])
    # record anything interesting about the model
    extras[[i]] <- thisModel@extras
    # save the predictions on the training set
    trainPredict[,i] <- predict(thisModel)
    # now make the predictions using the logistic model
    validPredict[,i] <- predict(thisModel, newdata=data[, !tr])
  }
  new("CrossValidate",
      nIterations=nLoop,
      trainPercent=frac,
      outcome=status,
      trainOutcome=trainOutcome,
      validOutcome=validOutcome,
      trainPredict=trainPredict,
      validPredict=validPredict,
      extras=extras)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
