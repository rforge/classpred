\name{CrossValidate-class}
\alias{CrossValidate-class}
\alias{summary,CrossValidate-method}
\docType{class}
\title{Class "CrossValidate"}
\description{
  A class that contains the results of internal cross-validation (by
  multiple splits into training and test sets) of an algorithm that
  builds a model to predict a binary outcome.
}
\section{Objects from the Class}{
Objects should be created by calls to the constructor function,
\code{\link{CrossValidate}}.
}
\section{Slots}{
  \describe{
    \item{\code{nIterations}:}{An integer; the number of times the
      data was split into training and test sets.}
    \item{\code{trainPercent}:}{A number between 0 and 1; the fraction
      of data used in each training set.}
    \item{\code{outcome}:}{A binary factor containing the true outcome
      for each sample.}
    \item{\code{trainOutcome}:}{A data frame containing the true
      outcomes for each member of the training set.  The value 'NA' is
      used for samples that were reserved for testing. Each column is a
      different split into training and test sets.} 
    \item{\code{trainPredict}:}{A data frame containing the predicted
      outcome from the model for each member of the training set.  The
      value 'NA' is used for samples that were reserved for testing.
      Each column is a different split into training and test sets.}
    \item{\code{validOutcome}:}{A data frame containing the true
      outcomes for each member of the test set.  The
      value 'NA' is used for samples that were used for training.
      Each column is a different split into training and test sets.}
    \item{\code{validPredict}:}{A data frame containing the predicted
      outcome from the model for each member of the test set.  The
      value 'NA' is used for samples that were used for training.
      Each column is a different split into training and test sets.}
    \item{\code{extras}:}{A list, whose length equals the number of
      plsits into trainin and test sets.  Each entry contains any
      "extra" information collected during the fitting of the model; the
      kinds of items stored here depend on the actual classification
      algorithm used.}
  }
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "CrossValidate")}: Produces
      a summary of the performance of the algorithm on both the trinaing
      sets and the test sets, in terms of specificity, sensitivity, and
      positive or negative predictive value.  Specifically, this method
      returns an object of the \code{\link{CrossValSummary-class}}.
    }
  }
}
\references{
  See the manual page for the \code{\link{CrossValidate-package}} for a list
  of related references.
}
\author{
  Kevin R. Coombes <krcoombes@mdanderson.org>
}
\seealso{
  See \code{\link{CrossValidate-package}} for an overview, and see
  \code{\link{CrossValidate}} for the constructor function.
}
\examples{
showClass("CrossValidate")
}
\keyword{classes}

