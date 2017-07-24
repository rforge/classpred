\name{CrossValSummary-class}
\Rdversion{1.1}
\docType{class}
\alias{CrossValSummary-class}
\alias{show,CrossValSummary-method}

\title{Class "CrossValSummary"}
\description{
  Represents the effect of summarizing a \code{\link{CrossValidate-class}} object
  by computing the performance of predictions on each split into
  training and test sets.
  }
\section{Objects from the Class}{
Objects are almost always created automatically by applying the
\code{summary} method to an object of the
\code{\link{CrossValidate-class}}.
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{A \code{"call"} object recoding how the summary
      method was invoked. }
    \item{\code{parent}:}{A character vector containing the name of the
      \code{CrossValidate} object being summarized.}
    \item{\code{trainAcc}:}{A \code{"list"} containing five numeric
      vectors: the sensitivity, specificity, accuracy, PPV, and NPV on
      each training data set.} 
    \item{\code{validAcc}:}{A \code{"list"} containing five numeric
      vectors: the sensitivity, specificity, accuracy, PPV, and NPV on
      each test data set.} 
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "CrossValSummary")}: Summarizes
    the algorithm performance.}
	 }
}
\author{
  Kevin R. Coombes <krcoombes@mdanderson.org>
}
\seealso{
  \code{\link{CrossValidate-class}}
}
\examples{
showClass("CrossValSummary")
}
\keyword{classes}
