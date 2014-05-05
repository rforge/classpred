\name{CrossValidate-class}
\alias{CrossValidate-class}
\alias{summary,CrossValidate-method}
\docType{class}
\title{Class "CrossValidate"}
\description{
  ~~ A concise (1-5 lines) description of what the class is.  ~~
}
\section{Objects from the Class}{
Objects can be created by calls to the constructor function, \code{\link{CrossValidate}}.
}
\section{Slots}{
  \describe{
    \item{\code{nIterations}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{trainPercent}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{outcome}:}{Object of class \code{"factor"} ~~ }
    \item{\code{trainOutcome}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{trainPredict}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{validOutcome}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{validPredict}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{extras}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "CrossValidate")}: ... }
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

