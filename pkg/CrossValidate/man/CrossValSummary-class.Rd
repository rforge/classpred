\name{CrossValSummary-class}
\Rdversion{1.1}
\docType{class}
\alias{CrossValSummary-class}
\alias{show,CrossValSummary-method}

\title{Class "CrossValSummary"}
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects are almost always created automatically by applying the
\code{summary} method to an object oif the
\code{\link{CrossValidate-class}}.
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"call"} ~~ }
    \item{\code{parent}:}{Object of class \code{"character"} ~~ }
    \item{\code{trainAcc}:}{Object of class \code{"list"} ~~ }
    \item{\code{validAcc}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "CrossValSummary")}: ... }
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
