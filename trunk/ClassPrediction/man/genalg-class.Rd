\name{genalg-class}
\Rdversion{1.1}
\docType{class}
\alias{genalg-class}
\alias{as.data.frame,genalg-method}
\alias{as.matrix,genalg-method}
\alias{summary,genalg-method}

\title{Class "genalg" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("genalg", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{fitfun}:}{Object of class \code{"function"} ~~ }
    \item{\code{mutfun}:}{Object of class \code{"function"} ~~ }
    \item{\code{p.mutation}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{p.crossover}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{generation}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{fitness}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{best.fit}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{best.individual}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{context}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "genalg")}: ... }
    \item{as.matrix}{\code{signature(x = "genalg")}: ... }
    \item{summary}{\code{signature(object = "genalg")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
	or \code{\linkS4class{CLASSNAME}} for links to other classes
}
\examples{
showClass("genalg")
}
\keyword{classes}
