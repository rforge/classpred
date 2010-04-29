\name{gaTourResults}
\Rdversion{1.1}
\docType{data}
\alias{gaTourResults}
\title{Results of a Genetic Algorithm}
\description{
  We ran a genetic algorithm to find the optimal 'fantasy' team for the
  competition run by the Versus broadcasting network for the 2009 Tour
  de France.  In order to make the vignette run in a timely fashion,
  we saved the results in this data object.
}
\usage{data(gaTourResults)}
\format{There are four objects in the data file.  the first is
  \code{recurse}, which is an object of the \code{\link{GenAlg-class}}
  representing the final generation.  The other three objects are all
  numeric vector of length 1100: \code{diversity} contains the average
  population diversity at each generation; \code{fitter} contains the
  maximum fitenss, and \code{meanfit} contains the mean fitness. }
\source{Kevin R. Coombes}
\references{
  Coombes KR. Genetic Algorthims, Feature Selection, and the Tour de
  France. Submitted.
}
\keyword{datasets}
