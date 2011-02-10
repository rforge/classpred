\name{GenAlg-class}
\Rdversion{1.1}
\docType{class}
\alias{GenAlg-class}
\alias{as.data.frame,GenAlg-method}
\alias{as.matrix,GenAlg-method}
\alias{summary,GenAlg-method}

\title{Class "GenAlg" }
\description{ Objects of the \code{GenAlg} class represent one step
  (population) in the evolution of a genetic algorithm.  This algorithm
  has been customized to perform feature selection for the class
  prediction problem. }
\section{Objects from the Class}{
  Objects should be created by calls to the function
  \code{\link{GenAlg}};  they will also be created automatically as a
  result of applying the function \code{\link{newGeneration}} to an
  existing \code{GenAlg} object.
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{The initial population of potential solutions,
      in the form of a data matrix with one individual per row.
}
    \item{\code{fitfun}:}{A function to compute the fitness of an
      individual solution.  Must take two input arguments: a vector of
      indices into the rows of
      the population matrix, and a \code{context} list within which any
      other items required by the function can be resolved.  Must return a real
      number; higher values indicate better fitness, with the maximum
      fitness occuring at the optimal solution to the underlying
      numerical problem.} 
    \item{\code{mutfun}:}{A function to mutate individual alleles in the
      population.   Must take two arguments: the starting allele and a
      \code{context} list as in the fitness function.}
    \item{\code{p.mutation}:}{A real value between 0 and 1, representing
      the probability that an individual allele will be mutated.}
    \item{\code{p.crossover}:}{A real value between 0 and 1,
      representing the probability that crossover will occur during
      reproduction.}
    \item{\code{generation}:}{An integer identifying thre current generation.}
    \item{\code{fitness}:}{A numeric vector containing the fitness of
      all individuals in the population.}
    \item{\code{best.fit}:}{A numeric value; the maximum fitness.}
     \item{\code{best.individual}:}{A matrix (often with one row)
       containing the individual(s) achieving the maximum fitness. }
    \item{\code{context}:}{A list of additional data required to perform
      mutation or to compute fitness. This list is passed along as the
      second argument when \code{fitfun} and \code{mutfun} are called.}
  }
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "GenAlg")}: Converts the
      \code{GenAlg} object into a data frame.  The first column contains
      the fitness ; remainign columns contain trhe selected features,
      given as interger indices into the rows of the original data matrix. }
    \item{as.matrix}{\code{signature(x = "GenAlg")}: Converts the GenAlg
    object into a matrix, following the conventions of \code{as.data.frame}.}
    \item{summary}{\code{signature(object = "GenAlg")}: Print a summary
      of the GenAlg object.}
	 }
}
\author{
Kevin R. Coombes <kcoombes@mdanderson.org>
}
\references{
  David Goldberg. "Genetic Algorithms in Search, Optimization and Machine
  Learning," Addison-Wesley, 1989.
}
\seealso{
  See also \code{\link{GenAlg}}, \code{\link{GenAlg-tools}}, and
  \code{\link{maha}}.
}
\examples{
showClass("GenAlg")
}
\keyword{classes}
\keyword{optimize}
\keyword{classif}
