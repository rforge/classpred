\name{genalg}
\Rdversion{1.1}
\alias{genalg}
\alias{newGeneration}
\title{
  Running genetic algorithm to select features
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
genalg(data, fitfun, mutfun, context, pm = 0.001, pc = 0.5, gen = 1)
newGeneration(ga)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{fitfun}{
%%     ~~Describe \code{fitfun} here~~
}
  \item{mutfun}{
%%     ~~Describe \code{mutfun} here~~
}
  \item{context}{
%%     ~~Describe \code{context} here~~
}
  \item{pm}{
%%     ~~Describe \code{pm} here~~
}
  \item{pc}{
%%     ~~Describe \code{pc} here~~
}
  \item{gen}{
%%     ~~Describe \code{gen} here~~
}
  \item{ga}{
%%     ~~Describe \code{ga} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data, fitfun, mutfun, context, pm=0.001, pc=0.50, gen=1) {
  fitness <- apply(data, 1, fitfun, context=context)
  best.fit <- max(fitness)
  cheat <- (1:length(fitness))[fitness==best.fit][1]
  b1 <- as.matrix(data[cheat,])
  new("genalg",
      data=data, fitfun=fitfun, mutfun=mutfun, p.mutation=pm,
      p.crossover=pc, generation=gen, fitness=fitness,
      best.fit=best.fit, best.individual=b1, context=context)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
