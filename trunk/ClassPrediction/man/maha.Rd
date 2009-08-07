\name{maha}
\Rdversion{1.1}
\alias{maha}
\title{
Compute the (squared) Mahalanobis distance between two groups of vectors
}
\description{
The Mahalanobis distance between two groups of vectors 
}
\usage{
maha(data, groups, method = "mve")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A matrix with columns representing features (or variables)
    and rows representing independent samples
}
  \item{groups}{
    A factor or logical vector with length equal to the number of rows
    (samples) in the \code{data} matrix
}
  \item{method}{
    A character string determing the method that should be used to
    estimate thecovariance matrix. The default value of "\code{mve}"
    uses the \link[MASS]{cov.mve} function from the MASS package. The
    other valid option is "var", which uses the \code{\link{var}}
    function from the standard \code{stats} package.
}
}
\details{
Stuff should go here
}
\value{
Returns a numeric vector of length 1.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Kevin R. Coombes <kcoombes@mdanderson.org>
}
\note{

}

\seealso{
\code{\link[MASS]{cov.mve}}, \code{\link[stats]{var}}
}
\examples{
nFeatures <- 40
nSamples <- 2*10
dataset <- matrix(rnorm(nSamples*nFeatures), ncol=nSamples)
groups <- factor(rep(c("A", "B"), each=10))
maha(dataset, groups)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
