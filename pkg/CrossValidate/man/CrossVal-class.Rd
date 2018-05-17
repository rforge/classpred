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
  Braga-Neto U, Dougherty ER.\cr
  \emph{Is cross-validation valid for small-sample microarray
  classification?}\cr
  Bioinformatics, 2004; \bold{20}:374--380.

  Jiang W, Varma S, Simon R.\cr
  \emph{Calculating confidence intervals for
  prediction error in microarray classification using resampling.}\cr
  Stat Appl Genet Mol Biol. 2008; \bold{7}:Article8.

  Fu LM, Youn ES.\cr
  \emph{Improving reliability of gene selection from
  microarray functional genomics data.}\cr
  IEEE Trans Inf Technol Biomed. 2003; \bold{7}:191--6.

  Man MZ, Dyson G, Johnson K, Liao B.\cr
  \emph{Evaluating methods for classifying expression data.}\cr
  J Biopharm Stat. 2004; \bold{14}:1065--84.

  Fu WJ, Carroll RJ, Wang S.\cr
  \emph{Estimating misclassification error with small samples via
  bootstrap cross-validation.}\cr
  Bioinformatics, 2005; \bold{21}:1979--86.

  Ancona N, Maglietta R, Piepoli A, D'Addabbo A, Cotugno R, Savino M,
  Liuni S, Carella M, Pesole G, Perri F.\cr
  \emph{On the statistical assessment of classifiers using DNA
  microarray data.}\cr
  BMC Bioinformatics, 2006; \bold{7}:387.

  Lecocke M, Hess K.\cr
  \emph{An empirical study of univariate and genetic
  algorithm-based feature selection in binary classification with
  microarray data.}\cr
  Cancer Inform, 2007; \bold{2}:313--27.

  Lee S.\cr
  \emph{Mistakes in validating the accuracy of a prediction classifier
  in high-dimensional but small-sample microarray data.}\cr
  Stat Methods Med Res, 2008; \bold{17}:635--42.
}
\author{
  Kevin R. Coombes <krcoombes@mdanderson.org>
}
\seealso{
  See \code{\link{CrossValidate}} for the constructor function.
}
\examples{
showClass("CrossValidate")
}
\keyword{classes}

