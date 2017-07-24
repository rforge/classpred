\name{CrossValidate-package}
\alias{CrossValidate-package}
\docType{package}
\title{Cross validation of high-throughput prediction algorithms}
\description{
  The \code{CrossValidate} package provides generic tools for performing
  cross-validation on classification methods in the context of high-throughput
  data sets such as those produced by gene expression profiling. In order to
  use a classifier with this implementation of cross-validation, you must first
  prepare a pair of functions (one for learning models from training data, and
  one for making predictions on test data). These functions, along with any
  required meta-parameters, are used to create an object of the
  \code{\link{Modeler-class}}. That object is then passed to the
  \code{\link{CrossValidate}} function along with the full training data set. The
  full data set is then repeatedly split into its own training and test sets;
  you can specify the fraction to be used for training and the number of
  iterations. The result is a detailed look at the accuracy, sensitivity,
  specificity, and positive and negative predictive value of the model, as
  estimated by cross-validation.
}
\details{
  \tabular{ll}{
    Package: \tab CrossValidate\cr
    Type: \tab Package\cr
    Version: \tab 1.0.1\cr
    Date: \tab 2012-05-04\cr
    License: \tab Artistic-2.0\cr
    LazyLoad: \tab yes\cr
  }
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com}
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
\seealso{
  The \code{\link{Modeler-package}} contains numerous classification
  methods that have been adapted to work within this general
  cross-validation framework, including: K nearest neighbors 
  (\code{\link{learnKNN}}), recursive partitioning and regression trees
  (\code{\link{learnRPART}}), random forests (\code{\link{learnRF}})
  neural networks (\code{\link{learnNNET}}),   support vector machines
  (\code{\link{learnSVM}}), compound covariate predictors
  (\code{\link{learnCCP}}), and the TailRank test
  (\code{\link{learnTailRank}}).
}
\keyword{package}
\keyword{classif}
\keyword{multivariate}

