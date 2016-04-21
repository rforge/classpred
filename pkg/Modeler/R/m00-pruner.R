# Copyright (C) Kevin R. Coombes, 2007-2016

###
### m00-model.R
###

# Pruner interface
# INPUT:
#	(training) data
#	(training) class-status
#	optional parameter list
# OUTPUT:
#	logical vector equal in length to number of data rows

keepAll <- function(data, group) {
  rep(TRUE, length=nrow(data))
}

# feature selection or "pruning" by an F-score
fscore <- function(data, group, q) {
  isA <- group == levels(group)[1]
  isB <- !isA
  m  <- matrixMean(data)
  mA <- matrixMean(data[,isA])
  mB <- matrixMean(data[,isB])
  vA <- matrixVar(data[,isA], mA)
  vB <- matrixVar(data[,isB], mB)
  fval <- ((mA - m)^2 + (mB - m)^2)/(vA + vB)
  fval > quantile(fval, q)
}

makeFPruner <- function(q) {
  function(data, group) {
    Modeler:::fscore(data, group, q)
  }
}

# pruning by a t-statistic
tscore <- function(data, group, fdr, ming=500) {
  mtt <- MultiTtest(data, group)
  bum <- Bum(mtt@p.values)
  x <- selectSignificant(bum, alpha=fdr, by="FDR")
  if (sum(x) < ming) {
    p <- sort(mtt@p.values)[ming]
    x <- mtt@p.values <= p
  }
  x
}

makeTPruner <- function(fdr, ming=500) {
  function(data, group) {
    Modeler:::tscore(data, group, fdr, ming)
  }
}
