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

setClass("Pruner",
         representation(pruneFunction = "function",
                        paramList = "list"))

Pruner <- function(f, ...) {
  new("Pruner",
      pruneFunction = f,
      paramList=list(...)
      )
}

prune <- function(object, data, group) {
  object@pruneFunction(data, group, object@paramList)
}

keepAll <- Pruner(function(data, group, params) {
  rep(TRUE, length=nrow(data))
})

# feature selection or "pruning" by an F-score
fscore <- function(data, group, params) {
  if (is.null(params$q)) {
    params$q <- 0.9
  }
  q <- params$q
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

makeFPruner <- function(q) Pruner(fscore, q)

# pruning by a t-statistic
tscore <- function(data, group, params) {
  fdr <- params$fdr
  ming <- params$ming
  mtt <- MultiTtest(data, group)
  bum <- Bum(mtt@p.values)
  x <- selectSignificant(bum, alpha=fdr, by="FDR")
  if (sum(x) < ming) {
    p <- sort(mtt@p.values)[ming]
    x <- mtt@p.values <= p
  }
  x
}

makeTPruner <- function(fdr, ming=100) {
  Pruner(tscore, fdr=fdr, ming=min)
}
