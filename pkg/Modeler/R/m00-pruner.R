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

# the keep-everything feature selection method
keepAll <- function(data, group) {
  rep(TRUE, length=nrow(data))
}

# feature selection or "pruning" by a modified Fisher criterion F-score
fsModifiedFisher <- function(q) {
  function(data, group) {
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
}

# pruning by a t-statistic
fsTtest <- function(fdr, ming=500) {
  function(data, group) {
    mtt <- MultiTtest(data, group)
    bum <- Bum(mtt@p.values)
    x <- selectSignificant(bum, alpha=fdr, by="FDR")
    if (sum(x) < ming) {
      p <- sort(mtt@p.values)[ming]
      x <- mtt@p.values <= p
    }
    x
  }
}

