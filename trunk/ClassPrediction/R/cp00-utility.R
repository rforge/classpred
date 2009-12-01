# This routine computes the Mahalanobis distance between the centers of
# two groups. We sometimes use this as the fitness function in our genetic
# algorithm.
maha <- function(data, groups, method='mve') {
  if (is.logical(groups)) {
    og <- groups
    groups <- rep('B', length(groups))
    groups[og] <- 'A'
  }
  if (!is.factor(groups)) {
    groups <- factor(groups)
  }
  xa <- apply(data[groups==levels(groups)[[1]],], 2, mean)
  xb <- apply(data[groups==levels(groups)[[2]],], 2, mean)
  if (method == 'mve') {
    covar <- cov.mve(data)$cov
  } else {
    covar <- var(data)
  }
  t(xa - xb) %*% solve(covar) %*% (xa - xb)
}

# split a dataset into training and testing sets, keeping a designated
# factor balanced between the two sets.
balancedSplit <- function(fac, size) {
  # fac is the factor with respect to which the pieces should be balanced
  # size is a number between 0 and 1, the fraction to be used for training
  trainer <- rep(FALSE, length(fac))
  for (lev in levels(fac)) {
    N <- sum(fac==lev)
    wanted <- max(1, trunc(N*size))
    trainer[fac==lev][sample(N, wanted)] <- TRUE
  }
  trainer
}

