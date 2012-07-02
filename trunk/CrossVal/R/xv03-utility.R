# Copyright (C) Kevin R. Coombes, 2007-2012

learnPCALR <- function(data, status, params, pfun) {
  require(ClassComparison)
  require(ClassDiscovery)
  if (is.null(params$alpha)) {
    params$alpha <- 0.10
  }
  if (is.null(params$minNGenes)) {
    params$minNGenes <- 10
  }
  if (is.null(params$perVar)) {
    params$perVar <- 0.8
  }
  if (is.null(params$verbose)) {
    params$verbose=TRUE
  }
  if (is.null(params$prior)) {
    params$prior <- 0.5
  }
  # perform two-sample t-tests w.r.t status
  mtt1 <- MultiTtest(data, status)
  bum1 <- Bum(mtt1@p.values)
  # select features that have small p-values
  sel <- selectSignificant(bum1, alpha=params$alpha, by="FDR")
  if(sum(sel) < params$minNGenes) {
    tgt <- sort(mtt1@p.values)[1+params$minNGenes]
    sel <- mtt1@p.values < tgt
  }
  # remember how many genes were used
  nGenesSelected <- sum(sel)
  # compute principal components on training set with selected features
  spca <- SamplePCA(data[sel,])
  # decide how many PCs to use
  NC <- sum(cumsum(spca@variances)/sum(spca@variances) < params$perVar)
  if(NC < 2) NC <- 2
  if(params$verbose) cat(paste("n comps:", NC, "\n"))
  # remember how many components were used
  nCompAvail <- NC
  # assemble training status and PC features into a data frame
  trdata <- data.frame(Stat=status, spca@scores[, 1:NC])
  # fit a logistic prediction model
  # use step-wise AIC to get the "optimal" model
  mmod <- step(glm(Stat ~ ., data=trdata, family=binomial), trace=0)
  nCompUsed <- length(mmod$coefficients)-1 #$
  fm <- FittedModel(pfun, data, status,
                    details=list(prior=params$prior, sel=sel, spca=spca,
                      mmod=mmod, nCompAvail=nCompAvail),
                    nGenesSelected=nGenesSelected,
                    nCompUsed=nCompUsed)
  fm
}

predictPCALR <- function(newdata, details, status, ...) {
  # project the test set into the principal component space so we know
  # the values of the predictors in the test set
  proj <- predict(details$spca, newdata=newdata[details$sel,])
  temp <- data.frame(proj)[, 1:details$nCompAvail]
  preds <- predict(details$mmod, newdata=temp, type='response')
  values <- rep(levels(status)[2], length(preds))
  values[preds < details$prior] <- levels(status)[1]
  factor(values)
} 

learnRPART <- function(data, status, params, pfun) {
  require(rpart)
  tda <- data.frame(status, t(data))
  rp <- rpart(status ~ ., data=tda, method="class", control=params)
  fm <- FittedModel(pfun, data, status,
                    details=list(rp=rp))
}

predictRPART <- function(newdata, details, status, ...) {
  nd <- data.frame(t(newdata))
  predict(details$rp, newdata=nd, type='class')
}


learnLR <- function(data, status, params, pfun) {
  if (is.null(params$prior)) {
    params$prior <- 0.5
  }
  tdata <- data.frame(Stat=status, t(data))
  model <- step(glm(Stat ~ ., data=tdata, family=binomial), trace=0)
  FittedModel(pfun, data, status, details=list(model=model, prior=params$prior))
}

predictLR <- function(newdata, details, status, ...) {
  preds <- predict(details$model, newdata=data.frame(t(newdata)), type='response')
  values <- rep(levels(status)[2], length(preds))
  values[preds < details$prior] <- levels(status)[1]
  values
}

learnSelectedLR <- function(data, status, params, pfun) {
  require(ClassComparison)
  require(ClassDiscovery)
  if (is.null(params$alpha)) {
    params$alpha <- 0.10
  }
  if (is.null(params$minNGenes)) {
    params$minNGenes <- 10
  }
  if (is.null(params$perVar)) {
    params$perVar <- 0.8
  }
  if (is.null(params$verbose)) {
    params$verbose=TRUE
  }
  if (is.null(params$prior)) {
    params$prior <- 0.5
  }
  # perform two-sample t-tests w.r.t status
  mtt1 <- MultiTtest(data, status)
  bum1 <- Bum(mtt1@p.values)
  # select features that have small p-values
  sel <- selectSignificant(bum1, alpha=params$alpha, by="FDR")
  if(sum(sel) < params$minNGenes) {
    tgt <- sort(mtt1@p.values)[1+params$minNGenes]
    sel <- mtt1@p.values < tgt
  }
  # remember how many genes were used
  nGenesSelected <- sum(sel)
  trdata <- data.frame(Stat=status, t(data[sel,]))
  # fit a logistic prediction model
  # use step-wise AIC to get the "optimal" model
  mmod <- step(glm(Stat ~ ., data=trdata, family=binomial), trace=0)
  nFeaturesUsed <- length(mmod$coefficients)-1 #$
  fm <- FittedModel(pfun, data, status,
                    details=list(prior=params$prior, sel=sel,
                      mmod=mmod),
                    nGenesSelected=nGenesSelected,
                    nFeaturesUsed=nFeaturesUsed)
  fm
}

predictSelectedLR <- function(newdata, details, status, ...) {
  # project the test set into the principal component space so we know
  # the values of the predictors in the test set
  temp <- data.frame(t(newdata[details$sel,]))
  preds <- predict(details$mmod, newdata=temp, type='response')
  values <- rep(levels(status)[2], length(preds))
  values[preds < details$prior] <- levels(status)[1]
  factor(values)
} 

learnTailRank <- function(data, status, params, pfun) {
  if (is.null(params$spec)) {
    params$spec <- 0.8
  }
  if (is.null(params$conf)) {
    params$conf <- 0.9
  }  
  require(TailRank)
  tr <- TailRankTest(data, status, specificity=params$spec,
                     confidence=params$conf, direction="two")
  summary(tr)
  
  tdata <- data.frame(Stat=status, t(data))
  model <- step(glm(Stat ~ ., data=tdata, family=binomial), trace=0)
  FittedModel(pfun, data, status, details=list(model=model, prior=params$prior))
}

predictTailRank <- function(newdata, details, status, ...) {
  preds <- predict(details$model, newdata=data.frame(t(newdata)), type='response')
  values <- rep(levels(status)[2], length(preds))
  values[preds < details$prior] <- levels(status)[1]
  values
}

learnKNN <- function(data, status, params, pfun) {
  k <- ifelse(is.null(params$k), 3, params$k)
  fm <- FittedModel(pfun, data, status,
                    details=list(train=data, status=status, k=k))
}

predictKNN <- function(newdata, details, status, ...) {
  require(class)
  data <- details$train
  k <- details$k
  status <- details$status
  knn(t(data), t(newdata), status, k=k)
}

learnCCP <- function(data, status, params, pfun) {
  norm.t <- MultiTtest(data, status) #$
  ccp <- matrix(norm.t@t.statistics, nrow=1) %*% data
  temp <- sapply(levels(status), function(f) {mean(ccp[status==f])})
  cutpoint <- mean(temp)
  FittedModel(pfun, data, status,
              details=list(norm.t=norm.t, ccp=ccp,
                cutpoint=cutpoint, big=which(temp==max(temp))))
}

predictCCP <- function(newdata, details, status, ...) {
  new.ccp <- matrix(details$norm.t@t.statistics, nrow=1) %*% newdata
  big <- details$big # must be 1 or 2
  # implies 3-big = 2 or 1
  pred <- rep(levels(status)[3-big], length(new.ccp))
  pred[new.ccp > details$cutpoint] <- levels(status)[big]
  factor(pred)
}

if (FALSE) {
  kmod <- Modeler(learnPCALR, predictPCALR, minNGenes=10, alpha=0.10, perVar=0.80)
  data <- matrix(rnorm(100*20), ncol=20)
  status <- factor(rep(c("A", "B"), each=10))

  fm <- learn(kmod, data, status)
  predict(fm)

  newdata <- matrix(rnorm(100*30), ncol=30)
  summary(predict(fm, newdata))

  rpartmod <- Modeler(learnRPART, predictRPART, minsplit=2, minbucket=1)
  fm2 <- learn(rpartmod, data, status)
  predict(fm2)
  summary(predict(fm2, newdata))
  
  lrmod <- Modeler(learnLR, predictLR, prior=0.5)
  fm3 <- learn(lrmod, data, status)
  predict(fm3)
  summary(predict(fm3, newdata))
  
  data <- matrix(rnorm(500*100), ncol=100)
  status <- factor(rep(c("A", "B"), each=50))

  xv <- XVal(kmod, data, status, frac=0.6, nLoop=5)
  x <- summary(xv)
  print(x)
}


