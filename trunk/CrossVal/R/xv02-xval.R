setClass("CrossVal", representation=list(
                   nIterations="numeric",
                   trainPercent="numeric",
                   outcome="factor",
                   trainOutcome="data.frame",
                   trainPredict="data.frame",
                   validOutcome="data.frame",
                   validPredict="data.frame",
                   extras="list"
                   ))

CrossVal <- function(model, data, status, frac, nLoop, verbose=TRUE) {
  if (length(status) != ncol(data)) {
    stop("The length of the status vector must match the size of the data set.")
  }
  
  temp <- balancedSplit(status, frac) # just to compute sizes
  nTrain <- sum(temp)
  nTest <- sum(!temp)

  # allocate space to hold the results
  trainOutcome <- data.frame(matrix(NA, ncol=nLoop, nrow=nTrain))
  validOutcome <- data.frame(matrix(NA, ncol=nLoop, nrow=nTest))
  trainPredict <- data.frame(matrix(NA, ncol=nLoop, nrow=nTrain))
  validPredict <- data.frame(matrix(NA, ncol=nLoop, nrow=nTest))
  extras <- list()
  
  for (i in 1:nLoop) {
    # show that we are still alive
    if(verbose) print(i)
    # split into training and test
    tr <- balancedSplit(status, frac)
    # record the true status for each split so we can get
    # statistics on the performance later
    trainOutcome[,i] <- status[tr]
    validOutcome[,i] <- status[!tr]
    # train the model
    thisModel <- learn(model, data[,tr], status[tr])
    # record anything interesting about the model
    extras[[i]] <- thisModel@extras
    # save the predictions on the training set
    trainPredict[,i] <- predict(thisModel)
    # now make the predictions using the logistic model
    validPredict[,i] <- predict(thisModel, newdata=data[, !tr])
  }
  new("CrossVal",
      nIterations=nLoop,
      trainPercent=frac,
      outcome=status,
      trainOutcome=trainOutcome,
      validOutcome=validOutcome,
      trainPredict=trainPredict,
      validPredict=validPredict,
      extras=extras)
}

setClass("CrossValSummary",
         representation=list(
           call="call",
           parent="character",
           trainAcc="list",
           validAcc="list"
           ))

setMethod("summary", "CrossVal", function(object, ...) {
  oneset <- function(outc, pred, N) {
    sens <- spec <- acc <- ppv <- npv <- rep(NA, N)
    for (i in 1:N) {
      o <- outc[,i]
      p <- factor(pred[,i], levels=levels(o))
      tab <- table(p, o)
      sens[i] <- tab[2,2]/sum(tab[,2])
      spec[i] <- tab[1,1]/sum(tab[,1])
      acc[i] <- (tab[1,1]+tab[2,2])/sum(tab)
      ppv[i] <- tab[2,2]/sum(tab[2,])
      npv[i] <- tab[1,1]/sum(tab[1,])
    } 
    list(sens=sens, spec=spec, acc=acc, ppv=ppv, npv=npv)
  }
  trainAcc <- oneset(object@trainOutcome, object@trainPredict, object@nIterations)
  validAcc <- oneset(object@validOutcome, object@validPredict, object@nIterations)
  new("CrossValSummary",
      call=match.call(),
      parent=deparse(substitute(object)),
      trainAcc=trainAcc, validAcc=validAcc)
})


setMethod("show", "CrossValSummary", function(object) {
  p <- eval(as.name(object@parent))
  temp <- paste("Cross-validation was performed using",
                round(100*p@trainPercent, digits=1),
                "percent of the data for training.",
                "The data set was randomly split into training",
                "and testing sets", p@nIterations, "times.")
  cat("---------------\n")
  writeLines(strwrap(temp))
  cat("\nTraining Accuracy:\n")
  print(sapply(object@trainAcc, summary))
  cat("\nValidation Accuracy:\n")
  print(sapply(object@validAcc, summary))

  e <- p@extras
  if (length(e) > 0) {
    who <- names(e[[1]])
    for (extra in who) {
      temp <- unlist(lapply(e, function(x) x[[extra]]))
      cat(paste("\n", extra, ":\n", sep=""))
      print(summary(temp))
    }
  }
})

