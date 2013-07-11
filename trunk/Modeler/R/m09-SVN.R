# Copyright (C) Kevin R. Coombes, 2007-2013

# Support Vector Machines

learnSVM <- function(data, status, params, pfun) {
  require(e1071)
  tdata <- data.frame(Stat=status, t(data))
  model <- svm(Stat ~ ., data=tdata)
  FittedModel(pfun, data, status,
              details=list(model=model))
}

predictSVM <- function(newdata, details, status, ...) {
  predict(details$model, t(newdata), ...)
}

modelerSVM <- Modeler(learnSVM, predictSVM)
