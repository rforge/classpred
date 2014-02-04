# Copyright (C) Kevin R. Coombes, 2007-2013

learnKNN <- function(data, status, params, pfun) {
  k <- ifelse(is.null(params$k), 3, params$k)
  fm <- FittedModel(pfun, data, status,
                    details=list(train=data, status=status, k=k))
}

predictKNN <- function(newdata, details, status, ...) {
  data <- details$train
  k <- details$k
  status <- details$status
  knn(t(data), t(newdata), status, k=k)
}

modeler3NN <- Modeler(learnKNN, predictKNN, params=list(k=3))
modeler5NN <- Modeler(learnKNN, predictKNN, params=list(k=5))
