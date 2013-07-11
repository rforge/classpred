# Copyright (C) Kevin R. Coombes, 2007-2013

learnNNET <- function(data, status, params, pfun) {
  require(nnet)
  tdata <- data.frame(Stat=status, t(data))
  model <- nnet(Stat ~ ., data=tdata, size=params$size)
  FittedModel(pfun, data, status,
              details=list(model=model))
}
predictNNET <- function(newdata, details, status, ...) {
  predict(details$model, t(newdata), ...)
}

modelerNNET <- Modeler(learnNNET, predictNNET, size=5)
