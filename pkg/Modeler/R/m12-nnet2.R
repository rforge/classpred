# Copyright (C) Kevin R. Coombes, 2016

learnNNET2 <- function(data, status, params, pfun) {
  f <- as.formula( paste("Stat ~ ",
                         paste(rownames(data), collapse="+")))
  tdata <- data.frame(Stat=status, t(data))
  arglist <- c(list(formula = f, data=tdata),  params)
  model <- do.call(neuralnet, arglist)
  FittedModel(pfun, data, status,
              details=list(model=model))
}
predictNNET2 <- function(newdata, details, status, ...) {
  predict(details$model, t(newdata), ...)
}

modelerNNET2 <- Modeler(learnNNET2, predictNNET2, hidden=5)
