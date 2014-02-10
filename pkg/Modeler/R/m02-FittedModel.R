# Copyright (C) Kevin R. Coombes, 2007-2013

###
### m02-fittedModel.R
###

setClassUnion("numericOrFactor", c("numeric", "factor"))

setClass("FittedModel",
         representation(predictFunction="function",
                        trainData="matrix",
                        trainStatus="numericOrFactor",
                        details="list",
                        extras="list"))

## Generates a FittedModel object
FittedModel <- function(predict, data, status, details, ...) {
#  cat("FittedModel received", class(status), "\n", file=stderr())
    new("FittedModel",
        predictFunction=predict,
        trainData=data,
        trainStatus=status,
        details=details,
        extras=list(...))
}

setMethod("predict", signature(object="FittedModel"),
          function(object,
                   newdata=object@trainData,
                   ...) {
    object@predictFunction(newdata=newdata,
                           object@details,
                           object@trainStatus,
                           ...)
})

