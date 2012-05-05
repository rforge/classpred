###
### FITTEDMODEL.R
###


##=============================================================================
setClass("FittedModel",
         representation(predict="function",
                        trainData="matrix",
                        trainStatus="factor",
                        details="list",
                        extras="list"))


##-----------------------------------------------------------------------------
## Generates a FittedModel object
FittedModel <- function(predict, data, status, details, ...) {
    new("FittedModel",
        predict=predict,
        trainData=data,
        trainStatus=status,
        details=details,
        extras=list(...))
}


##-----------------------------------------------------------------------------
setMethod("predict", signature(object="FittedModel"),
          function(object,
                   newdata=object@trainData,
                   ...) {
    object@predict(newdata=newdata,
                   object@details,
                   object@trainStatus,
                   ...)
})

