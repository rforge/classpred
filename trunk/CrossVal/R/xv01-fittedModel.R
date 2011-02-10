setClass("Modeler", representation=list(
                      learn = "function",
                      predict = "function",
                      params = "list"
                      ))

Modeler <- function(learn, predict, ...) {
  new("Modeler", learn = learn, predict = predict, params = list(...))
}

learn <- function(model, data, status) {
  model@learn(data, status, model@params, model@predict)
}

setClass("FittedModel", representation=list(
                          predict = "function",
                          trainData = "matrix",
                          trainStatus = "factor",
                          details = "list",
                          extras = "list"
                          ))

FittedModel <- function(predict, data, status, details, ...) {
  new("FittedModel", predict=predict, trainData=data, trainStatus=status,
      details=details, extras=list(...))
}

setMethod("predict", "FittedModel", function(object, newdata=object@trainData, ...) {
  object@predict(newdata=newdata, object@details, object@trainStatus, ...)
})

