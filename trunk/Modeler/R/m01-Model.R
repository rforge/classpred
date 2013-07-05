# Copyright (C) Kevin R. Coombes, 2007-2013

###
### m01-model.R
###

setClass("Modeler",
         representation(learnFunction="function",
                        predictFunction="function",
                        paramList="list"))

## Generates a Modeler object
Modeler <- function(learn, predict, ...) {
    new("Modeler",
        learnFunction=learn,
        predictFunction=predict,
        paramList=list(...))
}

learn <- function(model, data, status) {
    model@learnFunction(data,
                        status,
                        model@paramList,
                        model@predictFunction)
}

