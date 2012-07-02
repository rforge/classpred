# Copyright (C) Kevin R. Coombes, 2007-2012

###
### MODELER.R
###


##=============================================================================
setClass("Modeler",
         representation(learn="function",
                        predict="function",
                        params="list"))


##-----------------------------------------------------------------------------
## Generates a Modeler object
Modeler <- function(learn, predict, ...) {
    new("Modeler",
        learn=learn,
        predict=predict,
        params=list(...))
}


##-----------------------------------------------------------------------------
learn <- function(model, data, status) {
    model@learn(data,
                status,
                model@params,
                model@predict)
}

