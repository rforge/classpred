###
### ALLGENERICS.R
###


if (!isGeneric("as.data.frame")) {
    setGeneric("as.data.frame",
               function(x, row.names=NULL, optional=FALSE, ...) 
                   standardGeneric("as.data.frame"))
}

if (!isGeneric("as.matrix")) {
    setGeneric("as.matrix",
               function(x, ...) standardGeneric("as.matrix"))
}

if (!isGeneric("summary")) {
    setGeneric("summary",
               function(object, ...) standardGeneric("summary"))
}

