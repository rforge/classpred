###
### UTILITY.R
###


##-----------------------------------------------------------------------------
## Split a dataset into training and testing sets, keeping a designated
## factor balanced between the two sets.
##
## fac is the factor with respect to which the pieces should be balanced
## size is a number between 0 and 1, the fraction to be used for training
balancedSplit <- function(fac, size) {
    trainer <- rep(FALSE, length(fac))
    for (lev in levels(fac)) {
        N <- sum(fac==lev)
        wanted <- max(1, trunc(N*size))
        trainer[fac==lev][sample(N, wanted)] <- TRUE
    }
    trainer
}

