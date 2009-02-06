##############
## getHistory
##############
setGeneric("getHistory", function(x,...) {
    standardGeneric("getHistory")
})


setMethod("getHistory", "gGraph", function(x, ...) {
    res <- x@history
    return(res)
})





##############
## getGraph
##############
setGeneric("getGraph", function(x,...) {
    standardGeneric("getGraph")
})


setMethod("getGraph", "gGraph", function(x, ...) {
    res <- x@graph
    return(res)
})





##############
## getAttr
##############
setGeneric("getAttr", function(x,...) {
    standardGeneric("getAttr")
})


setMethod("getAttr", "gGraph", function(x, ...) {
    res <- x@nodes.attr
    return(res)
})
