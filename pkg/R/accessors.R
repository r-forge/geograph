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





#############
## getDates
#############
setGeneric("getDates", function(x, ...) {
    standardGeneric("getDates")
})



setMethod("getDates", "gGraphHistory", function(x, ...) {
    res <- x@dates
    res <- as.POSIXct(res)
    return(res)
})



setMethod("getDates", "gGraph", function(x, ...) {
    res <- getDates(getHistory(x))
    return(res)
})




#############
## getCoords
#############
setGeneric("getCoords", function(x, ...) {
    standardGeneric("getCoords")
})



setMethod("getCoords", "gGraph", function(x, ...) {
    res <- x@coords
    return(res)
})
