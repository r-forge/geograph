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





################
## getNodesAttr
################
setGeneric("getNodesAttr", function(x,...) {
    standardGeneric("getNodesAttr")
})


setMethod("getNodesAttr", "gGraph", function(x, nodes=NULL, attr.name=NULL,...) {
    if(is.null(nodes)){ # no node specified -> all nodes kept
        nodes <- TRUE
    }
    if(is.null(attr.name)){ # no attr specified -> all attr kept
        attr.name <- TRUE
    }
    if( (!is.logical(attr.name)) && length(attr.name)==1 ){ # only one attr kept
        res <- x@nodes.attr[,attr.name]
        names(res) <- row.names(x@nodes.attr)
        res <- res[nodes]
    } else { # other cases: a data.frame is returned
        res <- x@nodes.attr[nodes,attr.name]
    }
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





#############
## getNodes
#############
setGeneric("getNodes", function(x, ...) {
    standardGeneric("getNodes")
})



setMethod("getNodes", "gGraph", function(x, ...) {
    res <- rownames(x@coords)
    return(res)
})





#############
## getEdges
#############
setGeneric("getEdges", function(x, ...) {
    standardGeneric("getEdges")
})



setMethod("getEdges", "gGraph", function(x, mode=c("asIs","matNames", "matId"), unique=FALSE, ...) {
    mode <- match.arg(mode)
    if(mode=="asIs") return(x@graph@edgeL)

    if(mode=="matNames"){ # return matrix of node names
        res <- edges(x@graph)
        temp <- sapply(res, length)
        col1 <- rep(names(res), temp)
        ## col1 <- rep(1:length(res), temp)
        col2 <- unlist(res)
        res <- cbind(Vi=col1, Vj=col2)
    }

    if(mode=="matId"){ # return matrix of node numbers
        res <- edgeL(x@graph)
        temp <- sapply(res, function(e) length(e$edges))
        col1 <- rep(1:length(res), temp)
        col2 <- unlist(res)
        res <- cbind(Vi=col1, Vj=col2)
    }

    if(unique){
            toKeep <- res[,1] < res[,2]
            res <- res[toKeep,, drop=FALSE]
        }

    rownames(res) <- NULL
    return(res)
})





#############
## setEdges
#############
setGeneric("setEdges", function(x, ...) {
    standardGeneric("setEdges")
})



setMethod("setEdges", "gGraph", function(x, add=NULL, remove=NULL, weights=NULL, ...) {
    ## some checks
    if(is.null(add) & is.null(remove)) return(x)

    if(!is.null(add)){ ## add edges ##
        add <- as.data.frame(add)
        if(ncol(add) != 2) stop("add does not have two columns")
        from <- as.character(add[[1]])
        to <- as.character(add[[2]])
        if(!all(unique(c(from,to)) %in% getNodes(x))) stop("unknown specified nodes") # unknown nodes
        if(is.null(weights)){
            weights <- rep(1, length(from))
        }

        myGraph <- addEdge(from=from, to=to, graph=x@graph, weights=weights)

    } else { ## remove edges ##
        remove <- as.data.frame(remove)
        if(ncol(remove) != 2) stop("remove does not have two columns")
        from <- as.character(remove[[1]])
        to <- as.character(remove[[2]])
        if(!all(unique(c(from,to)) %in% getNodes(x))) stop("unknown specified nodes") # unknown nodes

        ## avoid attempts to removing non-existing edges
        temp <- areConnected(from, to, x@graph)
        myGraph <- removeEdge(from=from[temp], to=to[temp], graph=x@graph)
    }

    ##  subx <- deparse(substitute(x))
    res <- x
    res@graph <- myGraph

    ## remember this action
    curCall <- match.call()
    newHist <- new("gGraphHistory", res@history, cmd=curCall, comments="Modified edges using setEdges.")
    res@history <- newHist

    ## make assignement
    ## parEnv <- parent.frame()
    ## assign(subx, res, parEnv)

    return(res)
}) # end setEdges
