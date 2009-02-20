###################
## dijkstraBetween
###################
setGeneric("dijkstraBetween", function(x,...) {
    standardGeneric("dijkstraBetween")
})






#####################
## method for gGraph
#####################
setMethod("dijkstraBetween", "gGraph", function(x, from, to){

    ## some checks ##
    if(!require(RBGL)) stop("RBGL is required.")
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    if(!all(from %in% getNodes(x))) stop("Some starting nodes are not in x.")
    if(!all(to %in% getNodes(x))) stop("Some ending nodes are not in x.")

    ## build the wrapper ##
    myGraph <- getGraph(x)

    ## build indices of all pairwise combinations ##
    pairIdStart <- integer()
    pairIdStop <- integer()

    for(i in 1:length(x@nodes.id)){
        for(j in i+1:length(x@nodes.id)){
            pairIdStart <- c(pairIdStart, i)
            pairIdStop <- c(pairIdStop, j)
        }
    }


    ## wrap ##
    res <- sp.between(myGraph, start=from, finish=to)

    return(res)
}) # end dijkstraBetween for gGraph






#####################
## method for gData
#####################
setMethod("dijkstraBetween", "gData", function(x){

    ## some checks ##
    if(!require(RBGL)) stop("RBGL is required.")
    if(!is.gData(x)) stop("x is not a valid gData object")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))
    if(length(x@nodes.id)==0) stop("No assigned nodes (x@nodes.id is empty).")

    ## build the wrapper ##
    myGraph <- get(x@gGraph.name, envir=.GlobalEnv)
    myGraph <- getGraph(myGraph)

    ## build indices of all pairwise combinations ##
    pairIdStart <- integer()
    pairIdStop <- integer()

    for(i in 1:length(x@nodes.id)){
        j <- i
        while((j <- j+1) < length(x@nodes.id)){
            pairIdStart <- c(pairIdStart, i)
            pairIdStop <- c(pairIdStop, j)
        }
    }

    ## wrap ##
    res <- sp.between(myGraph, start=x@nodes.id[pairIdStart], finish=x@nodes.id[pairIdStop])

    return(res)
}) # end dijkstraBetween for gData





######################################
######################################




################
## dijkstraFrom
################
setGeneric("dijkstraFrom", function(x,...) {
    standardGeneric("dijkstraFrom")
})





#####################
## method for gGraph
#####################
setMethod("dijkstraFrom", "gGraph", function(x, start, weights="default"){

    ## some checks ##
    if(!require(RBGL)) stop("RBGL is required.")
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    if(!all(start %in% getNodes(x))) stop("Starting node is not in x.")


    ## build the wrapper ##
    myGraph <- getGraph(x)
    if(is.character(weights) && weights=="default"){
        weights <- unlist(edgeWeights(myGraph))
    }

    ## wrap ##
    res <- dijkstra.sp(myGraph, start=start)

    return(res)
}) # end dijkstraFrom for gGraph






####################
## method for gData
####################
setMethod("dijkstraFrom", "gData", function(x, start, weights="default"){

    ## some checks ##
    if(!require(RBGL)) stop("RBGL is required.")
    if(!is.gData(x)) stop("x is not a valid gData object")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))
    if(length(x@nodes.id)==0) stop("No assigned nodes (x@nodes.id is empty).")


    ## build the wrapper ##
    myGraph <- get(x@gGraph.name, envir=.GlobalEnv) # myGraph is a gGraph object
    myGraph <- getGraph(myGraph)

    if(is.character(weights) && weights=="default"){
        weights <- unlist(edgeWeights(myGraph))
    }

    ## wrap ##
    res <- sp.between(myGraph, start=start, finish=x@nodes.id)

}) # end dijkstraFrom for gData
