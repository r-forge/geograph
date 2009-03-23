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

    ## check connectivity ##
    if(!areConnected(myGraph, unique(c(from,to)))) stop("Not all nodes are connected by the graph.")

    ## recycle from and to
    maxLength <- max(length(from), length(to))
    from <- rep(from, length=maxLength)
    to <- rep(to, length=maxLength)

    ## build indices of all pairwise combinations ##
    pairIdStart <- integer()
    pairIdStop <- integer()

    for(i in 1:maxLength){
        j <- i
        while((j <- j+1) < length(x@nodes.id)){
            pairIdStart <- c(pairIdStart, i)
            pairIdStop <- c(pairIdStop, j)
        }
    }


    ## wrap ##
    res <- sp.between(myGraph, start=from[pairIdStart], finish=to[pairIdStop])

    ## make it a class "gPath" (output + xy coords) ##
    allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
    res$xy <- getCoords(x)[allNodes,]
    class(res) <- "gPath"

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
    if(!isConnected(x)) stop("Not all locations are connected by the graph.")

    ## build the wrapper ##
    myGraph <- get(x@gGraph.name, envir=.GlobalEnv)
    coords <- getCoords(myGraph) # store xy coords for later
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


    ## make it a class "gPath" (output + xy coords) ##
    allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
    res$xy <- coords[allNodes,]
    class(res) <- "gPath"

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
setMethod("dijkstraFrom", "gGraph", function(x, start){

    ## some checks ##
    if(!require(RBGL)) stop("RBGL is required.")
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    if(!all(start %in% getNodes(x))) stop("Starting node is not in x.")

    ## check connectivity ##
    if(!areConnected(myGraph, getNodes(myGraph))) stop("Not all nodes are connected by the graph.")

    ## build the wrapper ##
    myGraph <- getGraph(x)
    ##  if(is.character(costs) && costs=="default"){
    ##         costs <- unlist(edgeWeights(myGraph))
    ##     }

    ## wrap ##
    res <- dijkstra.sp(myGraph, start=start)

    ## sp.between uses unique(x@nodes.id) ##
    ## eventually have to duplicate paths ##
    temp <- gsub(".*:","",names(res))
    res <- res[match(getNodes(x), temp)]


    ## make it a class "gPath" (output + xy coords) ##
    allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
    res$xy <- getCoords(x)[allNodes,]
    class(res) <- "gPath"

    return(res)
}) # end dijkstraFrom for gGraph






####################
## method for gData
####################
setMethod("dijkstraFrom", "gData", function(x, start){

    ## some checks ##
    if(!require(RBGL)) stop("RBGL is required.")
    if(!is.gData(x)) stop("x is not a valid gData object")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))
    if(length(x@nodes.id)==0) stop("No assigned nodes (x@nodes.id is empty).")
    if(!isConnected(x)) stop("Not all locations are connected by the graph")


    ## build the wrapper ##
    myGraph <- get(x@gGraph.name, envir=.GlobalEnv) # myGraph is a gGraph object
    coords <- getCoords(myGraph) # store xy for later
    myGraph <- getGraph(myGraph)

    ##  if(is.character(weights) && weights=="default"){ # no longer used
    ##         weights <- unlist(edgeWeights(myGraph))
    ##     }


    ## wrap ##
    res <- sp.between(myGraph, start=start, finish=x@nodes.id)


    ## sp.between uses unique(x@nodes.id) ##
    ## eventually have to duplicate paths ##
    temp <- gsub(".*:","",names(res))
    res <- res[match(getNodes(x), temp)]


    ## make it a class "gPath" (output + xy coords) ##
    allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
    res$xy <- coords[allNodes,]
    class(res) <- "gPath"

    return(res)
}) # end dijkstraFrom for gData







######################################
######################################






#################
## plot methods
#################
plot.gPath <- function(x, col="rainbow", lwd=3, ...){

    listNodes <- lapply(x[-length(x)], function(e) e$path_detail)
    xy <- x$xy
    Npath <- length(listNodes)

    ## handle color ##
    if(is.character(col) && col[1]=="rainbow"){
        col <- sample(rainbow(length(x)))
    }
    col <- rep(col, length=Npath)
    lwd <- rep(lwd, length=Npath)


    ## function plotting one gPath
    f1 <- function(vecNodes, col, lwd, ...){
        N <- length(vecNodes)
        if(N<2) return() # escape if a path is a single vertice
        from <- vecNodes[1:(N-1)]
        to <- vecNodes[2:N]
        ## segments(xy[from,1], xy[from,2], xy[to,1], xy[to,2], col=col, lwd=lwd, ...)
        geo.segments(xy[from,1], xy[from,2], xy[to,1], xy[to,2], col=col, lwd=lwd, ...)
    }


    ## plot all gPaths
    lapply(1:length(listNodes), function(i) f1(listNodes[[i]], col=col[i], lwd=lwd[i], ...))

    return(invisible())
} # end plot.gPath
