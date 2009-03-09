#################
## areNeighbours
#################
areNeighbours <- function(V1, V2, graph){
    V1 <- as.character(V1)
    V2 <- as.character(V2)
    if(length(V1) != length(V2)) stop("V1 and V2 have different lengths.")

    edg <- edges(graph)

    ## function testing if two nodes are directly connected
    f1 <- function(A,B){
        return(any(edg[[A]]==B))
    }

    res <- mapply(function(x,y) f1(x,y), V1, V2)

    return(res)
} # end areNeighbours






################
## areConnected
################
areConnected <- function(x, nodes){ # x is a gGraph
    ## some checks ##
    if(!require(RBGL)) stop("RBGL package is required.")
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    if(!all(nodes %in% getNodes(x))) stop("Some specified nodes were not found in the gGraph object.")
    nodes <- unique(nodes)


    ## first check that all our nodes are part of an edge ##
    temp <- unique(as.vector(getEdges(x, mode="matName")))
    nodes.in.edges <- nodes %in% temp
    if(!all(nodes.in.edges)) return(FALSE) # not a connected set if some nodes aren't connected at all


    ## cutting x ##
    temp <- getCoords(x)[nodes,,drop=FALSE] # only nodes in area
    reg <- as.list(as.data.frame(apply(temp,2,range)))


    ## get connected sets ##
    ## !! use connectedComp from RBGL rather than connComp from graph
    ## 100 times faster
    connected.sets <- connectedComp(getGraph(x))

    ## just keep sets > 1 node
    temp <- sapply(connected.sets, length)
    reOrd <- order(temp,decreasing=TRUE) # sets ordered in decreasing size
    temp <- temp[reOrd]
    if(min(temp)==1){
        connected.sets <- connected.sets[reOrd][1:(which.min(temp)-1)]
    }

    names(connected.sets) <- paste("set",1:length(connected.sets))


    f1 <- function(oneSet){ # returns TRUE if our nodes are in a set
        if(length(oneSet) < length(unique(nodes))) return(FALSE)
        res <- all(nodes %in% oneSet)
        return(res)
    }


    ## browse the connected sets ##
    for(e in connected.sets){
        if(f1(e)) return(TRUE)
    }

    return(FALSE)
} # end areConnected






#########################
## isConnected for gData
#########################
## the GENERIC of this method is given in package 'graph'
setMethod("isConnected", "gData", function(object, ...){
    ## checks ##
    x <- object
    if(!is.gData(x)) stop("'object' is not a valid gData object.")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))


    ## set args for areConnected ##
    myGraph <- get(x@gGraph.name, env=.GlobalEnv)
    myNodes <- getNodes(x)

    ## wrapper ##
    res <- areConnected(myGraph, myNodes)

    ## return res ##
    return(res)
}) # end isConnected for gData






#################
## isReachable
#################
isReachable <- function(x, loc){ # x is a gData object
    ## checks ##
    if(!is.gData(x)) stop("x is not a valid gData object.")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))
    mygGraph <- get(x@gGraph.name, envir=.GlobalEnv)


    ## get connected sets ##
    connected.sets <- connectedComp(getGraph(x))


    ## just keep sets > 1 node
    temp <- sapply(connected.sets, length)
    reOrd <- order(temp,decreasing=TRUE) # sets ordered in decreasing size
    temp <- temp[reOrd]
    if(min(temp)==1){
        connected.sets <- connected.sets[reOrd][1:(which.min(temp)-1)]
    }

    names(connected.sets) <- paste("set",1:length(connected.sets))


    ## check which set contains loc ##
    refNode <- closestNode(mygGraph,loc)
    temp <- sapply(connected.sets, function(e) refNode %in% e)
    if(!any(temp)) {
        warning("The reference node is not connected to any node.")
        return(FALSE)
    }

    ## check reachability for each node ##
    myNodes <- getNodes(x)

    f1 <- function(oneNode){ # finds the set in which a node is
        temp <- sapply(connected.sets, function(e) oneNode %in% e)
        return(any(temp))
    }

    res <- sapply(myNodes, f1)
    names(res) <- myNodes

   ## return res ##
    return(res)
} # end isReachable





  f1 <- function(oneNode){ # finds the set in which a node is
        temp <- sapply(connected.sets, function(e) oneNode %in% e)
        if(!any(temp)) return(NA)
        return(which(temp))
    }
