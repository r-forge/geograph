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





#################
## dropDeadEdges
#################
dropDeadEdges <- function(x, thres=1e-10){ # x is a gGraph object
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")
    if(!hasWeights(x)) return(x)

    ## check weights under threshold
    myGraph <- getGraph(x)
    edgeW <- edgeWeights(myGraph)
    edgeL <- edgeL(myGraph)
    toKeep <- lapply(edgeW, function(v) v >= thres)

    newEdgeL <- list()
    for(i in 1:length(edgeL)){
        newEdgeL[[i]] <- list()
        newEdgeL[[i]]$edges <- edgeL[[i]]$edges[toKeep[[i]]]
        newEdgeL[[i]]$weights <- edgeW[[i]][toKeep[[i]]]
    }

    names(newEdgeL) <- nodes(myGraph) # items of the list must be named

    newGraph <- new("graphNEL", nodes=nodes(myGraph), edgeL=newEdgeL)
    res <- x
    res@graph <- newGraph

    return(res)
} # end dropDeadEdges






#################
## dropDeadNodes
#################
dropDeadNodes <- function(x){ # x is a gGraph object
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")

    ## get names of connected nodes
    nodes.in.edges <- unique(as.vector(getEdges(x,mode="matNames")))

    ## get all nodes
    res <- x[nodes.in.edges]

    return(res)
} # end dropDeadNodes






##############
## hasWeights
##############
hasWeights <- function(x){
    if(length(getGraph(x)@edgeData@data)==0) return(FALSE)
    w <- getWeights(x, mode="vector")
    if(length(unique(w)) < 2) return(FALSE)
    return(TRUE)
}






################
## areConnected
################
## the GENERIC of this method is given in package 'graph'
areConnected <- function(x, nodes){
    ## some checks ##
    if(!require(RBGL)) stop("RBGL package is required.")
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    if(!all(nodes %in% getNodes(x))) stop("Some specified nodes were not found in the gGraph object.")
    nodes <- unique(nodes)

    ## call to RBGL ##
    ## THIS HANGS:
    ## res <- sp.between(x, nodes, rep(nodes[1], length(nodes)))

    ## THIS does WORKS not (BUT) IS A KLUDGE:
    ## this algorithm seemingly display weird outputs
    ## either all segments are vertices (from == to)
    ## or at least some are points
    ## ...
    ## or not. Not working.
    ## res <- mstree.prim(x)$edgeList
    ## if(any(res[1,]==res[2,])) return(FALSE)


    ## OTHER APPROACH:
    ## - cut the object to the effective area
    ## - find all connected sets
    ## - search for our nodes inside each
    ## - return TRUE as soon as all are found in a set
    ## - return FALSE otherwise
    ## problem is, most of the time, we are interested into a sub-graph only

    ## first check that all our nodes are part of an edge ##
    temp <- unique(as.vector(getEdges(x, mode="matName")))
    nodes.in.edges <- nodes %in% temp
    if(!all(nodes.in.edges)) return(FALSE) # not a connected set if some nodes aren't connected at all


    ## cutting x ##
    temp <- getCoords(x)[nodes,,drop=FALSE] # only nodes in area
    reg <- as.list(as.data.frame(apply(temp,2,range)))
    ## x <- x[isInArea(x, reg=reg, buffer=0.1)] # too long
    ## x <- dropDeadNodes(x) # only connected nodes #too long


    ## get connected sets ##
    ## !! use connectedComp from RBGL rather than connComp from graph
    ## 100 times faster
    connected.sets <- connectedComp(getGraph(x))

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
