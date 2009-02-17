################
## areConnected
################
areConnected <- function(V1, V2, graph){
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
} # end areConnected





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





##############
## hasWeights
##############
hasWeights <- function(x){
    if(length(getGraph(x)@edgeData@data)==0) return(FALSE)
    w <- getWeights(x, mode="vector")
    if(length(unique(w)) < 2) return(FALSE)
    return(TRUE)
}
