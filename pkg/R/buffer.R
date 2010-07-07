#########
## buffer
#########
setGeneric("buffer", function(x, ...) {
    standardGeneric("buffer")
})



################
## gGraph method
################
setMethod("buffer", "gGraph", buffer <- function(x, nodes, d, ...){
    ## CHECKS ##
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    if(!is.numeric(d)) stop("d is not numeric")
    if(d > 1e4) warning("Buffer distance is greater than 10,000km; computations may be long.")

    ALL.NODES <- getNodes(x)
    if(!all(nodes %in% ALL.NODES)) stop("Some requested nodes do not exist in the gGraph grid.")

    GRAPH <- getGraph(x)
    EDGES <- edges(GRAPH)
    XY <- getCoords(x)


    ## FIND BUFFER FOR A NODE ##
    find.buf.onenode <- function(node, d){
        curNodes <- node
        res <- node
        visited.nodes <- node

        while(TRUE){
            neig <- unlist(EDGES[curNodes])
            neig <- setdiff(neig, visited.nodes)
            visited.nodes <- c(visited.nodes, neig)

            temp <- rdist.earth(XY[node,,drop=FALSE], XY[neig,,drop=FALSE], miles=FALSE, R=NULL)
            toKeep <- temp < d
            if(!any(toKeep)) break # exit
            curNodes <- neig[toKeep]
            res <- c(res, neig[toKeep])
        }
        return(res)
    }


    ## FIND BUFFER FOR ALL REQUESTED NODES ##
    res <- unlist(lapply(nodes,  find.buf.onenode, d))

    ## RETURN RESULTS ##
    res <- unique(res)
    return(res)

}) # end buffer for gGraph





################
## gGraph method
################
setMethod("buffer", "gData", buffer <- function(x, nodes, d, ...){
    ## CHECKS ##
    if(!is.gData(x)) stop("x is not a valid gData object")

}) # end buffer for gData
