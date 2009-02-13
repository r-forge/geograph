############
## isInArea
############
isInArea <- function(x, reg="current", res.type=c("logical","character"), buffer=0){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    res.type <- match.arg(res.type)

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    coords <- getCoords(x)

    ## get xlim and ylim
    if(exists("zoom.log", envir=env) && reg=="current"){ # xlim/ylim taken from log
        zoomlog <- get("zoom.log", envir=env)
        zoomlog <- zoomlog[1,]

        xlim <- zoomlog[1:2]
        ylim <- zoomlog[3:4]

    } else if(is.list(reg)){ # xlim/ylim user-provided (reg)
        if(length(reg)!=2) stop("reg is not a list of length 2.")
        xlim <- sort(reg[[1]])[1:2]
        ylim <- sort(reg[[2]])[1:2]

    } else return(NA)


    ## main computations ##

    ## handle a buffer around area
    bufferx <- (xlim[2]-xlim[1])*buffer
    buffery <- (ylim[2]-ylim[1])*buffer

    xlim <- xlim + c(-bufferx, bufferx)
    ylim <- ylim + c(-buffery, buffery)

    toKeep <- ( (coords[,1] >= xlim[1]) & (coords[,1] <= xlim[2])  # matching longitude
               & (coords[,2] >= ylim[1]) & (coords[,2] <= ylim[2]) ) # matching latitude

    names(toKeep) <- rownames(coords)

    if(res.type=="logical"){ # return a named vector of logicals
        return(toKeep)
    }

    if(res.type=="character"){ # return names of nodes in the area
        res <- names(toKeep)[toKeep]
        return(res)
    }


} # end isInArea





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





###############
## closestNode
###############
closestNode <- function(x, loc, zoneSize=5, attr.name=NULL, attr.values=NULL){

    ## handle arguments
    if(!require(fields)) stop("package fields is required.")
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")
    loc <- as.data.frame(loc)
    if(ncol(loc) != 2) stop("coords does not have two columns.")
    coords <- getCoords(x)
    nodes <- getNodes(x)

    ## handle attribute specification if provided
    if(!is.null(attr.name)){
        temp <- getNodesAttr(x, attr.name=attr.name)
        temp <- as.character(temp)
        hasRightAttr <- temp %in% attr.values
        if(!any(hasRightAttr)) stop(paste("specified values of",attr.name,"never found."))
    } else{
        hasRightAttr <- TRUE
    }

    ## function finding the closest node for 1 loc ##
    closeOne <- function(oneLoc){
        ## define area around loc
        reg <- list()
        toKeep <- character(0) # will contain node names

        while(length(toKeep) < 3){ # enlarge zoneSize until at least 3 candidates appear
            ## define region
            reg$x <- oneLoc[1] + c(-zoneSize,zoneSize) # +- zoneZine in long
            reg$y <- oneLoc[2] + c(-zoneSize,zoneSize) # +- zoneZine in lat

            ## isolate nodes in this area
            toKeep <- isInArea(x, reg) # ! from now nodes indices won't match those of x and coords

            ## intersect with attribute selection
            toKeep <- toKeep & hasRightAttr

            ## toKeep must be a character to insure matching
            toKeep <- nodes[toKeep]

            ## increment zoneSize
            zoneSize <-  zoneSize*1.5
        } # end while

        xy <- coords[toKeep,,drop=FALSE]

        ## compute all great circle distances between nodes and loc
        temp <- rdist.earth(xy, matrix(oneLoc, nrow=1))
        closeNode <- rownames(temp)[which.min(temp)]
        return(closeNode)
    } # end closeOne


    ## apply closeOne to all requested locations
    res <- apply(loc, 1, closeOne) # these are node labels

    ## must not return indices, as this would not work for subsets of data
    ## e.g. closestPoint[x[getNodesAttr(x)=="land"]] will return wrong indices
    ## temp <- res
    ## res <- match(res, getNodes(x))
    ## names(res) <- temp

    return(res)
} # end closestNode
