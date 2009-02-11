############
## isInArea
############
isInArea <- function(x, reg="current", buffer=0){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

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

    return(toKeep)


} # end isInArea





################
## areConnected
################
areConnected <- function(V1, V2, graph){
    V1 <- as.character(V1)
    V2 <- as.character(V2)
    if(length(V1) != length(V2)) stop("V1 and V2 have different lengths.")

    edg <- edges(graph)

    ## function testing if two nodes are connected
    f1 <- function(A,B){
        return(any(edg[[A]]==B))
    }

    res <- mapply(function(x,y) f1(x,y), V1, V2)

    return(res)
} # end areConnected
