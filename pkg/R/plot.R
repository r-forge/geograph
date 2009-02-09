###################
## plot for gGraph
###################
setMethod("plot", signature("gGraph", y="missing"), function(x, shape="world", psize=NULL,
                                      edges=FALSE, reset=FALSE, bg.col="gray", border.col="dark gray", ...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## create the .geoGraphEnv if it does not exist
    if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
        assign(".geoGraphEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
        warning(".geoGraphEnv was not present, which may indicate a problem in loading geoGraph.")
    }

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    coords <- getCoords(x)

    ## handle xlim and ylim
    if((!exists("xlim", envir=env)) | reset) { # if xlim absent or if reset
        assign("xlim", range(coords[,1]), envir=env)
    }

    if((!exists("ylim", envir=env)) | reset) {# if ylim absent or if reset
        assign("ylim", range(coords[,2]), envir=env)
    }

    xlim <- get("xlim", envir=env)
    ylim <- get("ylim", envir=env)

    ## handle zoom and psize
    if(is.null(psize)){
        psize <- get("psize", env=env)
    }

    coords <- getCoords(x)
    toKeep <- ( (coords[,1] >= xlim[1]) & (coords[,1] <= xlim[2])  # matching longitude
               & (coords[,2] >= ylim[1]) & (coords[,2] <= ylim[2]) ) # matching latitude

    coords <- coords[toKeep, ]

    ## handle arguments
    if(shape=="world"){
        if(!require(sp)) stop("sp package needed to map the world")
        data(worldshape)
        shape <- worldshape
    }

    if(!is.null(shape)){ # with background
        if(!inherits(shape,"SpatialPolygonsDataFrame"))
            stop("Shape must be a SpatialPolygonsDataFrame object \n(see readShapePoly in maptools to import such data from a GIS shapefile).")

        ## plot background
        plot(shape, col=bg.col, border=border.col, xlim=xlim, ylim=ylim)

        ## add edges and points
        if(edges){
            plotEdges(x, replot=FALSE)
            points(coords, cex=psize,...)
        } else points(coords, cex=psize,...)

    } else{ # add only points
        plot(coords, xlab="longitude", ylab="latitude", xlim=xlim, ylim=ylim, cex=psize, ...)
    }

    assign("usr", par("usr"), envir=env)

    curCall <- match.call()
    assign("last.plot", curCall, envir=env)

    return(invisible())
}) # end plot method





############
## plotEdges
############
plotEdges <- function(x, replot=TRUE, col="grey", pch=1, psize=NULL,...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")

    ## get the environment
    env <- get(".geoGraphEnv", envir=.GlobalEnv)

    ## retrieve some general plot info
    if(is.null(psize)){
        psize <- get("psize", env=env)
    }

    curUsr <- get("usr", envir=env)


    ## retained coords (those within plotting area)
    coords <- getCoords(x)
    toKeep <- ( (coords[,1] >= curUsr[1]) & (coords[,1] <= curUsr[2])  # matching longitude
               & (coords[,2] >= curUsr[3]) & (coords[,2] <= curUsr[4]) ) # matching latitude

    x <- x[toKeep]
    keptCoords <- getCoords(x)
    keptEdges <- getEdges(x, mode="matrix", unique=TRUE)
    if(nrow(keptEdges) < 1) {
        cat("\nNo edge to plot.\n")
        return(invisible())
    }

    ## plot segments
    segments(keptCoords[keptEdges[,1],1], keptCoords[keptEdges[,1],2],
             keptCoords[keptEdges[,2],1], keptCoords[keptEdges[,2],2], col=col, ...)

    ## replot points
    if(replot){
        points(keptCoords[,1], keptCoords[,2], pch=pch, cex=cex)
    }

    return(invisible())
} # end plotEdges
