###################
## plot for gGraph
###################
setMethod("plot", signature("gGraph", y="missing"), function(x, shape="world",
                                      edges=TRUE, bg.col="gray", border.col="dark gray", ...){
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
    if(!exists("xlim", envir=env)) {
        assign("xlim", range(coords[,1]), envir=env)
    }

    if(!exists("ylim", envir=env)) {
        assign("ylim", range(coords[,2]), envir=env)
    }

    xlim <- get("xlim", envir=env)
    ylim <- get("ylim", envir=env)

    ## retained coords (those within plotting area)
    curUsr <- par("usr")
    coords <- getCoords(x)
    toKeep <- ( (coords[,1] > curUsr[1]) && (coords[,1] < curUsr[2])  # matching longitude
               && (coords[,2] > curUsr[3]) && (coords[,2] < curUsr[4]) ) # matching latitude

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

        ## add points
        points(getCoords(x), ...)

    } else{ # no background
        plot(getCoords(x), xlab="longitude", ylab="latitude", xlim=xlim, ylim=ylim, ...)
    }

    assign("usr", par("usr"), envir=env)

    return(invisible())
}) # end plot method





############
## addEdges
############
addEdges <- function(x,...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")

    ## retained coords (those within plotting area)
    env <- get(".geoGraphEnv", envir=.GlobalEnv)
    curUsr <- get("usr", envir=env)
    coords <- getCoords(x)
    toKeep <- ( (coords[,1] > curUsr[1]) && (coords[,1] < curUsr[2])  # matching longitude
               && (coords[,2] > curUsr[3]) && (coords[,2] < curUsr[4]) ) # matching latitude

    x <- x[toKeep]

    ## make vectors of plotted coords
    




} # end addEdges
