###################
## plot for gGraph
###################
setMethod("plot", signature("gGraph", y="missing"), function(x, shape="world",
                                     bg.col="gray", border.col="dark gray", ...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## create the .geoGraphEnv if it does not exist
    if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
        assign(".geoGraphEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
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

}) # end plot method
