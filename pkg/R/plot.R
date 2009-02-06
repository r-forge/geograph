###################
## plot for gGraph
###################
setMethod("plot", signature("gGraph", y="missing"), function(x, shape="world",
                                     bg.col="gray", border.col="dark gray", ...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

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
    plot(shape, col=bg.col, border=border.col)

    ## add points
    points(getCoords(x), ...)

    } else{ # no background
        plot(getCoords(x), xlab="longitude", ylab="latitude", ...)
    }

}) # end plot method
