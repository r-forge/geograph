###################
## plot for gGraph
###################
setMethod("plot", signature("gGraph", y="missing"), function(x, shape="world", psize=NULL, pch=19, col=NULL,
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
    if((!exists("zoom.log", envir=env)) | reset) { # if xlim absent or if reset
        temp <- c(range(coords[,1]), range(coords[,2]))
        .zoomlog.up(temp)
    }

    zoomlog <- get("zoom.log", envir=env)
    zoomlog <- zoomlog[1,]

    xlim <- zoomlog[1:2]
    ylim <- zoomlog[3:4]

    ## handle zoom and psize
    if(is.null(psize)){
        psize <- get("psize", env=env)
    }

    coords <- getCoords(x)
    buffer <- ifelse(edges, 0.1, 0)
    toKeep <- isInArea(x, buffer=buffer)

    coords <- coords[toKeep, ]

    ## handle colors
    if( (!is.null(x@meta$color)) && nrow(x@meta$color)>0 ){
        rules <- x@meta$color
        criterion <- as.list(x@nodes.attr)[[names(rules)[1]]] # seek criterion in nodes.attr
        if(!is.null(criterion)){
            col <- as.character(criterion)
            for(i in 1:nrow(rules)){
                col[col==rules[i,1]] <- rules[i,2]
            }
        }
    } # end handle color
    if(is.null(col)) {
        col <- "black"
    }

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
            points(coords, cex=psize, pch=pch, ...)
        } else points(coords, cex=psize, pch=pch, col=col, ...)

    } else{ # add only points
        plot(coords, xlab="longitude", ylab="latitude", xlim=xlim, ylim=ylim,
             cex=psize, pch=pch, col=col, ...)
    }

    assign("usr", par("usr"), envir=env)

    curCall <- sys.call(-1)
    assign("last.plot", curCall, envir=env)

    return(invisible())
}) # end plot method





############
## plotEdges
############
plotEdges <- function(x, replot=TRUE, col="brown", lwd=1, pch=19, psize=NULL,...){
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
    toKeep <- isInArea(x)
    keptCoords <- getCoords(x)[toKeep, ]

    edges <- getEdges(x, mode="matrix", unique=TRUE)
    temp <- (edges[,1] %in% rownames(keptCoords)) & (edges[,2] %in% rownames(keptCoords))
    keptEdges <- edges[temp, ]

    if(nrow(keptEdges) < 1) {
        cat("\nNo edge to plot.\n")
        return(invisible())
    }

    ## plot segments
    idx1 <- match(as.character(keptEdges[,1]), rownames(keptCoords))
    idx2 <- match(as.character(keptEdges[,2]), rownames(keptCoords))

    segments(keptCoords[idx1, 1], keptCoords[idx1, 2],
             keptCoords[idx2, 1], keptCoords[idx2, 2], col=col, lwd=lwd, ...)

    ## replot points
    if(replot){
        points(keptCoords[,1], keptCoords[,2], pch=pch, cex=psize)
    }

    return(invisible())
} # end plotEdges
