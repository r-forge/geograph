###################
## plot for gGraph
###################
setMethod("plot", signature("gGraph", y="missing"), function(x, shape="world", psize=NULL, pch=19, col=NULL,
                                      edges=FALSE, reset=FALSE, bg.col="gray", border.col="dark gray",
                                      lwd=1, useWeights=NULL, maxLwd=3, attr.col=NULL,...){
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

    ## handle attr.col (attribute used in color)
    useAttrCol <- ( (!is.null(x@meta$color))  &&
    (nrow(x@meta$color)>0) &&  is.null(col)) # use color from node attribute?

    if(useAttrCol){
        if(is.null(attr.col)){
            attr.col <- colnames(x@meta$color)[1] # default attribute used for colors
        }
    }

    toKeep <- isInArea(x, res.type="integer")
    coords <- coords[toKeep, ]

    ## handle colors -- these are default, not used in some sub-plotting
    if(useAttrCol){
          col <- getColors(x, nodes=toKeep, attr.name=attr.col)
    } else {
        col <- "black"
    } # end handle color


    ## handle shape
    if(!is.null(shape) && shape=="world"){
        if(!require(sp)) stop("sp package needed to map the world")
        data(worldshape)
        shape <- worldshape
    }

    if(!is.null(shape)){ ## plot with background ##
        if(!inherits(shape,"SpatialPolygonsDataFrame"))
            stop("Shape must be a SpatialPolygonsDataFrame object \n(see readShapePoly in maptools to import such data from a GIS shapefile).")

        ## plot background
        plot(shape, col=bg.col, border=border.col, xlim=xlim, ylim=ylim)

        ## subset of points in area
        toKeep <- isInArea(x, reg="usr", res.type="character")
        coords <- getCoords(x)[toKeep, ]

        ## define colors for these points
        if(useAttrCol){
            col <- getColors(x, nodes=toKeep, attr.name=attr.col)
        } else {
            col <- "black"
        } # end handle color

        ## add edges and points
        if(edges){
            plotEdges(x, replot=FALSE, lwd=lwd, useWeights=useWeights, maxLwd=maxLwd)
        }
        points(coords, cex=psize, pch=pch, col=col, ...)

    } else{ ## plot only points ##
        plot(coords, xlab="longitude", ylab="latitude", xlim=xlim, ylim=ylim,
             cex=psize, pch=pch, col=col, ...)
        if(edges){
            plotEdges(x, replot=TRUE, psize=psize, pch=pch, pcol=col, lwd=lwd,
                      useWeights=useWeights, maxLwd=maxLwd)
        }
    }


    ## misc assignement in our dedicated environment
    assign("usr", par("usr"), envir=env)

    curCall <- sys.call(-1)
    assign("last.plot", curCall, envir=env)
    temp <- get("last.plot.param", envir=env)
    temp$psize <- psize
    temp$pch <- pch
    temp$col <- col
    assign("last.plot.param", temp, envir=env)

    return(invisible())
}) # end plot method





#####################
## points for gGraph
#####################
setMethod("points", signature("gGraph"), function(x, psize=NULL, pch=NULL, col=NULL,
                                      edges=FALSE, lwd=1, useWeights=NULL, maxLwd=3,...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## create the .geoGraphEnv if it does not exist
    if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
        assign(".geoGraphEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
        warning(".geoGraphEnv was not present, which may indicate a problem in loading geoGraph.")
    }

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement

    zoomlog <- get("zoom.log", envir=env)
    zoomlog <- zoomlog[1,]

    xlim <- zoomlog[1:2]
    ylim <- zoomlog[3:4]

    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    if(is.null(psize)) psize <- last.plot.param$psize
    if(is.null(pch)) pch <- last.plot.param$pch
    if(is.null(col)) col <- last.plot.param$col

    ## handle zoom and psize
    if(is.null(psize)){
        psize <- get("psize", env=env)
    }

    coords <- getCoords(x)
    toKeep <- isInArea(x)
    coords <- coords[toKeep, , drop=FALSE]

    ## adjust pcol to subset of points in area
    col <- rep(col, length=nrow(x@coords))
    col <- col[toKeep]


    ## add only points and optionally edges
     if(edges){
            plotEdges(x, replot=FALSE, lwd=lwd, useWeights=useWeights, maxLwd=maxLwd)
        }
    points(coords, xlab="longitude", ylab="latitude", xlim=xlim, ylim=ylim,
           cex=psize, pch=pch, col=col, ...)


    ## curCall <- sys.call(-1)
    ## assign("last.plot", curCall, envir=env)

    return(invisible())
}) # end points method






############
## plotEdges
############
plotEdges <- function(x, replot=TRUE, useWeights=NULL, col="black", lwd=1,
                      lty=1, pch=NULL, psize=NULL, pcol=NULL, maxLwd=3, ...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")

    ## handle weights for edges
    if(is.null(useWeights)){
        useWeights <- hasWeights(x)
    }

    ## get the environment
    env <- get(".geoGraphEnv", envir=.GlobalEnv)

    ## retrieve some general plot info
    curUsr <- get("usr", envir=env)

    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    if(is.null(psize)) psize <- last.plot.param$psize
    if(is.null(pch)) pch <- last.plot.param$pch
    ##if(is.null(pcol)) pcol <- last.plot.param$col

    if(is.null(psize)){
        psize <- get("psize", env=env)
    }

    ## retained coords (those within plotting area)
    toKeep <- isInArea(x)
    keptCoords <- getCoords(x)[toKeep, ]

    ## adjust pcol to subset of points in area
    pcol <- rep(pcol, length=nrow(x@coords))
    pcol <- pcol[toKeep]

    edges <- getEdges(x, mode="matNames", unique=TRUE) # retrieve (unique) edges
    temp <- (edges[,1] %in% rownames(keptCoords)) & (edges[,2] %in% rownames(keptCoords))
    keptEdges <- edges[temp, ]

    if(nrow(keptEdges) < 1) {
        cat("\nNo edge to plot.\n")
        return(invisible())
    }

    ## handle weights
    if(useWeights){
        edges.w <- getWeights(x, mode="vector", unique=TRUE)
        edges.w <- edges.w[temp]
        lwd <- edges.w / max(edges.w) # max lwd = 1
        lwd <- lwd * maxLwd # max lwd = maxLwd
        lty <- rep(1, length(lwd)) # make a lty vector
        lty[lwd < 1e-5] <- 3 # assign 3 (doted line) to dead edges.
    }

    ## plot segments
    idx1 <- match(as.character(keptEdges[,1]), rownames(keptCoords))
    idx2 <- match(as.character(keptEdges[,2]), rownames(keptCoords))

    segments(keptCoords[idx1, 1], keptCoords[idx1, 2],
             keptCoords[idx2, 1], keptCoords[idx2, 2], col=col, lwd=lwd, lty=lty, ...)


    ## replot points
    if(replot){

        ##   ## handle colors
        ##         if( (!is.null(x@meta$color)) && nrow(x@meta$color)>0 && is.null(pcol)){
        ##             rules <- x@meta$color
        ##             criterion <- as.list(x@nodes.attr)[[names(rules)[1]]] # seek criterion in nodes.attr
        ##             if(!is.null(criterion)){
        ##                 pcol <- as.character(criterion)[toKeep]
        ##                 for(i in 1:nrow(rules)){
        ##                     pcol[pcol==rules[i,1]] <- rules[i,2]
        ##                 }
        ##             }
        ##         } # end handle color

        ##         if(is.null(pcol)) {
        ##             pcol <- "black"
        ##         }

        points(keptCoords[,1], keptCoords[,2], pch=pch, cex=psize, col=pcol)
    }

    return(invisible())
} # end plotEdges
