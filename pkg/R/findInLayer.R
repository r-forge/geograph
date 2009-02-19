###############
## findInLayer
###############
setGeneric("findInLayer", function(x, ...) {
    standardGeneric("findInLayer")
})




################
## for matrices (of long/lat)
################
setMethod("findInLayer", "matrix", function(x, layer="world", attr="all",...){

    ## This functions automatically assigns to land all points overlapping the country polygons
    if(!require(maptools)) stop("maptools package is required.")

    ## Load default shapefile ##
    if(is.character(layer) && layer[1]=="world"){
        if(!require(sp)) stop("sp package needed to map the world")
        data(worldshape)
        layer <- worldshape
    }

    if(!is.null(layer)){
        if(!inherits(layer,"SpatialPolygonsDataFrame"))
            stop("Layer must be a SpatialPolygonsDataFrame object \n(see readShapePoly in maptools to import such data from a GIS shapefile).")
    }

    ## search attr in data ##
    if(attr[1]=="all"){
        selAttr <- 1:ncol(layer@data)
    } else{
        selAttr <- match(attr, colnames(layer@data)) # selected attributes
        if(is.na(selAttr)){ # attribute not found in layer@data
            cat("\nRequested attribute (attr) not found in the layer.\n")
            cat("\nAvailable data are:\n")
            print(head(layer@data))
            return(NULL) # return NULL if attr not found, not generate an error
        }
    }

    ## variables and initialization ##
    long <- unlist(x[,1]) # unlist needed when nrow==1
    lat <- unlist(x[,2])
    n.poly.list <- length(layer@polygons) # number of lists of Polygons obj.
    res <- NULL
    dat <- layer@data
    layerId <- rep(NA, length(long)) # stores the id of matching polygon for each location


    ## main computations ##

    ## browsing elements of @polygons
    ## each is a list with a @Polygons slot
    for(i in 1:n.poly.list) {
        this.poly.list <- layer@polygons[[i]]
        n.polys <- length(this.poly.list@Polygons)
        points.in.this.poly <- rep(0, length(long))

        ## browsing elements of @Polygons
        for (j in 1:n.polys) { ##
            this.poly <- this.poly.list@Polygons[[j]]
            points.in.this.poly <- points.in.this.poly +
                point.in.polygon(long,lat, this.poly@coords[,1], this.poly@coords[,2])

            points.in.this.poly <- as.logical(points.in.this.poly)

            if(any(points.in.this.poly)){
                layerId[points.in.this.poly] <- this.poly.list@ID
            }
        } # end for j
    } # end for i

    res <- dat[layerId, attr, drop=FALSE]
    row.names(res) <- rownames(x)

    return(res)
}) # end findInLayer for matrices






################
## for data.frames (of long/lat)
################
setMethod("findInLayer", "data.frame", function(x, layer="world", attr="all",...){
    x <- as.matrix(x)
    return(findInLayer(x, layer=layer, attr=attr, ...))
}) # end findInLayer






################
## for list (of long/lat)
################
setMethod("findInLayer", "list", function(x, layer="world", attr="all",...){
    x <- data.frame(x)
    return(findInLayer(x, layer=layer, attr=attr, ...))
}) # end findInLayer






##############
## for gGraph # should be carefully used, output is going to be heavy
##############
setMethod("findInLayer", "gGraph", function(x, layer="world", attr="all",...){
    coords <- getCoords(x)
    res <- findInLayer(x=coords, layer=layer, attr=attr, ...)

    if(nrow(x@nodes.attr)>1){
        x@nodes.attr <- cbind.data.frame(x@nodes.attr,res)
    } else {
        x@nodes.attr <- res
    }

    return(x)
}) # end findLand






##############
## for gData
##############
setMethod("findInLayer", "gData", function(x, layer="world", attr="all",...){
    coords <- getCoords(x)
    res <- findInLayer(x=coords, layer=layer, attr=attr, ...)

    if(is.null(x@data)){
        x@data <- res
    }else if(length(nrow(x@data))>0 && nrow(x@data)>1){ # if data are non-empty data.frame
        x@data <- cbind.data.frame(x@data,res)
    } else if(is.list(x@data)){ # if data is a list
        x@data$layerInfo <- res
    } else if(is.vector(x@data)){ # if data is a vector
        x@data <- cbind.data.frame(x@data, res)
    } else{ # else, build a list
        warning("x@data has been transformed into a list to include layer data.")
        x@data <- list(x@data, layerInfo=res)
        return(res)
    }

    return(x)
}) # end findLand

