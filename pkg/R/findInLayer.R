###############
## findInLayer
###############
setGeneric("findInLayer", function(x, ...) {
    standardGeneric("findInLayer")
})




################
## for matrices (of long/lat)
################
setMethod("findInLayer", "matrix", function(x, layer="world", attr,...){

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
    selAttr <- match(attr, colnames(layer@data)) # selected attributes
    if(is.na(selAttr)){ # attribute not found in layer@data
        cat("\nRequested attribute (attr) not found in the layer.\n")
        cat("\nAvailable data are:\n")
        print(head(layer@data))
        return(NULL) # return NULL if attr not found, not generate an error
    }

    ## variables and initialization ##
    long <- x[,1]
    lat <- x[,2]
    n.poly <- length(layer@polygons)
    res <- NULL
    dat <- layer@data


    ## main computations ##

    ## browsing elements of @polygons
    ## each is a list with a @Polygons slot
    for(i in 1:n.poly) {
        this.poly.list <- layer@polygons[[i]]
        n.polys <- length(this.poly.list@Polygons)
        points.in.this.poly <- rep(0, length(long))

        ## browsing elements of @Polygons
        for (j in 1:n.polys) { ##
            this.poly <- this.poly.list@Polygons[[j]]
            points.in.this.poly <- points.in.this.poly +
                point.in.polygon(long,lat, this.poly@coords[,1], this.poly@coords[,2])
        }

##        temp <- dat[points.in.this.poly>0]
        res <- rbind.data.frame(res, dat[points.in.this.poly>0, attr, drop=FALSE])
    }

    return(res)
}) # end findInLayer for matrices






################
## for data.frames (of long/lat)
################
setMethod("findInLayer", "gGraph", function(x, layer="world",...){
    x <- as.matrix(x)
    return(findInLayer(x, layer=layer, ...))
}) # end findInLayer







##############
## for gGraph
##############
setMethod("findInLayer", "gGraph", function(x, layer="world", attr.name,...){
    coords <- getCoords(x)
    res <- findLand(coords, layer=layer, ...)
    if(nrow(x@nodes.attr)>1){
        x@nodes.attr <- cbind.data.frame(x@nodes.attr,res)
        names(x@nodes.attr)[ncol(x@nodes.attr)] <- attr.name

    } else {
        x@nodes.attr <- data.frame(res)
        names(res) <- attr.name
    }

    return(x)
}) # end findLand

