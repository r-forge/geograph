###############
## findCountry
###############
setGeneric("findCountry", function(x,...) {
    standardGeneric("findCountry")
})




################
## for matrices (of long/lat)
################
setMethod("findCountry", "matrix", function(x, shape="world",...){

    ## This functions automatically assigns to land all points overlapping the country polygons
    if(!require(maptools)) stop("maptools package is required.")

    ## Load country shapefile
    if(shape=="world"){
        if(!require(sp)) stop("sp package needed to map the world")
        data(worldshape)
        shape <- worldshape
    }

    if(!is.null(shape)){ # with background
        if(!inherits(shape,"SpatialPolygonsDataFrame"))
            stop("Shape must be a SpatialPolygonsDataFrame object \n(see readShapePoly in maptools to import such data from a GIS shapefile).")
    }

    long <- x[,1]
    lat <- x[,2]
    n.country <- length(shape@polygons)

    for(i in 1:n.country) {
        this.country <- shape@polygons[i][[1]]
        n.polys <- length(this.country@Polygons)
        points.in.this.country <- rep(0, length(long))

        for (p in 1:n.polys) {
            this.poly <- this.country@Polygons[p][[1]]
            points.in.this.country <- points.in.this.country +
                point.in.polygon(long,lat, this.poly@coords[,1], this.poly@coords[,2])
        }

        countries[points.in.this.country>0] <- as.character(shape@data$CNTRY_NAME[i])
    }

    return(factor(countries))
}) # end findCountry for matrices






################
## for data.frames (of long/lat)
################
setMethod("findCountry", "gGraph", function(x, shape="world",...){
    x <- as.matrix(x)
    return(findCountry(x, shape=shape, ...))
}) # end findCountry







##############
## for gGraph
##############
setMethod("findCountry", "gGraph", function(x, shape="world", attr.name="country",...){
    coords <- getCoords(x)
    res <- findCountry(coords, shape=shape, ...)
    if(nrow(x@nodes.attr)>1){
        x@nodes.attr <- cbind.data.frame(x@nodes.attr,res)
        names(x@nodes.attr)[ncol(x@nodes.attr)] <- attr.name

    } else {
        x@nodes.attr <- data.frame(res)
        names(res) <- attr.name
    }

    return(x)
}) # end findCountry

