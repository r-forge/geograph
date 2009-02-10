############
## findLand
############
findLand <- function(coords, shape="world") {

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

    long <- coords[,1]
    lat <- coords[,2]
    n.country <- length(shape@polygons)

    ## create land vector to score land
    land <- rep(0,length(lat))

    for(i in 1:n.country) {
        this.country <- shape@polygons[i][[1]]
        n.polys <- length(this.country@Polygons)

        for (p in 1:n.polys) {
            this.poly <- this.country@Polygons[p][[1]]
            land <- land + point.in.polygon(long,lat, this.poly@coords[,1], this.poly@coords[,2])
        }
    }
    land[land>1] <- 1
    land[land==0] <- "sea"
    land[land==1] <- "land"

    return(factor(land))
}
