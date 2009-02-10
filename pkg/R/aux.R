############
## isInArea
############
isInArea <- function(x){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    coords <- getCoords(x)

    ## main computations
    if(exists("zoom.log", envir=env)){
        zoomlog <- get("zoom.log", envir=env)
        zoomlog <- zoomlog[1,]

        xlim <- zoomlog[1:2]
        ylim <- zoomlog[3:4]

        toKeep <- ( (coords[,1] >= xlim[1]) & (coords[,1] <= xlim[2])  # matching longitude
                   & (coords[,2] >= ylim[1]) & (coords[,2] <= ylim[2]) ) # matching latitude

        names(toKeep) <- rownames(coords)
        return(toKeep)
    } else return(TRUE)

} # end isInArea
