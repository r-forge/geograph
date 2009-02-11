#################
## geo.add.edges
#################
geo.add.edges <- function(x) {
    ## preliminary stuff
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    temp <- isInArea(x)
    coords <- getCoords(x)[temp,]
    nodes <- getNodes(x)[temp]
    lon <- coords[,1]
    lat <- coords[,2]
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    psize <- get("psize", env=env)

    ## initialize toAdd
    toAdd <- list(from=NULL, to=NULL)
    spoint <- 1:2

    ## getting input from the user
    while (length(spoint) > 1) {
        spoint <- NULL
        spoint <- identify(lon, lat, plot=FALSE, n=2)
        if(length(spoint) > 1) {
            segments(lon[spoint[1]], lat[spoint[1]], lon[spoint[2]], lat[spoint[2]], col="green")
            points(lon[spoint[1]],lat[spoint[1]],cex=psize)
            points(lon[spoint[2]],lat[spoint[2]],cex=psize)

            toAdd$from <- c(toAdd$from, nodes[spoint[1]])
            toAdd$to <- c(toAdd$to, nodes[spoint[2]])
        }
    }

    ## make sure added edges are unique
    toAdd <- as.matrix(as.data.frame(toAdd))
    toAdd <- t(apply(toAdd,1,sort)) # sorting
    toAdd <- paste(toAdd[,1], toAdd[,2], sep="-") # making strings
    toAdd <- unique(toAdd) # keep unique strings
    toAdd <- strsplit(toAdd, "-")
    from <- sapply(toAdd, function(e) e[1])
    to <- sapply(toAdd, function(e) e[2])

    ## call to setEdges
    res <- setEdges(x=x, add=cbind(from, to) )

    return(res)
} # end geo.add.edges





####################
## geo.remove.edges
####################
geo.remove.edges <- function(x, mode=c("points","area")) {
    ## preliminary stuff
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    temp <- isInArea(x)
    coords <- getCoords(x)[temp,]
    nodes <- getNodes(x)[temp]
    lon <- coords[,1]
    lat <- coords[,2]
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    psize <- get("psize", env=env)
    mode <- match.arg(mode)

    ## initialize toRemove
    toRemove <- list(from=NULL, to=NULL)


    ## mode: points ##

    if(mode=="points"){
        spoint <- 1:2
        ## getting input from the user
        while(length(spoint) > 1) {
            spoint <- NULL
            spoint <- identify(lon, lat, plot=FALSE, n=2)
            if(length(spoint) > 1) {
                segments(lon[spoint[1]], lat[spoint[1]], lon[spoint[2]], lat[spoint[2]], col="red")
                points(lon[spoint[1]],lat[spoint[1]],cex=psize)
                points(lon[spoint[2]],lat[spoint[2]],cex=psize)

                toRemove$from <- c(toRemove$from, nodes[spoint[1]])
                toRemove$to <- c(toRemove$to, nodes[spoint[2]])
            }
        }
    } # end mode: points

    if(mode=="area"){
        sarea <- data.frame(x=1:2,y=1:2)
        edg <- getEdges(x[isInArea(x)], mode="matrix", unique=TRUE)

        ## getting input from the user
        while(nrow(sarea[[1]]) > 1) {
            sarea <- sarea[integer(0),]
            sarea <- locator(2)

            if(nrow(sarea[[1]]) > 1) {
                selPoints <- isInArea(x, reg=sarea)

                segments(lon[selPoints[1]], lat[spoint[1]], lon[spoint[2]], lat[spoint[2]], col="red")
                points(lon[spoint[1]],lat[spoint[1]],cex=psize)
                points(lon[spoint[2]],lat[spoint[2]],cex=psize)

                toRemove$from <- c(toRemove$from, nodes[spoint[1]])
                toRemove$to <- c(toRemove$to, nodes[spoint[2]])
            }
        }

    } # end mode: area



    ## handle toRemove ##
    ## make sure removed edges are unique
    toRemove <- as.matrix(as.data.frame(toRemove))
    toRemove <- t(apply(toRemove,1,sort)) # sorting
    toRemove <- paste(toRemove[,1], toRemove[,2], sep="-") # making strings
    toRemove <- unique(toRemove) # keep unique strings
    toRemove <- strsplit(toRemove, "-")
    from <- sapply(toRemove, function(e) e[1])
    to <- sapply(toRemove, function(e) e[2])

    ## call to setEdges
    res <- setEdges(x=x, remove=cbind(from, to) )

    return(res)
} # end geo.remove.edges


