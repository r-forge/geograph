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

    toAdd <- list(from=NULL, to=NULL)
    spoint <- 1:2

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





#################
## geo.remove.edges
#################
geo.remove.edges <- function(x) {
    ## preliminary stuff
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    temp <- isInArea(x)
    coords <- getCoords(x)[temp,]
    nodes <- getNodes(x)[temp]
    lon <- coords[,1]
    lat <- coords[,2]
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    psize <- get("psize", env=env)

    toRemove <- list(from=NULL, to=NULL)
    spoint <- 1:2

    while (length(spoint) > 1) {
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

