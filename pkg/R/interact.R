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

    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    psize <- last.plot.param$psize
    pch <- last.plot.param$pch


    ## initialize toAdd
    toAdd <- list(from=NULL, to=NULL)
    spoint <- 1:2

    ## getting input from the user
    while (length(spoint) > 1) {
        spoint <- NULL
        spoint <- identify(lon, lat, plot=FALSE, n=2)
        if(length(spoint) > 1) {
            segments(lon[spoint[1]], lat[spoint[1]], lon[spoint[2]], lat[spoint[2]], col="green")
            points(lon[spoint[1]],lat[spoint[1]],cex=psize, col="green", pch=pch)
            points(lon[spoint[2]],lat[spoint[2]],cex=psize, col="green", pch=pch)

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
    # coords <- getCoords(x)[temp,] # not needed: can work with whole object
    coords <- getCoords(x)
    nodeNames <- getNodes(x)
    lon <- coords[,1]
    lat <- coords[,2]
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    psize <- get("psize", env=env)
    mode <- match.arg(mode)

    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    psize <- last.plot.param$psize
    pch <- last.plot.param$pch

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
                points(lon[spoint[1]], lat[spoint[1]], cex=psize, col="red", pch=pch)
                points(lon[spoint[2]], lat[spoint[2]], cex=psize, col="red", pch=pch)

                toRemove$from <- c(toRemove$from, nodeNames[spoint[1]])
                toRemove$to <- c(toRemove$to, nodeNames[spoint[2]])
            }
        }
    } # end mode: points

    if(mode=="area"){
        selArea <- data.frame(x=1:2,y=1:2)

        ## getting input from the user
        while(nrow(selArea) > 1) {
            ##  selArea <- selArea[integer(0),]  not needed
            selArea <- data.frame(locator(2))

            if(nrow(selArea) > 1) {
                selIdx <- which(isInArea(x, reg=selArea)) # indices of selected points
                selEdges <- getEdges(x, mode="matId", unique=TRUE) # edges, nodes=numerical indices
                temp <- (selEdges[,1] %in% selIdx) & (selEdges[,2] %in% selIdx)
                selEdges <- selEdges[temp,] # edges wholly inside the selected area

                segments(lon[selEdges[,1]], lat[selEdges[,1]], lon[selEdges[,2]], lat[selEdges[,2]], col="red")
                points(lon[selIdx], lat[selIdx], cex=psize*1.5, col="red")

                toRemove$from <- c(toRemove$from, nodeNames[selEdges[,1]])
                toRemove$to <- c(toRemove$to, nodeNames[selEdges[,2]])
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





###################
## geo.change.attr
###################
geo.change.attr <- function(x, mode=c("points","area"), attr.name, attr.value, newCol="black") {
    ## preliminary stuff
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    coords <- getCoords(x)
    lon <- coords[,1]
    lat <- coords[,2]
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    mode <- match.arg(mode)
    if(!attr.name %in% colnames(x@nodes.attr)) stop("specified node attribute name not found")

    ## set replacement color
    if( (!is.null(x@meta$color)) && (attr.name %in% colnames(x@meta$color)) ){
        temp <- which(attr.value == x@meta$color[,attr.name])[1]
        if(!is.na(temp)){ # attr.value is documented in @meta$color
            newCol <- x@meta$color[temp,2]
        } else{ # if attr.value is not documented, we document it in @meta$color
            if(is.factor(x@meta$color[,attr.name])){ # if attr is a factor
                x@meta$color[,attr.name] <- as.character(x@meta$color[,attr.name]) # convert as character
                x@meta$color <- rbind.data.frame(x@meta$color, c(attr.value,newCol))
                x@meta$color[,attr.name] <- factor(x@meta$color[,attr.name]) # restore factor type
            } else { # attr is not a factor
                x@meta$color <- rbind.data.frame(x@meta$color, c(attr.value,newCol))
            }
        }
    } # end setting replacement color


    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    psize <- last.plot.param$psize
    pch <- last.plot.param$pch

    ## initialize toChange
    toChange <- integer(0)


    ## mode: points ##

    if(mode=="points"){
        spoint <- 0
        ## getting input from the user
        while(length(spoint) > 0) {
            spoint <- NULL
            spoint <- identify(lon, lat, plot=FALSE, n=1)
            if(length(spoint) > 0) {
                points(lon[spoint], lat[spoint], cex=psize, pch=pch, col=newCol)

                toChange <- c(toChange, spoint)
            }
        }
    } # end mode: points

    if(mode=="area"){
        selArea <- data.frame(x=1:2,y=1:2)

        ## getting input from the user
        while(nrow(selArea) > 1) {
            selArea <- selArea[integer(0),]
            selArea <- data.frame(locator(2))

            if(nrow(selArea) > 1) {
                selIdx <- which(isInArea(x, reg=selArea)) # indices of selected points
                points(lon[selIdx], lat[selIdx], cex=psize, pch=pch, col=newCol)

                toChange <- c(toChange, selIdx)
            }
        }

    } # end mode: area


    ## make changes ##
    toChange <- unique(toChange)
    res <- x

    if(is.factor(res@nodes.attr[,attr.name])){ # special handling if attr is a factor
        temp <- as.character(res@nodes.attr[, attr.name])
        temp[toChange] <- attr.value
        res@nodes.attr[,attr.name] <- factor(temp)
    } else { # in other cases...
        res@nodes.attr[toChange,attr.name] <- attr.value
    }

    ## need to save the call here ! ##

    return(res)
} # end geo.change.attr


