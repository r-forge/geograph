############
## isInArea
############
isInArea <- function(x, reg="current", res.type=c("logical","character"), buffer=0){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    res.type <- match.arg(res.type)

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    coords <- getCoords(x)

    ## get xlim and ylim
    if(exists("zoom.log", envir=env) && reg=="current"){ # xlim/ylim taken from log
        zoomlog <- get("zoom.log", envir=env)
        zoomlog <- zoomlog[1,]

        xlim <- zoomlog[1:2]
        ylim <- zoomlog[3:4]

    } else if(is.list(reg)){ # xlim/ylim user-provided (reg)
        if(length(reg)!=2) stop("reg is not a list of length 2.")
        xlim <- sort(reg[[1]])[1:2]
        ylim <- sort(reg[[2]])[1:2]

    } else return(NA)


    ## main computations ##

    ## handle a buffer around area
    bufferx <- (xlim[2]-xlim[1])*buffer
    buffery <- (ylim[2]-ylim[1])*buffer

    xlim <- xlim + c(-bufferx, bufferx)
    ylim <- ylim + c(-buffery, buffery)

    toKeep <- ( (coords[,1] >= xlim[1]) & (coords[,1] <= xlim[2])  # matching longitude
               & (coords[,2] >= ylim[1]) & (coords[,2] <= ylim[2]) ) # matching latitude

    names(toKeep) <- rownames(coords)

    if(res.type=="logical"){ # return a named vector of logicals
        return(toKeep)
    }

    if(res.type=="character"){ # return names of nodes in the area
        res <- names(toKeep)[toKeep]
        return(res)
    }


} # end isInArea
