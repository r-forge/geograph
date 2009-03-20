##############
## hasCosts
##############
hasCosts <- function(x){
    if(length(getGraph(x)@edgeData@data)==0) return(FALSE)
    w <- getCosts(x, res.type="vector")
    if(length(unique(w)) < 2) return(FALSE)
    return(TRUE)
}






###################
## geo.segments
###################
##
## Rectifies segments drawn from one hemisphere to another
## in the wrong direction (i.e. not the shortest path)
## and draws it.
##
## Is to be called instead segments but will be slower.
##
geo.segments <- function(x0, y0, x1, y1,
             col = par("fg"), lty = par("lty"), lwd = par("lwd"), ...){

    ## some declarations ##
    THRES <- 91
    XMIN <- -180
    XMAX <- 180

    ## pin down problematic segments ##
    toChange <- abs(x0-x1) > THRES
    if(sum(toChange)==0){
        segments(x0, y0, x1, y1,
             col = par("fg"), lty = par("lty"), lwd = par("lwd"), ...)
        return()
    }

    ## isolate problematic segments
    temp.x0 <- x0[toChange]
    temp.x1 <- x1[toChange]
    temp.y0 <- y0[toChange]
    temp.y1 <- y1[toChange]

    ## remove problematic segments
    x0 <- x0[!toChange]
    x1 <- x1[!toChange]
    y0 <- y0[!toChange]
    y1 <- y1[!toChange]

    ## sort x coordinates


    ## define new segments ##
    ## notations:
    ## - x0: x coord, left point
    ## - x1: x coord, right point
    ## - d0: distance x0 - XMIN
    ## - d1: distance XMAX - x1
    ## - h0, h1: differential of y coord for new coord
    ## (h0/d0 = h1/d1)
    ## - H: distance between y0 and y1


    ## add new segments to old segments




    ## final call to segments ##
    segments(x0, y0, x1, y1,
             col = par("fg"), lty = par("lty"), lwd = par("lwd"), ...)
} # end geo.segments
