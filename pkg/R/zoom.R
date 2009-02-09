###############
## .zoomlog.up
###############
.zoomlog.up <- function(vec){
    if(!is.vector(vec) || length(vec)!=4) stop("Updating zoomlog using a wrong value.")

    geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)
    oldZoomLog <- get("zoom.log", env=geoEnv)
    newZoomLog <- rbind(vec, oldZoomLog)
    colnames(newZoomLog) <- colnames(oldZoomLog)

    if(nrow(newZoomLog) > 100){
        newZoomLog <- newZoomLog[1:100,]
    }
    assign("zoom.log", newZoomLog,env=geoEnv)


    return(invisible())
}





##############
## zoomin.geo
##############
zoomin.geo <- function(){
    ## get environment
    geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

    ## get last plot
    last.plot.call <- get("last.plot", envir=geoEnv)

    ## define new xlim and ylim
    reg <- locator(2)
    reg <- lapply(reg, sort)

   .zoomlog.up(c(reg$x, reg$y))

    ## reconstruct a valid call to plot
    temp <- deparse(last.plot.call)
    ##  temp <- sub("xlim[^,]*,","",temp) # remove xlim if given
    ##     temp <- sub("ylim[^,]*,","",temp) # idem, ylim
    ##     temp <- sub(")$","",temp) # idem, ylim
    ##     temp <- paste(temp, ", xlim = c(", reg$x[1], ",", reg$x[2],")")
    ##     temp <- paste(temp, ", ylim = c(", reg$y[1], ",", reg$y[2],")")
    ##     temp <- paste(temp, ")")

    newCall <- parse(text=temp)

    eval(newCall)

}
