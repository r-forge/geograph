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
## geo.zoomin
##############
geo.zoomin <- function(reg=NULL){
    ## get environment
    geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

    ## get last plot
    last.plot.call <- get("last.plot", envir=geoEnv)


    ## reg provided => no loop ##
    if(!is.null(reg)){

        ## define new xlim and ylim
        if(!is.list(reg) || length(reg)!=2) stop("Wrong reg specified.")
        reg <- lapply(reg, sort)

        .zoomlog.up(c(reg$x, reg$y))

        ## reconstruct a valid call to plot
        temp <- deparse(last.plot.call)
        temp <- sub("reset[^,]*,","",temp) # remove subset if provided
        temp <- sub(",[[:blank:]]*reset[^)]*", "",temp) # same thing, if last arg

        ##     temp <- sub("ylim[^,]*,","",temp) # idem, ylim
        ##     temp <- sub(")$","",temp) # idem, ylim
        ##     temp <- paste(temp, ", xlim = c(", reg$x[1], ",", reg$x[2],")")
        ##     temp <- paste(temp, ", ylim = c(", reg$y[1], ",", reg$y[2],")")
        ##     temp <- paste(temp, ")")

        newCall <- parse(text=temp)
        eval(newCall)

    } else { ## reg not provided => looping ##

        reg <- data.frame(x=1:2,y=1:2)

        ## getting input from the user
        while(nrow(reg) > 1) {
            reg <- reg[integer(0),]
            reg <- data.frame(locator(2))

            if(nrow(reg) > 1) {
                ## define new xlim and ylim
                reg <- as.list(reg)
                reg <- lapply(reg, sort)

                .zoomlog.up(c(reg$x, reg$y))

                ## reconstruct a valid call to plot
                temp <- deparse(last.plot.call)
                temp <- sub("res..[^,]*,","",temp) # remove subset if provided
                temp <- sub(",[[:blank:]]*res..[^)]*", "",temp) # same thing, if last arg

                newCall <- parse(text=temp)
                eval(newCall)

                reg <- data.frame(reg)
            } # end if nrow(reg) > 1

        } # end while
    } # end else

    return(invisible())
} # end geo.zoomin




##############
## geo.zoomout
##############
geo.zoomout <- function(){
    ## get environment
    geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

    ## loop ##
    while(!is.null(locator(1))){
        ## get last plot
        last.plot.call <- get("last.plot", envir=geoEnv)

        ## get former coordinates and go one step back
        zoomLog <- get("zoom.log", env=geoEnv)
        if(nrow(zoomLog) < 2) {
            cat("\nNo previous zoom coordinates in zoom history.\n")
            return(invisible())
        }

        zoomLog <- zoomLog[-1,,drop=FALSE]
        assign("zoom.log", zoomLog, env=geoEnv)

        ## reconstruct a valid call to plot
        temp <- deparse(last.plot.call)

        newCall <- parse(text=temp)

        eval(newCall)
    }

    return(invisible())
} # end geo.zoomout

