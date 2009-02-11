.First.lib <- function (lib, pkg){
    ##.initAdegenetClasses()
    ##.initAdegenetUtils()
    ## library.dynam("adegenet", pkg, lib)

    cat("\n\t========================\n")
    cat("\tgeoGraph 1.0-0 is loaded")
    cat("\n\t========================\n")

    ## define various items in our specific environment
    assign(".geoGraphEnv", new.env(parent=.GlobalEnv), env=.GlobalEnv)
    geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

    zoom.log <- matrix(c(-180,180,-90,90),ncol=4)
    colnames(zoom.log) <- c("x1","x2","y1","y2")

    assign("zoom.log", zoom.log, env=geoEnv)
    assign("psize", 0.5, env=geoEnv)

    temp <- list(psize=0.5, pch=19, col="black")
    assign("last.plot.param", temp, env=geoEnv)
}
