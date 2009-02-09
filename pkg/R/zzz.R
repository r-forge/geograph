.First.lib <- function (lib, pkg){
    ##.initAdegenetClasses()
    ##.initAdegenetUtils()
    ## library.dynam("adegenet", pkg, lib)

    cat("\n\t========================\n")
    cat("\t geoGraph 1.0-0 is loaded ")
    cat("\n\t========================\n")

    ## define various items in our specific environment
    assign(".geoGraphEnv", new.env(parent=.GlobalEnv), env=.GlobalEnv)
    geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

    zoom.log <- matrix(c(-180,180,-90,90),ncol=4)
    colnames(zoom.log) <- c("x1","x2","y1","y2")

    assign("zoom.log", zoom.log, env=geoEnv)
    assign("psize", 2, env=geoEnv)
}
