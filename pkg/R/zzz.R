.First.lib <- function (lib, pkg){
##.initAdegenetClasses()
##.initAdegenetUtils()
    ## library.dynam("adegenet", pkg, lib)

    cat("\n\t====================\n")
    cat("\t geoGraph is loaded ")
    cat("\t\n====================\n")
    assign(".geoGraphEnv", new.env(parent=.GlobalEnv), env=.GlobalEnv)
}
