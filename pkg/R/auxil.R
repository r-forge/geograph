##############
## hasCosts
##############
hasCosts <- function(x){
    if(length(getGraph(x)@edgeData@data)==0) return(FALSE)
    w <- getCosts(x, res.type="vector")
    if(length(unique(w)) < 2) return(FALSE)
    return(TRUE)
}
