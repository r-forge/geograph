#################
## BASIC METHODS
#################
setMethod("[", "gGraphHistory", function(x, i) {
    if(missing(i)) i <- TRUE

    res <- x
    res@cmd <- res@cmd[i]
    res@dates <- res@dates[i]
    res@comments <- res@comments[i]

    return(res)
})




setMethod("[", "gGraph", function(x, i, j, ..., drop=TRUE) {
    if(missing(i)) i <- TRUE
    if(missing(j)) j <- TRUE

    res <- x
    res@coords <- res@coords[i, , drop=FALSE]
    res@coords.attr <- res@nodes.attr[i, j, drop=FALSE]
    res@graph <- subGraph(nodes(res@graph)[i], res@graph)
    res@history <- res@history[i]

    return(res)
})





################
## SHOW METHODS
################
setMethod("show", "gGraphHistory", function(object){
    x <- object
    N <- length(x@cmd)

    ## printing
    cat("\n=== gGgraphHistory ===\n")
    if(N > 0){
        for(i in 1:N){
            cat(i, "// Date:", x@dates[i], "\n")
            cat("// Comment:", x@comments[i], "\n")
            print(x@cmd[[i]])
            cat("\n")
        }
    } else{
        cat("\t- empty object -\n")
    }

}) # end show gGraphHistory





setMethod("show", "gGraph", function(object){
    x <- object
    N <- nrow(x@coords)
    nDisp <- 3

    ## printing
    cat("\n=== gGgraph object ===\n")
    if(N > 0){
        cat("\n\t- @coords: spatial coordinates of nodes -\n")
        head(x@coords, nDisp)

        cat("\n\t- @nodes.attr: nodes attributes -\n")
        head(x@nodes.attr, nDisp)

        cat("\n\t- @graph: -\n")
        print(x@graph)

        cat("\n\t- @gGraphHistory -\n")
        print(x@history[1:nDisp])

    } else{
        cat("\t- empty object -\n")
    }

}) # end show gGraph







