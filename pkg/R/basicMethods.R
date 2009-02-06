#################
## BASIC METHODS
#################
setMethod("[", "gGraphHistory", function(x, i, j = "missing", drop = "missing") {
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

    ## do the subsetting
    res <- x
    res@coords <- res@coords[i, , drop=FALSE]
    if(nrow(res@nodes.attr)>0){
        res@nodes.attr <- res@nodes.attr[i, j, drop=FALSE]
    }
    res@graph <- subGraph(nodes(res@graph)[i], res@graph)
    res@history <- res@history

    ## remember this subsetting
    curCall <- match.call()
    newHist <- new("gGraphHistory", res@history, cmd=curCall, comments="Subsetting using [...]")
    res@history <- newHist

    return(res)
})





################
## SHOW METHODS
################
setMethod("show", "gGraphHistory", function(object){
    x <- object
    N <- length(x@cmd)

    ## printing
    ## cat("\n=== gGgraphHistory ===\n")
    if(N > 0){
        for(i in 1:N){
            cat("=",i, "=\n")
            cat("Date:", x@dates[i], "\n")
            cat("Comment:", x@comments[i], "\n")
            cat("Command: ")
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
    cat("\n@coords: spatial coordinates of",nrow(x@coords),"nodes\n")
    print(head(x@coords, nDisp))
    if(N > nDisp) cat("...\n")

    cat("\n@nodes.attr:",nrow(x@nodes.attr),"nodes attributes\n")
    print(head(x@nodes.attr, nDisp))
    if(nrow(x@nodes.attr) > nDisp) cat("...\n")

    cat("\n@graph:\n")
    print(x@graph)

    cat("\n@gGraphHistory: (", length(x@history@cmd)," items )\n")
    print(x@history[1:min(nDisp,length(x@history@cmd))])
    if(length(x@history@cmd) > nDisp) cat("\n...\n")

}) # end show gGraph

