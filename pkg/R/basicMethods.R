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
    if(missing(i)) {
        i <- TRUE
    }
    if(is.logical(i)){
        i <- rep(i, length=nrow(getCoords(x)))
    }
    if(missing(j)) {
        j <- TRUE
    }
    argList <- list(...)
    if(is.null(argList$useSubGraph)){
        useSubGraph <- FALSE
    } else {
        useSubGraph <- argList$useSubGraph
    }

    nodeNames <- getNodes(x)

    ## do the subsetting ##
    res <- x
    res@coords <- res@coords[i, , drop=FALSE]
    if(nrow(res@nodes.attr)>0){
        res@nodes.attr <- res@nodes.attr[i, j, drop=FALSE]
    }
    if(useSubGraph){ # use procedure from graph package to subset graph (slow)
        myGraph <- subGraph(nodes(res@graph)[i], res@graph)
    } else { # use a customized procedure (faster)
        myGraph <- getGraph(res)
        myGraph@nodes <- myGraph@nodes[i]
        myGraph@edgeL <- myGraph@edgeL[myGraph@nodes]
        ## special handling of i, to know which indexes are kept
        if(is.character(i)){ # type == character
            keptIdx <- match(i, nodeNames)
            keptIdx <- !is.na(keptIdx)
        }
        if(is.logical(i)){ # type == logical
            keptIdx <- which(i)
        }
        if(is.numeric(i)){ # type == numeric
            if(i[1]>0) {
                keptIdx <- i
            } else{
                keptIdx <- setdiff(1:nrow(x@coords), i)
            }
        }

        f1.noweights <- function(oneNode){ # function to subset graph without weights
            oneNode$edges <- oneNode$edges[oneNode$edges %in% keptIdx]
            return(oneNode)
        }
        f1.withweights <- function(oneNode){ # function to subset graph with weights
            temp <- oneNode$edges %in% keptIdx
            oneNode$edges <- oneNode$edges[temp]
            oneNode$weights <- oneNode$weights[temp]
            return(oneNode)
        }

        if(is.null(myGraph@edgeL[[1]]$weights)){
            myGraph@edgeL <- lapply(myGraph@edgeL, f1.noweights)
        } else {
            myGraph@edgeL <- lapply(myGraph@edgeL, f1.withweights)
        }
    } # end subset graph

    res@graph <- myGraph

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

    cat("\n@meta: list of meta information with", length(x@meta),"items\n")
    if(length(x@meta)>0) print(paste("$", names(x@meta), sep=""))

    cat("\n@graph:\n")
    print(x@graph)

    cat("\n@gGraphHistory: (", length(x@history@cmd)," items )\n")
    print(x@history[1:min(nDisp,length(x@history@cmd))])
    if(length(x@history@cmd) > nDisp) cat("\n...\n")

}) # end show gGraph

