###############
## setFriction
###############
setFriction <- function(x, attr.name=NULL, method=c("mean", "product"), drop=TRUE){
    ## some checks + argument handling
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    method <- match.arg(method)


    ## assign weights to vertices
    nodeAttr <- unlist(getNodesAttr(x, attr.name=attr.name))
    if(!is.null(x@meta$costs)){
        if(!any(attr.name %in% colnames(x@meta$costs))) {
            stop("attr.name is not documented in x@meta$costs.")
        }
        nodeWeights <- as.character(nodeAttr)
        rules <- x@meta$costs
        for(i in 1:nrow(x@meta$costs)){
            nodeWeights[nodeWeights==rules[i,attr.name]] <- rules[i,"weight"]
        }
        nodeWeights <- as.numeric(nodeWeights)
    } else stop("x@meta does not contain a 'weights' component.")

    ## find weights of edges as a function of terminating vertices
    EL <- getGraph(x)@edgeL

    ## method == mean ##
    if(method=="mean"){
        for(i in 1:length(EL)){
            EL[[i]]$weights <- (nodeWeights[i] + nodeWeights[EL[[i]]$edges]) / 2
            }
    }

    ## method == product ##
    if(method=="product"){
        for(i in 1:length(EL)){
            EL[[i]]$weights <- nodeWeights[i] * nodeWeights[EL[[i]]$edges]
            }
    }

    ## return result
    newGraph <- new("graphNEL", nodes=getNodes(x), edgeL=EL)
    res <- x
    res@graph <- newGraph

    return(res)
} # end setFriction
