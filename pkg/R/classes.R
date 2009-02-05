###############################################################
###############################################################
## CLASSES DEFINITION FOR THE GEOGRAPH PACKAGE
###############################################################
###############################################################




######################
## CLASSES DEFINITION
######################

setClass("gGraphHistory", representation(cmd = "list", dates = "character", comments = "character"))



setClass("gGraph",
         representation(coords = "matrix", nodes.attr = "data.frame", graph = "graphNEL",
                        history = "gGraphHistory"),
         prototype(coords = matrix(numeric(0), ncol=2, dimnames=list(NULL, c("lon","lat")))
                   nodes.attr = data.frame(),
                   graph = new("graphNEL"),
                   history = new("gGraphHistory"))
         )







####################
## VALIDITY METHODS
####################
.gGprah.valid <- function(x){
    N <- nrow(x@coords)

    ## several cases of non-validity

    ## coords not numeric
    if(!is.numeric(x@coords)){
        cat("\n Content of coords is not numeric.")
        return(FALSE)
    }

    ## wrong nrow for coords
    if(nrow(x@coords.attr != N)){
        cat("\n Number of coords do not match number of node attributes.")
        return(FALSE)
    }

    ## NAs in coords
    if(any(is.na(x@coords))){
        cat("\n NAs in coords coordinates.")
        return(FALSE)
    }


    return(TRUE)
} # end .gGprah.valid





.gGprahHistory.valid <- function(x){
    Lcmd <- length(x@cmd)
    Ldates <- length(x@dates)
    Lcomments <- length(x@comments)
    ## several cases of non-validity ##

    ## empty object always ok
    if(Lcmd == Ldates == Lcomments = 0) return(TRUE)

    ## different length
    if(length(unique(c(Lcmd, Ldates, Lcomments)))>1) {
        cat("\n Components have different lengths.")
        return(FALSE)
    }

    ## cmd wrong class
    if(!all(sapply(x@cmd, class)=="call")){
        cat("\n Some cmd components are not calls.")
        return(FALSE)
    }

    ##if(){
    ##	cat("\n .")
    ##	return(FALSE)
    ##}

    return(TRUE)
} # end .gGprahHistory.valid





setValidity("gGraph", .gGprah.valid)
setValidity("gGraphHistory", .gGprahHistory.valid)

