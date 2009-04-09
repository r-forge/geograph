doSimul <- function(startNode, data = hgdp, path=""){
    ## set path
    curwd <- getwd()
    setwd(path)
    on.exit(setwd(curwd))

    ## some variables
    N <- length(startNode)
    vec.lab <- .genlab("",N)
    R2 <- numeric(N)


    ## loop: 1 iteration = one path
    for(i in 1:N){
        objName <- paste("path",vec.lab[i],sep="")
        assign(objName, dijkstraFrom(data, startNode[i]))
        myPath <- get(objName)
        save(myPath, file=paste(objName,".RData",sep=""))

        geo.dist <- sapply(myPath[-length(myPath)],function(e) e$length)
        geo.dist[is.na(geo.dist)] <- 0 # occurs when the source is one hgdp pop

        div <- data@data$Genetic.Div
        ##R2[i] <- cor(geo.dist, div/(1-div) )^2
        R2[i] <- cor(geo.dist, div)^2
        cat("\n",i,"/",N,": R2 =", R2[i])
    }

    cat("\n")

    ## return result
    names(R2) <- startNode
    return(R2)

} # end doSimul
