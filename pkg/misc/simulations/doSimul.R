doSimul <- function(startNode,path=""){
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
        assign(objName, dijkstraFrom(hgdp, startNode[i]))
        myPath <- get(objName)
        save(myPath, file=paste(objName,".RData",sep=""))

        geo.dist <- sapply(myPath[-length(myPath)],function(e) e$length)
        geo.dist[is.na(geo.dist)] <- 0 # occurs when the source is one hgdp pop
        R2[i] <- cor(geo.dist, hgdp@data$Genetic.Div)^2
        cat("\n",i,"/",N,": R2 =", R2[i])
    }

    cat("\n")

    ## return result
    names(R2) <- startNode
    return(R2)

} # end doSimul
