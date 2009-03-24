##
## BASED ON HGDP DATA
## TRY DIFFERENT ORIGINS and:
## - compute least-cost paths
## - obtain subsequent geographic distances
## - compute correlation with genetic diversity within pop
## - display the results


##
## LOAD STUFF
##
library(geoGraph)
data(hgdp)
data(worldgraph.40k)



##
## DEFINE COSTS
##

## habitat
hab.costs <- getNodeCosts(worldgraph.40k,attr.name="habitat")
hab.costs <- hab.costs/5 # standardize

## latitude
lat <- getCoords(worldgraph.40k)[,"lat"]
lat.costs <- lat^4 # this is arbitrary
lat.costs <- lat.costs/max(lat.costs) # standardize
plot(lat.costs ~ abs(lat), main="cost due to latitude", ylim=c(0,1)) # more or less what we want

## combined costs
myCosts <- hab.costs + lat.costs

## show costs map
##myCosts.disp <- round(100*(myCosts/max(myCosts)))
##palette(terrain.colors(110))
##plot(getCoords(worldgraph.40k), col=myCosts.disp, pch=20)


## define edge costs
worldgraph.40k <- setFriction(worldgraph.40k, node.costs=myCosts)


## compute shortest path
##myPath <- dijkstraFrom(hgdp, ori)
##plot(worldgraph.40k,reset=TRUE)
##plot(myPath)




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
        R2[i] <- cor(geo.dist, hgdp@data$Genetic.Div)^2
        cat("\n",i,"R2:", R2[i])
    }

    cat("\n")

    ## return result
    names(R2) <- startNode
    return(R2)

} # end doSimul



## make some simulations
##res <- doSimul(c("21820","22459"), "outputs")


load("/home/master/dev/geograph/pkg/misc/simulations/outputs/path1.RData")
plot(worldgraph.40k, col=0)
plot(myPath)
