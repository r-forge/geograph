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

setwd("/home/master/dev/geograph/pkg/misc/simulations/")



##
## DROP COSTS
##
worldgraph.40k <- dropCosts(worldgraph.40k)




##
## function doing the simulations
##

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
        cat("\n",i,"R2:", R2[i])
    }

    cat("\n")

    ## return result
    names(R2) <- startNode
    return(R2)

} # end doSimul



##
## using Addis Ababa as source
##
addis <- list(lon=38.74,lat=9.03)
addis <- closestNode(worldgraph.40k,addis) # this takes a while
doSimul(addis,".") # result: R2=0.749


##
## define candidate nodes on a large grid
##
temp <- expand.grid(seq(-180,180,by=12), seq(-90,90,by=6))
x <- closestNode(worldgraph.40k,temp) # this takes a while

conCom <- connectedComp(getGraph(worldgraph.40k)) # get connected sets
conCom <- conCom[order(sapply(conCom,length),decreasing=TRUE)] # sort by decr. size
conCom <- conCom[[1]]
myCandidates <- intersect(x,conCom) # retained candidates
length(myCandidates)


## make some simulations
res <- doSimul(myCandidates, "outputs") # this can take hours (3 sim/minute)

## load("outputs/candidates.RData")
## res <- doSimul(candidates, "outputs")
## myNA <- names(res)[is.na(res)]
## myNA # there are NAs
## myNA %in% getNodes(hgdp) # NAs are all nodes associated to one population in hgdp

## examin result
range(res, na.rm=TRUE)
hist(res, col="grey")

## plot the worst one
load(paste("outputs/path",which.min(res),".RData",sep=""))
par(mar=c(0,0,2,0))
plot(worldgraph.40k,res=TRUE, col=0)
plot(myPath,seed=1)
points(hgdp,col.node="black")
ori <- sub(":.*","",names(myPath)[1])
points(getCoords(x)[ori,1],getCoords(x)[ori,2],pch="x", cex=2,col="red")
title("'worst' result")

## plot the best one
x11()
load(paste("outputs/path0",which.max(res),".RData",sep=""))
par(mar=c(0,0,2,0))
plot(worldgraph.40k,res=TRUE, col=0)
plot(myPath,seed=1)
points(hgdp,col.node="black")
ori <- sub(":.*","",names(myPath)[1])
points(getCoords(x)[ori,1],getCoords(x)[ori,2],pch="x", cex=2,col="red")
title("'best' result")

## plot all results (R^2)
dispRes <- res - min(res,na.rm=TRUE)
dispRes <- 1 + (99*dispRes/max(dispRes,na.rm=T))
range(dispRes,na.rm=T)

x <- worldgraph.40k[candidates]

plot(x,reset=TRUE)
palette(heat.colors(105))
points(getCoords(x),col=dispRes,pch=15,cex=1)
points(hgdp,col.node="blue") # show the NA -> these are pop from hgdp
