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
data(hgdpPlus)
data(worldgraph.40k)

setwd("/home/master/dev/geograph/pkg/misc/simulations/")
source("doSimul.R")



##
## COMPUTE COSTS
##

## turn productivity into costs (range: 0-100)
myCosts1 <- getNodesAttr(worldgraph.40k)$meanProd
myCosts1 <- max(myCosts1) - myCosts1
range(myCosts1)
myCosts1 <- 100*myCosts1/max(myCosts1) # range 0 - 100
range(myCosts1)
hist(myCosts1, col="grey")


## turn var of productivity into costs (range: 0-100)
myCosts2 <- getNodesAttr(worldgraph.40k)$varProd
myCosts2 <- 100*(myCosts2 +1)/max(myCosts2 +1 ) # range 0 - 100
range(myCosts2)
hist(myCosts2, col="grey")


## merge both
myCosts <- (myCosts1 + myCosts2)/2
myCosts <- 100* myCosts /max(myCosts)
hist(myCosts, col="grey")
myCol <- round((100-myCosts)/100, 10)
myCol <- gray(myCol)
myCol[worldgraph.40k@nodes.attr$sea] <- rgb(1,1,1,1)
plot(worldgraph.40k, col=myCol)


## set costs for special habitats
myCosts[worldgraph.40k@nodes.attr$sea] <- 1e10
myCosts[worldgraph.40k@nodes.attr$deselected.land] <- 1e10
myCosts[worldgraph.40k@nodes.attr$landbridge] <- 100


## set costs
worldgraph.40k <- setFriction(worldgraph.40k, node.costs=myCosts)
worldgraph.40k <- dropDeadEdges(worldgraph.40k, thres=2e5)

isConnected(hgdpPlus)

source("doSimul.R")

##
## using Addis Ababa as source
##
addis <- list(lon=38.74,lat=9.03)
addis <- closestNode(worldgraph.40k,addis) # this takes a while
doSimul(addis,hgdpPlus, ".") # result: R2=0.7946


##
## define candidate nodes on a large grid
##
temp <- expand.grid(seq(-30,60,by=5), seq(-40,40,by=5))
x <- closestNode(worldgraph.40k,temp) # this takes a while

conCom <- connectedComp(getGraph(worldgraph.40k)) # get connected sets
conCom <- conCom[order(sapply(conCom,length),decreasing=TRUE)] # sort by decr. size
conCom <- conCom[[1]]
myCandidates <- intersect(x,conCom) # retained candidates
length(myCandidates)

temp <- findInLayer(getCoords(worldgraph.40k)[myCandidates,], attr="CONTINENT")
myCandidates <- row.names(temp[temp=="Africa",,drop=FALSE]) # keep nodes in Africa
plot(worldgraph.40k)
points(getCoords(worldgraph.40k)[myCandidates,])



## make some simulations
res <- doSimul(myCandidates, hgdpPlus, "outputs") # this can take hours (3 sim/minute)

## load("outputs/candidates.RData")
## res <- doSimul(candidates, "outputs")
## myNA <- names(res)[is.na(res)]
## myNA # there are NAs
## myNA %in% getNodes(hgdpPlus) # NAs are all nodes associated to one population in hgdpPlus

## examin result
range(res, na.rm=TRUE)
hist(res, col="grey")

## plot the worst one
load(paste("outputs/path",which.min(res),".RData",sep=""))
par(mar=c(0,0,2,0))
plot(worldgraph.40k,res=TRUE, col=0)
plot(myPath,seed=1)
points(hgdpPlus,col.node="black")
ori <- sub(":.*","",names(myPath)[1])
points(getCoords(worldgraph.40k)[ori,1],getCoords(worldgraph.40k)[ori,2],pch="x", cex=2,col="red")
title("'worst' result")

## plot the best one
x11()
load(paste("outputs/path0",which.max(res),".RData",sep=""))
par(mar=c(0,0,2,0))
plot(worldgraph.40k,res=TRUE, col=0)
plot(myPath,seed=1)
points(hgdpPlus,col.node="black")
ori <- sub(":.*","",names(myPath)[1])
points(getCoords(worldgraph.40k)[ori,1],getCoords(worldgraph.40k)[ori,2],pch="x", cex=2,col="red")
title("'best' result")

## plot all results (R^2)
dispRes <- res - min(res,na.rm=TRUE)
dispRes <- 1 + (99*dispRes/max(dispRes,na.rm=T))
range(dispRes,na.rm=T)

x <- worldgraph.40k[myCandidates]

plot(x,reset=TRUE)
palette(heat.colors(105))
points(getCoords(x),col=dispRes,pch=15,cex=1)
points(hgdpPlus,col.node="blue") # show the NA -> these are pop from hgdpPlus
