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
data(hgdpPlus)
data(worldgraph.40k)

setwd("/home/master/dev/geograph/pkg/misc/simulations/")
source("doSimul.R")



##
## COMPUTE COSTS
##

## load costs

## set no costs
worldgraph.40k <- dropCosts(worldgraph.40k)

isConnected(hgdpPlus)


##
## using Addis Ababa as source
##
addis <- list(lon=38.74,lat=9.03)
addis <- closestNode(worldgraph.40k,addis) # this takes a while
doSimul(addis,hgdpPlus, ".") # result: R2=


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

temp <- extractFromLayer(getCoords(worldgraph.40k)[myCandidates,], attr="CONTINENT")
myCandidates <- row.names(temp[temp=="Africa",,drop=FALSE]) # keep nodes in Africa
plot(worldgraph.40k)
points(getCoords(worldgraph.40k)[myCandidates,])



## make some simulations
res <- doSimul(myCandidates, hgdpPlus, "outputs") # this can take hours (3 sim/minute)

## save(res.1.2, file="/home/master/dev/geograph/pkg/misc/simulations/res.1.2.RData")

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
points(hgdpPlus,col.node="blue") # show the NA -> these are pop from hgdp



##
## pairwise correlations
##

## compute pairwise physical distances (hgdpPlus)
# pairPath <- dijkstraBetween(hgdpPlus) # takes around 20 minutes

# save(pairPath, file="pairPath.RData")

## get pairwise distance
dist.phy <- sapply(pairPath[-length(pairPath)], function(e) sum(e$length_detail[[1]]) )


## shape as dist object
temp <- dist(1:76)


fst <- read.table("/home/master/dev/geograph/pkg/misc/HGDP/66pop.fst")
