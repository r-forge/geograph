##
## code to process monthly_gpp.out for geoGraph
##


## ! read.table won't read it all
txt <- scan("/home/master/dev/geograph/pkg/misc/productivity/monthly_gpp.out",what="numeric")
txt <- as.numeric(txt)

gpp <- matrix(txt, byrow=TRUE, ncol=4)
gpp <- as.data.frame(gpp)
names(gpp) <- c("lat","lon","month","gpp")
## myNodes <- closestNode(rawgraph.40k, gpp[,2:1]) # this takes ages

## save gpp
gpp$node <- myNodes
save(gpp, file="/home/master/dev/geograph/pkg/misc/productivity/gpp.RData")


## mean prod, var, range size
meanProd <- tapply(gpp$gpp, gpp$node, mean)
varProd <- tapply(gpp$gpp, gpp$node, var)
maxProd <- tapply(gpp$gpp, gpp$node, max)

## rangeProd <- tapply(gpp$gpp, gpp$node, function(e) diff(range(e)))
## minProd <- tapply(gpp$gpp, gpp$node, min)


data(worldgraph.40k)
worldgraph.40k@nodes.attr <- cbind.data.frame(worldgraph.40k@nodes.attr, meanProd, varProd)

## save(worldgraph.40k, file="/home/master/dev/geograph/pkg/data/worldgraph.40k.RData")
