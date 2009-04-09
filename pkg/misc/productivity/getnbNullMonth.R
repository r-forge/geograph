##
## code to extract number of shitty months
## (in terms of average productivity < threshold
##



## load gpp
load("/home/master/dev/geograph/pkg/misc/productivity/gpp.RData")
data(worldgraph.40k)

temp <- split(gpp, gpp$node)

myThres <- 1:50

base.res <- rep(12, length(getNodes(worldgraph.40k)))
names(base.res) <- getNodes(worldgraph.40k)
res <- list()

for(i in 1:length(myThres)){
    THRES <- myThres[i]
    toto <- sapply(temp, function(e) sum(e$gpp<THRES) / e$nbPts[1])

    res[[i]] <- base.res
    res[[i]][names(toto)] <- toto
}

res <- data.frame(res)
names(res) <- paste("nbMonthLessThan",myThres,sep="")
