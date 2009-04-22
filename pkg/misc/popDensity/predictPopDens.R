##
## CODE TO MODEL POPULATION DENSITY
##


load("/home/master/dev/geograph/pkg/misc/popDensity/huntGathDens.RData")
data(worldgraph.40k)
dat <- getNodesAttr(huntGathDens)


## response variable : log(density)
ldens <- log(huntGathDens@data$density)

myR2 <- sapply(11:ncol(dat), function(i) summary(lm(ldens ~ dat[,i]))$r.squared)

which.max(myR2) # threshold = 15 is the best


## retained models
lat <- huntGathDens@coords[,2]
hemis <- factor(lat<0)
levels(hemis) <- c("north","south")

lm1 <- lm(ldens ~ meanProd + nbMonthLessThan15 + habitat + hemis, data=dat)

summary(lm1)
anova(lm1)
plot(lm1)
