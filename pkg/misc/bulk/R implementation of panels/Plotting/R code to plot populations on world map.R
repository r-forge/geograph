###Code to plot the world and locations
file.name<-"C40962.global"
library(maptools)
readShapePoly("E:/Docs/Pictures/Maps/Arcview_shapefiles/world/country.shp",force_ring=T)->world

read.csv(paste(file.name,".center.coord.csv",sep=""))->coord.centers
coord.centers$long[coord.centers$long>180]<-coord.centers$long[coord.centers$long>180]-360
read.csv("C40962.global.land.1.csv")->vertex.attr


plot(world,col="gray",border="dark gray")
points(coord.centers$long[vertex.attr$land==0],coord.centers$lat[vertex.attr$land==0],pch=19,cex=0.5,col="blue")
points(coord.centers$long[vertex.attr$land==1],coord.centers$lat[vertex.attr$land==1],pch=19,cex=0.5,col="brown")






locations.file.name<-"grid.origins"
locations.file.name<-paste(locations.file.name,".pop.locations.txt",sep="")
locations<-read.table(locations.file.name,header=T)

library(maptools)
readShapePoly("E:/Docs/Pictures/Maps/Arcview_shapefiles/world/country.shp",force_ring=T)->world
plot(world,col="gray",border="dark gray")

points(locations$LONGITUDE,locations$LATITUDE,pch=19,cex=0.5,col="blue")


##highlight specific popoulation
pop.focus<-36
points(locations$LONGITUDE[pop.focus],locations$LATITUDE[pop.focus],pch=19,cex=0.5,col="red")