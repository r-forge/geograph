file.name<-"C10242.global"

##Data directory
data.dir<-c("../0.Data/")

setwd(paste(data.dir,file.name,sep=""))
read.csv(paste(file.name,".center.coord.csv",sep=""))->Coord
Coord$long[Coord$long>180]<-Coord$long[Coord$long>180]-360


#################################################################
####Define which points are on land
#################################################################
find.land<-function(Long,Lat) {
  ##This functions automatically assigns to land all points overlapping the country polygons

  require(maptools)
  ##Load country shapefile
  world<-readShapePoly("E:/Docs/Pictures/Maps/Arcview_shapefiles/world/country.shp",force_ring=T)
  n.country<-length(world@polygons)

  ##create land vector to score land
  land<-rep(0,length(Lat))

  for (i in 1:n.country) {
    this.country<-world@polygons[i][[1]]
    n.polys<-length(this.country@Polygons)
 
    for (p in 1:n.polys) {
       this.poly<-this.country@Polygons[p][[1]]
      land<-land+point.in.polygon(Long,Lat, this.poly@coords[,1], this.poly@coords[,2])  
    }
  }
  land[land>1]<-1
  return(land)
}

land<-find.land(Coord$long,Coord$lat)
land[land==0]<-"sea"
land[land==1]<-"land"
land<-as.data.frame(matrix(matrix(land,ncol=1),dimnames=list(NULL,"habitat")))

write.csv(land,paste(file.name,".habitat.v1.csv",sep=""))  ##this version 1, as it is automatically generated

