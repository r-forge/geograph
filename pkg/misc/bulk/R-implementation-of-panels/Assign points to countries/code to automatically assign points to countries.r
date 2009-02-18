rm(list=ls())
read.csv("sample.pop.locations.csv")->Coord
##condition coordinates to be from -180 to +180
names(Coord)<-c("Pop","lat","long")
Coord$long[Coord$long>180]<-Coord$long[Coord$long>180]-360



#################################################################
####Define which points are on land
#################################################################
find.country<-function(Long,Lat) {
  ##This functions automatically assigns points to countries

  require(maptools)
  ##Load country shapefile
  world<-readShapePoly("C:/Andrea/Docs/Pictures/Maps/Arcview_shapefiles/world/country.shp",force_ring=T)
  n.country<-length(world@polygons)

  ##create land vector to score land
  countries<-rep(NULL, length(Long))

  for (i in 1:n.country) {
    this.country<-world@polygons[i][[1]]
    n.polys<-length(this.country@Polygons)
    points.in.this.country<-rep(0, length(Long))
    for (p in 1:n.polys) {
      this.poly<-this.country@Polygons[p][[1]]
      points.in.this.country<-points.in.this.country+point.in.polygon(Long,Lat, this.poly@coords[,1], this.poly@coords[,2])
    }
    countries[points.in.this.country>0]<-as.character(world@data$CNTRY_NAME[i])

    
  }
  return(countries)
}


Coord$Country<-find.country(Coord$long,Coord$lat)