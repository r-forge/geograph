##KEY VARIABLES TO MODIFY
file.name<-"C10242.global"
habitat.ver<-1 ##version file for the habitat variables
##END OF KEY VARIABLES TO MODIFY


##Data directory
data.dir<-c("../0.Data/")
setwd(paste(data.dir,file.name,sep=""))

##libraries needed
library(maptools)

##################################################################################################
##Read in informations

##Read map
readShapePoly("E:/Docs/Pictures/Maps/Arcview_shapefiles/world/world_merged_polygons.shp")->world

##read vertext coordinates
read.csv(paste(file.name,".center.coord.csv",sep=""))->coord.centers
coord.centers$long[coord.centers$long>180]<-coord.centers$long[coord.centers$long>180]-360

##We need a vertex.habitat file that contains the habitat definition of vertices
read.csv(paste(file.name,".habitat.v",habitat.ver,".csv",sep=""),row.names=1)->vertex.habitat

##read in edges object
load(paste(file.name,".edgeList.R.object",sep=""))->edgeList

##We need a removed.edges file that contains edges that should be removed from the graph


############
##Definition scheme for vertices (to be expanded if needed)
habitat.details<-as.data.frame(cbind(habitat=c("land","mountain","sea","landbridge","oceanic.crossing"),
                     colour=c("green","brown","blue","light green","light blue")),
                     passable=c("y","n","n","y","y"))
##land = normal land, easy to cross
##mountain = above 2000 meters
##sea = uncrossable
##landbridge = sea which was crossable in the past
##oceanic.crossing = sea which might have been crossed to reach an oceanic island


##object to store zoom logs to be able to zoom out
zoom.log<-matrix(c(-180,180,-90,90),ncol=4,dimnames=list(NULL,c("x1","x2","y1","y2")))
psize.def=0.5


##Plotting functions (it would be nice to upgrade to iplot when it becomes stable enough

# default plot shows all the data (ideally we should reduce the margin)
plot.world<-function(psize=psize.def){
  n.obs<-dim(zoom.log)[1]
	plot(world,col="gray",border="dark gray",xlim=sort(zoom.log[n.obs,1:2]),ylim=sort(zoom.log[n.obs,3:4]))
	for (i in 1:dim(habitat.details)[1]) {
	 points(coord.centers[vertex.habitat==habitat.details[i,1],],pch=19,cex=psize,col=as.character(habitat.details[i,2]))
  }
}


# define an interactive 'zoomin' function:
zoomin.world<-function(){
	reg<-locator(2)
  zoom.log<<-rbind(zoom.log,c(reg$x[1],reg$x[2],reg$y[1],reg$y[2]))
  plot.world()
}

zoomout.world<-function(){
  if (dim(zoom.log)[1]>1) {
    zoom.log<<-matrix(zoom.log[-(dim(zoom.log)[1]),],ncol=4,dimnames=list(NULL,c("x1","x2","y1","y2"))) ##make sure that zoom.log is not turned into a vector
    plot.world()
  } else {
    cat("Can not zoom out any further \n")
  }
}


## define a function to change selected points to a given type
#ideally this should be interactive and update every action we take
change.vertices<-function(habitat.type=NULL) {
   if (!habitat.type %in% as.character(habitat.details[,1])) {
    cat("The habitat you chose is not yet described in 'habitat.details'\n")
    return (0)
   }
   if (is.null(habitat.type)) {
    cat("You need to specify a habitat!\n")
   } else {
    spoint<-1
    while (length(spoint)>0) {
      spoint<-NULL
      spoint<-identify(coord.centers$long,coord.centers$lat,plot=F,n=1)
      if (length(spoint)>0) {
       vertex.habitat[spoint]<<-habitat.type
       plot.world()
       ##This can be more efficiently done by replotting a specific point rather than the whole graph
       ##	 points(coord.centers[spoints,],pch=19,cex=psize,col=as.character(habitat.details[i,2]))

      }
    }
  }
}


##Add a function to plot edges


##Add a function to remove special edges
