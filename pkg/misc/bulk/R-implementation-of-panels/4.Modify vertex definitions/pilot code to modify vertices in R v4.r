##KEY VARIABLES TO MODIFY
file.name<-"C10242.global"
habitat.ver<-1 ##version file for the habitat variables
##END OF KEY VARIABLES TO MODIFY

############
##Definition scheme for vertices (to be expanded if needed)
habitat.details<-matrix(ncol=3,nrow=0,dimnames=list(NULL,c("habitat","colour","crossable")))
habitat.details<-rbind(habitat.details,c("land","green","y"))       ##land = normal land, easy to cross
habitat.details<-rbind(habitat.details,c("sea","blue","n"))         ##sea = uncrossable
habitat.details<-rbind(habitat.details,c("mountain","brown","n"))   ##mountain = above 2000 meters, not crossable
habitat.details<-rbind(habitat.details,c("landbridge","light green","y"))   ##landbridge = sea which was crossable in the past
habitat.details<-rbind(habitat.details,c("oceanic.crossing","light blue","y"))  ##oceanic.crossing = sea which might have been crossed to reach an oceanic island

habitat.details<-as.data.frame(habitat.details)
 



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
names(coord.centers)[1]<-"id"

##We need a vertex.habitat file that contains the habitat definition of vertices
read.csv(paste(file.name,".habitat.v",habitat.ver,".csv",sep=""),row.names=1)->vertex.habitat
vertex.habitat$habitat<-factor(vertex.habitat$habitat,levels=levels(habitat.details$habitat))   ##create all appropriate levels for logical operators


##read in edges object
load(paste(file.name,".edgeList.R.object",sep=""))

##We need a removed.edges file that contains edges that should be removed from the graph
read.csv(paste(file.name,".removed.edges.csv",sep=""),row.names=1)->removed.edges


                    


##object to store zoom logs to be able to zoom out
zoom.log<-matrix(c(-180,180,-90,90),ncol=4,dimnames=list(NULL,c("x1","x2","y1","y2")))
psize.def<-2


##Plotting functions (it would be nice to upgrade to iplot when it becomes stable enough

# default plot shows all the data (ideally we should reduce the margin)
plot.world<-function(psize=psize.def){
  n.obs<-dim(zoom.log)[1]
	plot(world,col="gray",border="dark gray",xlim=sort(zoom.log[n.obs,1:2]),ylim=sort(zoom.log[n.obs,3:4]))
	for (i in 1:dim(habitat.details)[1]) {
	 points(coord.centers[vertex.habitat$habitat==habitat.details[i,1],2:3],pch=19,cex=psize,col=as.character(habitat.details[i,2]))
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
   if (!habitat.type %in% as.character(habitat.details$habitat)) {
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
       vertex.habitat$habitat[spoint]<<-habitat.type
       
       ##replot the point with its new colour
       points(coord.centers[spoint,2:3],pch=19,cex=psize.def,col=as.character(habitat.details[habitat.details$habitat==habitat.type,2]))

      }
    }
  }
}


##Add a function to plot edges
plot.edges<-function()  {
 ##first we need to select only points which are not in the current zoomed view
  displayed.vertices<-subset(coord.centers,long>min(par("usr")[1:2]))
  displayed.vertices<-subset(displayed.vertices,long<max(par("usr")[1:2]))
  displayed.vertices<-subset(displayed.vertices,lat>min(par("usr")[3:4]))
  displayed.vertices<-subset(displayed.vertices,lat<max(par("usr")[3:4]))
  ##now cycle through selected vertices and plot
  for (i in 1:(dim(displayed.vertices)[1])) {
     id1<-as.numeric(row.names(displayed.vertices)[i])
     connected.vertices<-edgeList[[as.character(displayed.vertices$id[i])]]$edges
     id1.cross<-habitat.details[habitat.details$habitat==vertex.habitat$habitat[id1],3]
     if (id1.cross=="y") {
        for (j in 1:length(connected.vertices)) {
          id2<-connected.vertices[j]      
          id2.cross<-habitat.details[habitat.details$habitat==vertex.habitat$habitat[id2],3]
            if (id2.cross=="y") {
                segments(coord.centers$long[id1],coord.centers$lat[id1],coord.centers$long[id2],coord.centers$lat[id2],lwd=2)
            }
        }
     }   
  }
  ##plot removed edges differently
  if (dim(removed.edges)[1]>0) {
    for (i in 1:dim(removed.edges)[1]) {
      segments(coord.centers$long[removed.edges[i,1]],coord.centers$lat[removed.edges[i,1]],coord.centers$long[removed.edges[i,2]],coord.centers$lat[removed.edges[i,2]], col="red",lwd=2)
    }
  }
  
  ##replot the poinst to make the figure prettier
	for (i in 1:dim(habitat.details)[1]) {
	 points(coord.centers[vertex.habitat$habitat==habitat.details[i,1],2:3],pch=19,cex=psize.def,col=as.character(habitat.details[i,2]))
  }  
}




##Function to remove special edges (e.g. to prevent crossing of straits)
remove.edges<-function() {
    spoint<-c(1,2)
    while (length(spoint)>1) {
      spoint<-NULL
      spoint<-identify(coord.centers$long,coord.centers$lat,plot=F,n=2)
      if(length(spoint)>1) {
        segments(coord.centers$long[spoint[1]],coord.centers$lat[spoint[1]],coord.centers$long[spoint[2]],coord.centers$lat[spoint[2]],col="red",lwd=2) 
        points(coord.centers$long[spoint[1]],coord.centers$lat[spoint[1]],pch=19,cex=psize.def,col=as.character(habitat.details[habitat.details$habitat== vertex.habitat$habitat[spoint[1]],2]))
        points(coord.centers$long[spoint[2]],coord.centers$lat[spoint[2]],pch=19,cex=psize.def,col=as.character(habitat.details[habitat.details$habitat== vertex.habitat$habitat[spoint[1]],2]))
        
        removed.edges[dim(removed.edges)[1]+1,]<<-sort(c(as.numeric(row.names(coord.centers)[spoint[1]]),as.numeric(row.names(coord.centers)[spoint[2]])))
      }
    }
    ##remove multiple instances of the same edge
    removed.edges<<-unique(removed.edges)
}



##Function to remove special edges
readd.edges<-function() {
    spoint<-c(1,2)
    while (length(spoint)>1) {
      spoint<-NULL
      spoint<-identify(coord.centers$long,coord.centers$lat,plot=F,n=2)
      if(length(spoint)>1) {
        segments(coord.centers$long[spoint[1]],coord.centers$lat[spoint[1]],coord.centers$long[spoint[2]],coord.centers$lat[spoint[2]],lwd=2) 
        points(coord.centers$long[spoint[1]],coord.centers$lat[spoint[1]],pch=19,cex=psize.def,col=as.character(habitat.details[habitat.details$habitat== vertex.habitat$habitat[spoint[1]],2]))
        points(coord.centers$long[spoint[2]],coord.centers$lat[spoint[2]],pch=19,cex=psize.def,col=as.character(habitat.details[habitat.details$habitat== vertex.habitat$habitat[spoint[1]],2]))
        
        ##now remove this pair from the removed edges
        removed.edges<<-removed.edges[(removed.edges[,1]==spoint[1]) + (removed.edges[,2]==spoint[2])<2,]
      }
    }
}




################################################
###EXAMPLE ON HOW TO USE THE PLOTTING FUNCTIONS
################################################
plot.world()   ##plot the world
zoomin.world() ## select the two opposite corners of the area you want to select
zoomout.world() ## go back to prevous zoom level
zoomin.world()  ##now zoom in
change.vertices("land")
remove.edges()
readd.edges()






####################################################
##WE CAN NOW CONDITION THE GRAPH BASED ON THE RULES
####################################################
friction.details<-habitat.details[,-2]
friction.details$friction<-c(1,0,0,1,1)


edgeList.friction<-edgeList
##vertex.friction<-numeric(dim(coord.centers)[1])
vertex.friction<-rep(NA,dim(friction.details)[1])
for (i in 1:dim(friction.details)[1]) {
  vertex.friction[vertex.habitat$habitat==friction.details$habitat[i]]<-friction.details$friction[i]
}
if (sum(is.na(vertex.friction))>0) {
  cat("There is a problem with the classification system\n")
}

for (i in 1:dim(coord.centers)[1]) {
  if (vertex.friction[i]>0) {
    neighbours<-edgeList.friction[[coord.centers$id[i]]]$edges
  edgeList.friction[[coord.centers$id[i]]]$weights<-edgeList.friction[[coord.centers$id[i]]]$weights*(vertex.friction[neighbours]+vertex.friction[i])/2
  ##subset the above to only contain edges bigger than zero
   edgeList.friction[[coord.centers$id[i]]][[2]]<-edgeList.friction[[coord.centers$id[i]]][[2]][edgeList.friction[[coord.centers$id[i]]][[2]]>0]
   edgeList.friction[[coord.centers$id[i]]][[1]]<-edgeList.friction[[coord.centers$id[i]]][[1]][edgeList.friction[[coord.centers$id[i]]][[2]]>0]
  } else {
    edgeList.friction[[coord.centers$id[i]]]<-list(edges=NULL,weights=NULL)
  }
}
###We now need to spot check a few vertices neighbouring sea to check that the appropriate edges have been removed





library(RBGL)
gR.friction <- new("graphNEL", nodes=rownames(coord.centers), edgeL=edgeList.friction)






##things to add
##FOR THE 2D, WE NEED A FUNCTION TO CHECK THAT ALL LAND VERTICES ARE CONNECTED TO EACH OTHER. WE THEN BUILD A NEIGHBOUR LIST OF ONLY THE LAND VERTICES


