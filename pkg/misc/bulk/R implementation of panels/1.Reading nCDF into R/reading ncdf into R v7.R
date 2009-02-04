file.name<-"C10242.global"

##Data directory
data.dir<-c("E:/Docs/Projects/Genetics/Isolation_by_distance/R implementation of panels/0.Data/")
setwd(paste(data.dir,"Original data files",sep=""))

##################################################################################################
#####Code to extract data from the netCDF file


library(ncdf)
library(fields)

open.ncdf(paste(file.name,".nc",sep=""))->mygrid
print(mygrid)
center.lat<-get.var.ncdf(mygrid,"grid_center_lat")
center.long<-get.var.ncdf(mygrid,"grid_center_lon")

corner.lat<-get.var.ncdf(mygrid,"grid_corner_lat")
corner.long<-get.var.ncdf(mygrid,"grid_corner_lon")

library(fields)
rad.to.deg<-function(x) {
	x*180/pi
}

deg.to.rad<-function(x) {
	x*pi/180
}


center.lat<-rad.to.deg(center.lat)
center.long<-rad.to.deg(center.long)
center.coord<-cbind(center.long,center.lat)
dimnames(center.coord)[[2]]<-c("long","lat")

corner.lat<-rad.to.deg(corner.lat)
corner.long<-rad.to.deg(corner.long)


##################################################################################################
#####A Few Useful Functions

###Function to find neighbours of a polygon
find.neighbours<-function(id,corner.coord) {
	match.ref<-function(x) {x %in% unique(corner.coord[,id])}
	corner.in.common<-apply(corner.coord,MARGIN=2,match.ref)
	corner.in.common<-colSums(corner.in.common)
	c((1:length(corner.in.common))[corner.in.common==2],(1:length(corner.in.common))[corner.in.common==3]) ##pentagons can have 3 corners in common becuase of the repeated corner in the grid
}

##A generic function that finds an hexagon with three corner in common with two reference hexagons
##This can return more than one hexagon, so we need to allow for the exlclusiong of some

corners.in.common<-function(id1,id2,corner.coord,corner.long,corner.lat,exclude.list=NULL) {
		corners.ref<-unique(as.vector(corner.coord[,c(id1,id2)]))
		match.ref<-function (x) { sum(unique(as.vector(x)) %in% corners.ref)}
		corner.in.common<-apply(corner.coord,MARGIN=2,match.ref)
		chosen.hexagons<-(1:length(corner.in.common))[corner.in.common==3]
		##now find the one we want, excluding the wrong ones
		chosen.hexagons[!chosen.hexagons %in% exclude.list]
}


##A generic function that finds the next polygon (hexagon or pentagon) in a line given two reference hexagons (finds 3rd polygon in row of 1,2,3)
next.polygon<-function(id1,id2,corner.coord,corner.long,corner.lat,exclude.list=NULL) {
		##first we find the two corners in common
		id1.corners<-unique(corner.coord[,id1])
		id2.corners<-unique(corner.coord[,id2])
		common.corners<-id2.corners %in% id1.corners
		if (length(id2.corners)==6) {
			common.corners<-cbind(corner.long[,id2][common.corners],corner.lat[,id2][common.corners])
			## now find the two corners opposite to these
			corner.dist<-rdist.earth(common.corners,cbind(corner.long[,id2],corner.lat[,id2]))
			max.dist<-apply(corner.dist,MARGIN=1,max)  ##max.dist
			corner1<-match(max.dist[1],corner.dist[1,])
			corner2<-match(max.dist[2],corner.dist[2,])
			##now find hexagon that contains these two corners (there will be two of those, id2 and id3
			match.ref<-function(x) {sum(unique(as.vector(x)) %in% corner.coord[c(corner1,corner2),id2])}
			corner.in.common<-apply(corner.coord,MARGIN=2,match.ref)
			chosen.hexagons<-(1:length(corner.in.common))[corner.in.common==2]
			chosen.hexagons<-chosen.hexagons[!chosen.hexagons %in% c(exclude.list,id2)]
		} else { chosen.hexagons<-NA} ##there is not next polygon when we have a pentagon!
		chosen.hexagons
}



##################################################################################################
######Code to extract 5 panels (called "layers" in the code)

##create unique id for each corner, with logitude given by digits left to the decimal point, and latitude by digits to the right of point
corner.coord<- round(corner.long*10000000)+round(corner.lat/1000,10)

##create array to contain panels
##for a grid of 2562, we need 5 layers of 16*32
##for a grid of 10242, we need 5 layers of 32*64
##for a grid of 40962, we need 5 layers of 64*128
N.hexagons<-dim (corner.coord)[2]
N.layers<-5
N.rows<-sqrt(((N.hexagons-2)/N.layers)/2)
N.cols<-N.rows*2

rect.layers<-array(dim=c(N.rows,N.cols,N.layers))

##first find the pentagons, which will have to be handled in a special way
count.vertices<-function(x) {length(unique(x))}
n.vertices<-apply(corner.coord,MARGIN=2,count.vertices)
id.pentagons<-(1:length(n.vertices))[n.vertices==5]
rm(n.vertices)


##find five neighbours to north pole: the "1st" hexagons of each layer
id.N.1st<-find.neighbours(1,corner.coord)
##order 1st hexagons them E to W
id.N.1st<-id.N.1st[order(center.coord[id.N.1st,1])]
coord.N.1st<-center.coord[id.N.1st,]



	
for (l in 1:length(id.N.1st)){

	#the first hexagon in a layer
	rect.layers[1,1,l]<-id.N.1st[l] ##this will have to be changed to add an extra row and column for the "ghosts"
	#the second hexagon on the first row is
	rect.layers[1,2,l]<-next.polygon(1,id.N.1st[l],corner.coord,corner.long,corner.lat,exclude.list=c(1))
	
	##find two reference hexagons from the five "1st" hexagons (the cornerstone for this layer and the one from the next layer moving E)
	if (l>1) {
		sub.id.N.1st<-id.N.1st[c(l,l-1)]
	} else {
		sub.id.N.1st<-id.N.1st[c(1,5)]
	}

	#the first hexagon on the second row in a layer is the one with 3 corners in common to the two cornerstones (excluding the N pole)
	rect.layers[2,1,l]<-corners.in.common(sub.id.N.1st[1],sub.id.N.1st[2],corner.coord,corner.long,corner.lat,exclude.list=c(1))
	
	#the second hexagon on the second row is the one with 3 corners in common with [1,2] and [2,1], which is not [1,1]
	rect.layers[2,2,l]<-corners.in.common(rect.layers[1,2,l],rect.layers[2,1,l],corner.coord,corner.long,corner.lat,exclude.list=c(1,rect.layers[1,1,l]))

	#now fill up the first two column
	for (n in 1:2) {
		for (r in 3:(N.rows)) {
			rect.layers[r,n,l]<-next.polygon(rect.layers[r-2,n,l],rect.layers[r-1,n,l],corner.coord,corner.long,corner.lat,exclude.list=c(1))
		}
	}

	##now fill up all rows but the first one, which contains a pentago, and thus is problematic
	for (r in 2:(N.rows)) {
		for (n in 3:N.cols) {
			rect.layers[r,n,l]<-next.polygon(rect.layers[r,n-2,l],rect.layers[r,n-1,l],corner.coord,corner.long,corner.lat,exclude.list=c(1))
		}
	}
	
	##now fill up the first row
	for (n in 3:N.cols) {
		rect.layers[1,n,l]<-next.polygon(rect.layers[3,n,l],rect.layers[2,n,l],corner.coord,corner.long,corner.lat,exclude.list=c(1))
	}
}


##test that all cells have been used
used.cells<-as.vector(rect.layers)
##We did not use the following cells:
c(1:N.hexagons)[!(1:N.hexagons %in%used.cells)]
##only the two poles should be left, 1 & 2


##################################################################################################
######Split the orignal 5 panels into 10 panels
library(abind)
rect.layers<-abind(rect.layers[,1:(dim(rect.layers)[2]/2),],rect.layers[,((dim(rect.layers)[2]/2)+1):dim(rect.layers)[2],])


setwd(paste(data.dir,file.name,sep=""))

write.csv(rect.layers,paste(file.name,".10.layers.csv",sep=""),row.names=F)
dimnames(center.coord)[[1]]<-paste("V.",1:length(center.coord[,1]),sep="")
write.csv(center.coord,paste(file.name,".center.coord.csv",sep=""))

