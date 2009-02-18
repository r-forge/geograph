rm(list=ls())
file.name<-"C40962.global"
v<-2	##version of land definitions

##################################################################################################
###### Read in panels
as.matrix(read.csv(paste(file.name,".10.layers.csv",sep="")))->temp.panels
n.col<-dim(temp.panels)[1]
dimnames(temp.panels)[[2]]<-rep(1:n.col,10)
rect.layers<-array(NA,dim=c(n.col,n.col,10))

for (p in 1:10)
	rect.layers[,,p]<-temp.panels[,(1+(p-1)*n.col):(p*n.col)]
rm(p,temp.panels)



##################################################################################################
######Assign hexagons to land

##Read land assignments (which was done in ArcView)
library(foreign)
read.dbf(paste(file.name,".land.",v,".dbf",sep=""))->land
n.land<-length(land$ID)
rect.layers[!(rect.layers %in% land$ID)]<-NA


#####DEBUG DEBUG BETTER NOT TO RECODE NOW???
rect.layers[(rect.layers %in% land$ID)]->orig.ID
rect.layers[(rect.layers %in% land$ID)]<-0:(n.land-1)
	


##################################################################################################
######Create coordinates for centers on land
	read.csv(paste(file.name,".center.coord.csv",sep=""))->center.coord
	center.coord[land$ID,]->center.coord

	
##################################################################################################
###### Glue panels together

##if we ever use the North and South pole, we might have to change the following, but this is not likely
N.pole<-NA
S.pole<-NA

N.rows<-dim(rect.layers)[1]+2
N.cols<-dim(rect.layers)[2]+2
N.layers<-dim(rect.layers)[3]
glued.layers<-array(NA,dim=c(N.rows,N.cols,N.layers))

#####a modified modulus 5 function to sort out glueing of first five (top) layers
mod5<-function(x) {
	y<-x%%5
	if (y==0) {y=5}
	y
}


for (l in 1:10) {
	##fill up the core part of the layer
	glued.layers[(2:(N.rows-1)),(2:(N.cols-1)),l]<-rect.layers[,,l]
	if (l<6) {
		##1st column is the 1st row of mod5(l-1) layer
		glued.layers[3:N.rows,1,l]<-rect.layers[1,,mod5(l-1)] ##do we need last element????
		##1st row is the 1st column of mod5(l+1) layer
		glued.layers[1,2:(N.cols-1),l]<-rect.layers[,1,mod5(l+1)]
		##last column is the 1st column of mod5(l)+5
		glued.layers[2:(N.rows-1),N.cols,l]<-rect.layers[,1,mod5(l)+5]
		##last row is the 1 row of mod5(l-1)+5
		glued.layers[N.rows,2:(N.cols-1),l]<-rect.layers[1,,mod5(l-1)+5]
		##add the North Pole
		glued.layers[2,1,l]<-N.pole
		
	} else {
		##1st column is the last column of (l-5)
		glued.layers[2:(N.rows-1),1,l]<-rect.layers[,N.cols-2,(l-5)]
		##1st row is last row of (l%%5+1)
		glued.layers[1,2:(N.cols-1),l]<-rect.layers[N.rows-2,,(l%%5+1)]
		##last column is the last row of (l%%5+1+5)
		glued.layers[2:(N.rows-1),N.cols,l]<-rect.layers[N.rows-2,,(l%%5+1+5)]
		##last row is the last column of (mod5(l-1)+5)
		glued.layers[N.rows,1:(N.cols-2),l]<-rect.layers[,N.cols-2,(mod5(l-1)+5)]
		##add the south pole
		glued.layers[N.rows,N.cols-1,l]<-S.pole
	}
}
		
		
		
##correct<-matrix(ncol=10,nrow=4)
##for (l in 1:10) {
##		correct[1,l]<-glued.layers[3,1,l]==next.polygon( glued.layers[3,3,l] , glued.layers[3,2,l],corner.coord,corner.long,corner.lat,exclude.list=c(1))
##		correct[2,l]<-glued.layers[1,3,l]==next.polygon( glued.layers[3,3,l] , glued.layers[2,3,l],corner.coord,corner.long,corner.lat,exclude.list=c(1))
##		correct[3,l]<-glued.layers[3,18,l]==next.polygon( glued.layers[3,16,l] , glued.layers[3,17,l],corner.coord,corner.long,corner.lat,exclude.list=c(1))
##		correct[4,l]<-glued.layers[18,3,l]==next.polygon( glued.layers[16,3,l] , glued.layers[17,3,l],corner.coord,corner.long,corner.lat,exclude.list=c(1))
##}
##correct
##all should be true if the layers are "glued together" properly


##################################################################################################
###### Create list of neighbours
neighbours<-matrix(ncol=2,nrow=0)

for (p in 1:10) {
	for (col in 2:(n.col+1)) {
		for (row in 2:(n.col+1)) {  ############IS THIS A BUG, SHOULD IT BE N.ROW?????????????????????????????????????????????
			if(!is.na(glued.layers[row,col,p])){
				id<-(glued.layers[row,col,p])
				id.neig<-c(glued.layers[row+1,col,p],glued.layers[row-1,col,p],glued.layers[row,col+1,p],glued.layers[row,col-1,p],glued.layers[row+1,col-1,p],glued.layers[row-1,col+1,p])
				id.neig<-id.neig[!is.na(id.neig)]
				neighbours<-rbind(neighbours,cbind(rep(id,length(id.neig)),id.neig))
			}
		}
	}
}
dimnames(neighbours)[[2]]<-c("id1","id2")

##

##################################################################################################
######write code to remove special neighbours (done on original coordinates, so that the changes can be moved down to future version with different number of vertices 
remove.joint<-matrix(ncol=2,nrow=0)
remove.joint<-cbind(remove.joint,c(NA,NA))  ##this is a blank example




##################################################################################################
######create list of unique neighbours to be used to create a graph
unique.neig<-neighbours
unique.neig[(unique.neig[,1]>unique.neig[,2]),]<-cbind(unique.neig[(unique.neig[,1]>unique.neig[,2]),2],unique.neig[(unique.neig[,1]>unique.neig[,2]),1])
unique.neig<-unique(unique.neig)

##################################################################################################
######Check that all cells can be reached from all other cells using graph theory
library(igraph)
ngraph<-graph.edgelist(unique.neig,directed=FALSE)
nshort<-as.vector(shortest.paths(ngraph,0))
if (max(nshort)==length(V(ngraph))) {
cat("THE GRAPH IS NOT FULLY CONNECTED\n")
cat("EXTRACT UNCONNECTED VERTICES:\n")
V(ngraph)[nshort==max(nshort)]
}



##################################################################################################
######Now write out files, taking care of index numbers for use with C
write.csv(neighbours,paste(file.name,".neighbours.csv",sep=""),row.names=F)







