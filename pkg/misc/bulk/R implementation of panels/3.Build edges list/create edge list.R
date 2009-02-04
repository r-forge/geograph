rm(list=ls())

file.name<-"C10242.global"

##Data directory
data.dir<-c("../0.Data/")
setwd(paste(data.dir,file.name,sep=""))


library(maptools)
##################################################################################################
##Read in center coordinates

read.csv(paste(file.name,".center.coord.csv",sep=""),row.names=1)->coord.centers
coord.centers$long[coord.centers$long>180]<-coord.centers$long[coord.centers$long>180]-360

n.vertices<-dim(coord.centers)[1]
##################################################################################################
###### Read in panels to build neighbours
as.matrix(read.csv(paste(file.name,".10.layers.csv",sep="")))->temp.panels
n.col<-dim(temp.panels)[1]
dimnames(temp.panels)[[2]]<-rep(1:n.col,10)
rect.layers<-array(NA,dim=c(n.col,n.col,10))

for (p in 1:10)
	rect.layers[,,p]<-temp.panels[,(1+(p-1)*n.col):(p*n.col)]
rm(p,temp.panels)



##################################################################################################
###### Glue panels together

##if we ever use the North and South pole, we might have to change the following, but this is not likely
N.pole<-1
S.pole<-2

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

##Now add the North Pole
glued.layers[2,1,1:5]<-N.pole
glued.layers[N.rows,N.cols-1,6:10]<-S.pole
		


##################################################################################################
###### Create list of neighbours
edgeList<- vector("list", length=n.vertices)
names(edgeList)<-rownames(coord.centers)

library(fields)
neighbours<-matrix(ncol=2,nrow=0)

for (p in 1:10) {
	for (col in 2:(n.col+1)) {
		for (row in 2:(n.col+1)) {
			if(!is.na(glued.layers[row,col,p])){
				id<-(glued.layers[row,col,p])
				id.neig<-c(glued.layers[row+1,col,p],glued.layers[row-1,col,p],glued.layers[row,col+1,p],glued.layers[row,col-1,p],glued.layers[row+1,col-1,p],glued.layers[row-1,col+1,p])
				id.neig<-id.neig[!is.na(id.neig)]
				##find distance
				this.neighbours.dist<-rdist.earth(as.matrix(coord.centers[id,]),as.matrix(coord.centers[id.neig,]),miles=F)[1,]
				##neighbours<-rbind(neighbours,cbind(rep(id,length(id.neig)),id.neig))
        edgeList[[id]] <- list(edges=id.neig, weights=this.neighbours.dist)
			}
		}
	}
}


##Now add the poles to the edge list
N.id.neig<-sort(c(glued.layers[3,1,1],glued.layers[2,2,1],glued.layers[1,2,1],glued.layers[1,2,2],glued.layers [1,2,3]))
N.neighbours.dist<-rdist.earth(as.matrix(coord.centers[N.pole,]),as.matrix(coord.centers[N.id.neig,]),miles=F)[1,]
edgeList[[N.pole]] <- list(edges=N.id.neig, weights=N.neighbours.dist)

S.id.neig<-sort(c(glued.layers[N.rows,N.cols-2,6],glued.layers[N.rows-1,N.cols-1,6],glued.layers[N.rows-1,N.cols,6],glued.layers[N.rows-1,N.cols,7],glued.layers[N.rows-1,N.cols,8]))
S.neighbours.dist<-rdist.earth(as.matrix(coord.centers[S.pole,]),as.matrix(coord.centers[S.id.neig,]),miles=F)[1,]
edgeList[[S.pole]] <- list(edges=S.id.neig, weights=S.neighbours.dist)



##Now, it time to create a graph and check that all verteces are connected
##(they will be by definition, but the test can then be repeated after changes in land characteristics)

##To install the bioconductor library to use the BOOST graph library
##source("http://bioconductor.org/biocLite.R")
##biocLite("RBGL")
library(RBGL)
gR <- new("graphNEL", nodes=rownames(coord.centers), edgeL=edgeList)
shortest.distances<-dijkstra.sp(gR)
max(shortest.distances$distances)
##max distance (above) should be a reasonable number (say less than 100000)


##Save the edgeList as an R object
save(edgeList,file=paste(file.name,".edgeList.R.object",sep=""))
##Create an empty file to remove edges
write.csv(matrix(ncol=2,nrow=0,dimnames=list(NULL,c("vertex1","vertex2"))),file=paste(file.name,".removed.edges.csv",sep=""))




