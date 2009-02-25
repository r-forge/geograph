\name{dijkstra-methods}
\docType{methods}
\alias{dijkstraFrom}
\alias{dijkstraFrom-methods}
\alias{dijkstraFrom,gData-method}
\alias{dijkstraFrom,gGraph-method}
\alias{dijkstraBetween}
\alias{dijkstraBetween-methods}
\alias{dijkstraBetween,gData-method}
\alias{dijkstraBetween,gGraph-method}
\alias{gPath}
\alias{plot.gPath}
\title{ Shortest path using Dijkstra algorithm }
\description{ The methods \code{dijkstraFrom} and \code{dijkstraBetween}
  are wrappers of procedures implemented in RBGL package, designed for
  \linkS4class{gGraph} and \linkS4class{gData} object.\cr

  \code{dijkstraFrom} finds minimum costs paths to nodes from a given
  'source' node.\cr

  \code{dijkstraBetween} finds minimum costs paths between all possible
  pairs of nodes given two sets of nodes.\cr

  All these functions return objects with S3 class "gPath". These
  objects can be plotted using \code{plot.gPath}.
}
\usage{
  \S4method{dijkstraFrom}{gGraph}(x, start, weights="default")
  \S4method{dijkstraFrom}{gData}(x, start, weights="default")
  \S4method{dijkstraBetween}{gGraph}(x, from, to)
  \S4method{dijkstraBetween}{gData}(x)
  \method{plot}{gPath}(x, col="rainbow", lwd=3, \dots)
}
\arguments{
  \item{\code{x}}{a \linkS4class{gGraph} or a \linkS4class{gData}
    object.}
  \item{\code{start}}{a character string naming the 'source' node.}
  \item{\code{weights}}{an optional set of weights for the edges.}
  \item{\code{from}}{a vector of character strings giving node names.}
  \item{\code{to}}{a vector of character strings giving node names.}
  \item{\code{x}}{a \code{gPath} object.}
  \item{\code{col}}{a character string indicating a color or
    a palette of colors to be used for plotting edges.}
  \item{\code{lwd}}{a numeric value indicating the width of edges.}
}
\details{
  In 'dijkstraBetween', paths are seeked all possible pairs of
  nodes between 'from' and 'to'.
}
\value{
  A "gPath" object. These are basically the outputs of RBGL's
  \code{sp.between} function (see \code{?sp.between}), with a class
  attribute set to "gPath", and an additional slot 'xy' containing
  geographic coordinates of the nodes involved in the paths.\cr
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
\dontrun{
data(worldgraph.40k)
data(hgdp)

## plotting
world <- worldgraph.40k
par(mar=rep(.1,4))
plot(world, reset=TRUE)
geo.zoomin(reg=list(x=c(-10,100), y=c(-10,60)))

## subset data to plotted region
myPop <- hgdp[isInArea(hgdp)]
myPop
isConnected(myPop)

## remove disconnected data
toRemove <- match(c("26898","26917"),getNodes(myPop))
myPop <- myPop[-toRemove]
isConnected(myPop)

## refresh plot
plot(world,reset=TRUE)
geo.zoomin(reg=list(x=c(-10,100), y=c(-10,60)))
points(myPop, lwd=3)

## Lowest cost path from an hypothetical origin
ori.coord <- list(33,10) # one given location long/lat
points(data.frame(ori.coord), pch="x", col="black", cex=3) # an 'x' shows the putative origin
ori <- closestNode(world, ori.coord) # assign it the closest node

myPath <- dijkstraFrom(myPop, ori)

## plotting
plot(world,pch="") # plot the world
points(myPop, lwd=3) # plot populations
plot(myPath) # plot the path
points(data.frame(ori.coord), pch="x", col="black", cex=3) # add origin

}
}
\keyword{methods}
\keyword{spatial}
