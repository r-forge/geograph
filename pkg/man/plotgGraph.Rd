\name{plot-gGraph}
\docType{methods}
\alias{plot,gGraph-method}
\alias{plot.gGraph}
\alias{points,gGraph-method}
\alias{points.gGraph}
\alias{plotEdges,gGraph-method}
\alias{plotEdges.gGraph}
\title{ Plot a gGraph object. }
\description{
  Various functions to plot a \linkS4class{gGraph} object: \code{plot}
  opens a device and plot the object, while \code{points} plots the object on
  the existing device. \code{plotEdges} only plots the edges of the
  graph: it can be called directly, or via arguments passed to
  \code{plot} and \code{points}.\cr

  Plotting of a gGraph object stores some parameters in R; see details
  for more information.
}
\usage{
  \S4method{plot}{gGraph}(x, shape="world", psize=NULL, pch=19,
  col=NULL, edges=FALSE, reset=FALSE, bg.col="gray", border.col="dark
  gray", lwd=1, useWeights=NULL, maxLwd=3, attr.col=NULL, \dots)

 \S4method{points}{gGraph}(x, psize=NULL, pch=NULL, col=NULL,
 edges=FALSE, lwd=1, useWeights=NULL, maxLwd=3, attr.col=NULL,
 sticky.points=FALSE,\dots)

\S4method{plotEdges}{gGraph}(x, replot=TRUE, useWeights=NULL,
col="black", lwd=1, lty=1, pch=NULL, psize=NULL, pcol=NULL, maxLwd=3,
attr.col=NULL,\dots)

}
\arguments{
  \item{\code{x}}{a \linkS4class{gGraph} object.}
  \item{\code{shape}}{a shapefile used as background to the object. Must
    be of the class \code{SpatialPolygonsDataFrame}
    (see \code{readShapePoly} in maptools package to import such data
    from a GIS shapefile). Alternatively, a character string indicating
    one shapefile released with geoGraph.}
  \item{\code{psize}}{a numeric giving the size of points.}
  \item{\code{pch}}{a numeric or a character indicating the type of point.}
  \item{\code{col}}{a character string indicating the color to be used.}
  \item{\code{edges}}{a logical indicating if edges should be plotted (TRUE) or
    not (FALSE).}
  \item{\code{reset}}{a logical indicating if plotting parameters should
    be reset (TRUE) or not (FALSE).}
  \item{\code{bg.col}}{a character string indicating the color of the
    polygons of the shapefile used as background.}
  \item{\code{border.col}}{a character string indicating the color of the
    polygon borders.}
  \item{\code{lwd}}{a numeric indicating the width of line (used for edges).}
  \item{\code{useWeights}}{a logical indicating if edge width should be
    proportionnal to edge weights or not.}
  \item{\code{maxLwd}}{a numeric indicating the maximum edge width
    (corresponding to the maximum weight).}
  \item{\code{attr.col}}{a data.frame with two named columns, the first
    one giving values of a node attribute, and the second one stating
    colors to be used for each value. If not provided, this is seeked
    from the \code{@meta$color} slot of the object.}
  \item{\code{sticky.points}}{a logical indicating if added points should
    be kept when replotting (TRUE), or not (FALSE). In any case,
    \code{reset=TRUE} will prevent points to be redrawn.}
  \item{\code{replot}}{a logical indicating if points should be redrawn
    after plotting the edges (TRUE), or not (FALSE).}
  \item{\code{lty}}{the type of line (for the edges).}
  \item{\code{pcol}}{a character indicating the color to be used for points.}
  \item{\code{\dots}}{further arguments passed to the generic methods
    (plot, points, and segments, respectively).}
  %  \item{\code{}}{.}
  %   \item{\code{}}{.}
  %   \item{\code{}}{.}
  %   \item{\code{}}{.}
}
\details{
  To be able to zoom in and out, or slide the window, previous plotting
  information are stored in a particular environment (.geoGraphEnv),
  which is created when loading \code{geoGraph}. Users should not have
  to interact directly with objects in this environment.\cr

  The resulting plotting behaviour is that when plotting a \code{gGraph}
  object, last plotting parameters are re-used. To override this
  behaviour, specify \code{reset=TRUE} as argument to \code{plot}.
}
\seealso{
  - Different functions to explore these plots:\cr
  \code{\link{geo.zoomin}}, \code{\link{geo.zoomout}},
  \code{\link{geo.slide}}, \code{\link{geo.back}}.\cr

  - \code{\link{isInArea}}, to retain a set of visible data.\cr
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
data(worldgraph.10k)

## just the background
plot(worldgraph.10k,reset=TRUE,type="n")

## basic plot
plot(worldgraph.10k)

## zooming and adding edges
geo.zoomin(list(x=c(90,150),y=c(0,-50)))
plot(worldgraph.10k, edges=TRUE)

## display edges differently
plotEdges(worldgraph.10k, col="red", lwd=2)

## replot points with different color
points(worldgraph.10k, col="orange")

## mask points in the sea
inSea <- unlist(getNodesAttr(worldgraph.10k,attr.name="habitat"))=="sea"
head(inSea)
points(worldgraph.10k[inSea], col="white")

## but better, only draw those on land, and use a fancy setup
par(bg="blue")
plot(worldgraph.10k[!inSea], bg.col="darkgreen", col="white", edges=TRUE)

}

\keyword{methods}
\keyword{hplot}
\keyword{spatial}
