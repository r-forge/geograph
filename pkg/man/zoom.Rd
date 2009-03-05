\encoding{UTF-8}
\name{geo.zoomin}
\alias{geo.zoomin}
\alias{geo.zoomout}
\alias{geo.slide}
\alias{geo.back}
\alias{.zoomlog.up}
\title{Navigate in the plot of a gGraph object}
\description{
  The functions \code{geo.zoomin}, \code{geo.zoomout}, \code{geo.slide},
  and \code{geo.back} are used to navigate interactively in the plot of
  a \linkS4class{gGraph} object.\cr

  \code{geo.zoomin} and \code{geo.zoomout} are used to zoom in and
  out. For zooming in, the user has to delimit the opposite corner of
  the new plotting area; alternatively, a set of coordinates can be
  provided. For zooming out, each click on the screen will
  zoom out further.\cr

  \code{geo.slide} moves the window toward the direction indicated by
  clicking in the screen.\cr

  \code{geo.back} redraws previous plots each time screen is clicked.\cr

  \code{.zoomlog.up} is an auxiliary function used to update the zoom
  log, by providing new sets of coordinates.

  Whenever clicking is needed, a right-click will stop the function.
}
\usage{
geo.zoomin(reg=NULL)
geo.zoomout()
geo.slide()
geo.back()
.zoomlog.up(vec)
}
\arguments{
  \item{reg}{a list of length 2, with its first component being
    the new x (longitude) boundaries (a vector of length 2), and its second being new y
    (latitude) boundaries (a vector of length 2).}
  \item{vec}{a numeric vector of length 4 giving the new
    coordinates of the plotting window, in the order: xmin, xmax, ymin, ymax.}
}
\seealso{
  \code{\link{plot.gGraph}} for plotting of a \linkS4class{gGraph} object.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
data(worldgraph.10k)
plot(worldgraph.10k, reset=TRUE)

## zooming in
x.ini <- c(-100,-60)
y.ini <- c(-30,30)
for(i in 0:3){
geo.zoomin(list(x=x.ini + i*60, y=y.ini))
}


\dontrun{
## going back
geo.back() # you have to click !

## zooming in interactively
geo.zoomin() # you have to click !

## zooming out
geo.zoomout() # you have to click !

## moving window
geo.slide() # you have to click !
}
}
\keyword{utilities}
\keyword{hplot}
