\encoding{UTF-8}
\name{closestNode}
\alias{closestNode}
\alias{closestNode-methods}
\alias{closestNode,gGraph-method}
\title{Find the closest node to a given location}
\description{
  The function \code{closestNode} searches for the closest node in a
  \linkS4class{gGraph} or a \linkS4class{gData} object to a given
  location. It is possible to restrain the research to given values of a
  node attribute. For instance, one can search the closest node on land
  to a given location.\cr

  This function is also used to match locations of a \linkS4class{gData}
  object with nodes of the \code{gGraph} object to which it is linked.
}
\usage{
closestNode(x, loc, zoneSize=5, attr.name=NULL, attr.values=NULL)
}
\arguments{
  \item{\code{x}}{a valid \linkS4class{gGraph} or \linkS4class{gData}
    object. In the latter case, the \linkS4class{gGraph} to which the
    \linkS4class{gData} is linked has to be in the current environment.}
  \item{\code{loc}}{locations, specified as a list with two components indicating longitude and
    latitude of locations. Alternatively, this can be a data.frame or a
    matrix with longitude and latitude in columns, in this order. Note
    that \code{locator()} can be used to specify interactively the locations.}
  \item{\code{zoneSize}}{a numeric value indicating the size of the zone
    (in latitude/longitude units) where the closest node is searched
    for. Note that this only matters for speed purpose: if no closest
    node is found inside a given zone, the zone is expanded until nodes
    are found.}
  \item{\code{attr.names}}{the optional name of a node attribute. See details.}
  \item{\code{attr.values}}{an optional vector giving values for \code{attr.names}. See details.}
}
\value{
  If \code{x} is a \linkS4class{gGraph} object: a vector of node
  names.\cr

  If \code{x} is a \linkS4class{gData} object: a \linkS4class{gData}
  object with matching nodes stored in the \code{@nodes.id} slot. Note
  that previous content of \code{@nodes.id} will be erased.\cr
}
\seealso{
  \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} to interactively add or remove edges in a
  \linkS4class{gGraph} object.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{

}
\keyword{utilities}
\keyword{methods}