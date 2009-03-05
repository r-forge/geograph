\encoding{UTF-8}
\name{connectivity-checks}
\alias{areNeighbours}
\alias{areConnected}
\alias{isConnected,gData-method}
\title{Check connectivity of a gGraph object}
\description{
  The functions \code{areNeighbours}, \code{areConnected} and the method
  \code{isConnected} test connectivity in different ways.\cr

  - \code{areNeighbours}: tests connectivity between couples of nodes on
  an object inheriting \code{graph} class (like a
  \linkS4class{graphNEL} object).\cr

  - \code{areConnected}: tests if a set of nodes form a connected set on
  a \linkS4class{gGraph} object.\cr

  - \code{isConnected}: tests if the nodes of a \linkS4class{gData}
  object form a connected set. Note that this is a method for
  \linkS4class{gData}, the generic being defined in the \code{graph}
  package.
}
\usage{
areNeighbours(V1, V2, graph)
areConnected(x, nodes)
isConnected(object, \dots)
}
\arguments{
  \item{\code{V1}}{a vector of node names}
  \item{\code{V2}}{a vector of node names}
  \item{\code{graph}}{a valid \linkS4class{graphNEL} object.}
  \item{\code{x}}{a valid \linkS4class{gGraph} object.}
  \item{\code{nodes}}{a vector of node names}
  \item{\code{object}}{a valid \linkS4class{gData} object.}
  \item{\code{\dots}}{other arguments passed to other methods (currently
    unused).}
}
\value{
  - \code{areNeighbours}: a vector of logical, having one value for each
  couple of nodes.\cr

  - \code{areConnected}: a single logical value, being TRUE if nodes
    form a connected set.\cr

  - \code{isConnected}: a single logical value, being TRUE if nodes
    of the object form a connected set.\cr
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\keyword{utilities}
\keyword{methods}