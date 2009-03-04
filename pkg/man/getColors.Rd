\encoding{UTF-8}
\name{getColors}
\alias{getColors}
\alias{getColors-methods}
\alias{getColors,gGraph-method}
\title{Get colors associated to edges of a gGraph object}
\description{
  The function \code{getColors} returns the colors associated to the nodes of a
  \linkS4class{gGraph} object, based on a specified node attribute.
}
\usage{
getColors(x, nodes="all", attr.name, ...)
}
\arguments{
  \item{\code{x}}{a valid \linkS4class{gGraph}.}
  \item{\code{nodes}}{a vector of character strings or of integers
    identifying nodes by their name or their index. Can be "all", in
    which case all nodes are considered.}
  \item{\code{\dots}}{other arguments passed to other methods (currently
    unused).}
}
\value{
  A vector of characters being valid colors.\cr
}
\details{
  Colors are based on a node attribute, that is, on a column of the
  \code{nodes.attr} data.frame. This attribute should have a finite
  number of values, and would most likely be a factor. Correspondence
  between values of this variable and colors must be provided in the
  \code{@meta$color} slot. See example section to know how this slot
  should be designed.
  
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
## 
data(worldgraph.10k)

worldgraph.10k # there is a node attribute 'habitat'
worldgraph.10k@meta$color

head(getNodes(worldgraph.10k))
head(getColors(worldgraph.10k,mode="vector", attr.name="habitat"))

}
\keyword{utilities}
\keyword{methods}