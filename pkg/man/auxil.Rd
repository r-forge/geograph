\encoding{UTF-8}
\name{Auxiliary methods}
\alias{hasCosts}
\title{Auxiliary methods for geoGraph}
\description{
  These methods are low-level functions called by other procedures of
  \code{geoGraph}. Some can, however, be useful in themselves. Note that
  unlike other functions in \code{geoGraph}, these functions do not
  generally test for the validity of the provided arguments (for speed
  purposes).\cr

  - \code{hasCosts}: tests whether a \linkS4class{gGraph} has costs
  associated to its edges.
}
\usage{
hasCosts(x)
}
\arguments{
  \item{\code{x}}{a valid \linkS4class{gGraph}.}
}
\value{
   A logical .
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
data(worldgraph.10k)
hasCosts(worldgraph.10k)
}
\keyword{utilities}
\keyword{methods}