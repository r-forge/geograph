\encoding{UTF-8}
\name{hgdp}
\alias{hgdp}
\docType{data}
\title{Human genome diversity panel - georeferenced data}
\description{
  The dataset \code{hgdp} contains microsatellite data for several human
  populations worldwide. \code{hgdp} is a \linkS4class{gData} object,
  interfaced with the \linkS4class{gGraph} object \code{\link{worldgraph.40k}}.
}
\usage{
data(hgdp)
}
\format{
  \code{hgdp} is a \linkS4class{gGraph} object with the following data:
%   \describe{
%     \item{@nodes.attr$habitat}{habitat corresponding to each
%       vertice; currently 'land' or 'sea'.}
%     \item{@meta$color}{a matrix assigning a color for plotting
%       vertices (second column) to different values of habitat (first
%       column).}	
%   }
}
\references{
 Authors \emph{Journal}, YEAR, \bold{nb}: pp-pp.
}
\examples{

data(hgdp)
data(worldgraph.40k)
hgdp

## plotting the object
plot(hgdp)

}
\keyword{datasets}
