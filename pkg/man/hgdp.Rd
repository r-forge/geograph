\encoding{UTF-8}
\name{hgdp}
\alias{hgdp}
\alias{hgdpPlus}
\docType{data}
\title{Human genome diversity panel - georeferenced data}
\description{
  The datasets \code{hgdp} and \code{hgdpPlus} provides genetic diversity several human
  populations worldwide. Both datasets are \linkS4class{gData} objects,
  interfaced with the \linkS4class{gGraph} object
  \code{\link{worldgraph.40k}}.\cr

  \code{hgdp} describes 52 populations from the original Human Genome
  Diversity Panel. \cr

  \code{hgdpPlus} describes \code{hgdp} populations plus 24 native
  American populations. \cr
}
\usage{
data(hgdp)
data(hgdpPlus)
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
