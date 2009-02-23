\name{gGraph-class}
\docType{class}
\alias{gGraph-class}
\alias{[,gGraph-method}
\alias{closestNode,gGraph-method}
\alias{dijkstraBetween,gGraph-method}
\alias{dijkstraFrom,gGraph-method}
\alias{dropWeights,gGraph-method}
\alias{findInLayer,gGraph-method}
\alias{findLand,gGraph-method}
\alias{getColors,gGraph-method}
\alias{getCoords,gGraph-method}
\alias{getDates,gGraph-method}
\alias{getEdges,gGraph-method}
\alias{getGraph,gGraph-method}
\alias{getHistory,gGraph-method}
\alias{getNodesAttr,gGraph-method}
\alias{getNodes,gGraph-method}
\alias{getWeights,gGraph-method}
\alias{initialize,gGraph-method}
\alias{isInArea,gGraph-method}
\alias{plot,gGraph,missing-method}
\alias{points,gGraph-method}
\alias{setEdges,gGraph-method}
\alias{show,gGraph-method}

\title{Class "gGraph" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gGraph", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{nodes.attr}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{meta}:}{Object of class \code{"list"} ~~ }
    \item{\code{graph}:}{Object of class \code{"graphNEL"} ~~ }
    \item{\code{history}:}{Object of class \code{"gGraphHistory"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "gGraph")}: ... }
    \item{closestNode}{\code{signature(x = "gGraph")}: ... }
    \item{dijkstraBetween}{\code{signature(x = "gGraph")}: ... }
    \item{dijkstraFrom}{\code{signature(x = "gGraph")}: ... }
    \item{dropWeights}{\code{signature(x = "gGraph")}: ... }
    \item{findInLayer}{\code{signature(x = "gGraph")}: ... }
    \item{findLand}{\code{signature(x = "gGraph")}: ... }
    \item{getColors}{\code{signature(x = "gGraph")}: ... }
    \item{getCoords}{\code{signature(x = "gGraph")}: ... }
    \item{getDates}{\code{signature(x = "gGraph")}: ... }
    \item{getEdges}{\code{signature(x = "gGraph")}: ... }
    \item{getGraph}{\code{signature(x = "gGraph")}: ... }
    \item{getHistory}{\code{signature(x = "gGraph")}: ... }
    \item{getNodesAttr}{\code{signature(x = "gGraph")}: ... }
    \item{getNodes}{\code{signature(x = "gGraph")}: ... }
    \item{getWeights}{\code{signature(x = "gGraph")}: ... }
    \item{initialize}{\code{signature(.Object = "gGraph")}: ... }
    \item{isInArea}{\code{signature(x = "gGraph")}: ... }
    \item{plot}{\code{signature(x = "gGraph", y = "missing")}: ... }
    \item{points}{\code{signature(x = "gGraph")}: ... }
    \item{setEdges}{\code{signature(x = "gGraph")}: ... }
    \item{show}{\code{signature(object = "gGraph")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
	or \code{\linkS4class{CLASSNAME}} for links to other classes
}
\examples{
showClass("gGraph")
}
\keyword{classes}
