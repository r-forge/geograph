\name{gData-class}
\docType{class}
\alias{gData-class}
\alias{[,gData-method}
\alias{closestNode,gData-method}
\alias{dijkstraBetween,gData-method}
\alias{dijkstraFrom,gData-method}
\alias{findInLayer,gData-method}
\alias{getCoords,gData-method}
\alias{getData,gData-method}
\alias{getNodes,gData-method}
\alias{initialize,gData-method}
\alias{isInArea,gData-method}
\alias{points,gData-method}
\alias{show,gData-method}

\title{Class "gData" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gData", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{nodes.id}:}{Object of class \code{"character"} ~~ }
    \item{\code{data}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{gGraph.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{gGraph.version}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "gData")}: ... }
    \item{closestNode}{\code{signature(x = "gData")}: ... }
    \item{dijkstraBetween}{\code{signature(x = "gData")}: ... }
    \item{dijkstraFrom}{\code{signature(x = "gData")}: ... }
    \item{findInLayer}{\code{signature(x = "gData")}: ... }
    \item{getCoords}{\code{signature(x = "gData")}: ... }
    \item{getData}{\code{signature(x = "gData")}: ... }
    \item{getNodes}{\code{signature(x = "gData")}: ... }
    \item{initialize}{\code{signature(.Object = "gData")}: ... }
    \item{isInArea}{\code{signature(x = "gData")}: ... }
    \item{points}{\code{signature(x = "gData")}: ... }
    \item{show}{\code{signature(object = "gData")}: ... }
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
showClass("gData")
}
\keyword{classes}
