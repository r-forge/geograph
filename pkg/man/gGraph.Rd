\name{gGraph-class}
\docType{class}
\alias{gGraph-class}
\alias{[,gGraph-method}
% \alias{closestNode,gGraph-method}
% \alias{dijkstraBetween,gGraph-method}
% \alias{dijkstraFrom,gGraph-method}
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
% \alias{plot,gGraph,missing-method}
% \alias{points,gGraph-method}
\alias{setEdges,gGraph-method}
\alias{show,gGraph-method}
\alias{is.gGraph}
\title{Formal class "gGraph"}
\description{The class \code{gGraph} is a formal (S4) class storing geographic
  data.\cr
  Such data are composed of a set of geographic coordinates of vertices
  (or 'nodes'), and a graph describing connectivity between these
  vertices. Data associated to the nodes can also be stored ('nodes attributes'), as well as
  meta-information used when plotting the object, or when computing
  weights associated to the edges based on nodes attributes.\cr
  History associated to a \code{gGraph} object is stored in the slot
  \code{history}, as an object of the class \linkS4class{gGraphHistory}.
}
\section{Objects from the class \code{gGraph}}{
  \code{gGraph} objects can be created by calls to \code{new}
  \code{new("gGraph", ...)}, where '...' can be the following arguments:
  \describe{
    \item{\code{coords}}{a matrix of spatial coordinates with two columns,
      being respectively longitude (from -180 to 180) and latitude. Positive
      numbers are intended as 'east' and 'north', respectively.}
    \item{\code{nodes.attr}}{a data.frame whose rows are nodes, and whose
      columns are different variables associated to the nodes.}
    \item{\code{meta}}{a list, most likely containing named data.frames
      (see Slots).}
    \item{\code{graph}}{an object of the class \linkS4class{graphNEL},
      from the \code{graph} package (see \code{class?graphNEL}),
      describing connectivity among nodes.}
    \item{\code{history}}{an object of the class
      \linkS4class{gGraphHistory}, storing a previous history.}
    \item{\code{cmd}}{an unevaluated expression (see \code{?expression})
      passed to the \linkS4class{gGraphHistory} object.}
    \item{\code{comments}}{a character string passed to the
      \linkS4class{gGraphHistory} object, commenting the object creation.}
    %  \item{\code{}}{}
    %   \item{\code{}}{}
    %   \item{\code{}}{}
  }

  Note that none of these is mandatory: \code{new("gGraph")} would work, and
  create an empty \code{gGraph} object.
}
\section{Slots}{
	 \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"} storing
  geographic coordinates (see above).}
    \item{\code{nodes.attr}:}{A \code{"data.frame"}
  storing nodes data (see above).}
    \item{\code{meta}:}{A \code{"list"} containing meta information
      about how data in the \code{nodes.attr} slot should be used in
  different contexts. This information is to be stored as named
  data.frames. Currently recognized meta-information are:\cr
  - \code{$color}: a data.frame giving colors to be used when plotting
  the object, according to a node attribute taking a finite number of
  values (likely a factor). Each documented attribute is a column, the
  last one being reserved to (valid) named colors. See the
  \code{@meta$color} slot in \code{\link{worldgraph.10k}} for an
  example.\cr
  -  \code{$weights}: a data.frame giving weights to be used when computing
  edge weights using \code{\link{setFriction}}. The first column is a
  node attribute, and the second column gives corresponding weights. See the
  \code{@meta$weights} slot in \code{\link{worldgraph.10k}} for an
  example.}
    \item{\code{graph}:}{Object of class \linkS4class{"graphNEL"} (see
  \code{class?graphNEL} for more information).}
    \item{\code{history}:}{Object of class
  \linkS4class{"gGraphHistory"}, recording the history associated to the
  object.}
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
