\name{Intervals_full-class}
\docType{class}
\alias{Intervals_full-class}
\alias{[,Intervals_full-method}
\alias{coerce,Intervals,Intervals_full-method}
\alias{coerce,Intervals_full,Intervals-method}
\alias{initialize,Intervals_full-method}

\title{Class "Intervals_full" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Intervals_full", .Data, type, closed)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{closed}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{type}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Intervals_virtual}"}, directly.
Class \code{"\linkS4class{matrix}"}, by class "Intervals_virtual", distance 2.
Class \code{"\linkS4class{array}"}, by class "Intervals_virtual", distance 3.
Class \code{"\linkS4class{structure}"}, by class "Intervals_virtual", distance 4.
Class \code{"\linkS4class{vector}"}, by class "Intervals_virtual", distance 5, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Intervals_full")}: ... }
    \item{coerce}{\code{signature(from = "Intervals", to = "Intervals_full")}: ... }
    \item{coerce}{\code{signature(from = "Intervals_full", to = "Intervals")}: ... }
    \item{initialize}{\code{signature(.Object = "Intervals_full")}: ... }
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
showClass("Intervals_full")
}
\keyword{classes}
