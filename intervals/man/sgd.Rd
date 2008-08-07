\name{sgd}

\alias{sgd}

\docType{data}

\title{Yeast gene model information}

\description{
  This data set contains a data frame describing a subset of
  the chromosome feature data represented in Fall 2007 version of
  \code{saccharomyces_cerevisiae.gff}, available for download from the
  \emph{Saccharomyces} Genome Database (\url{www.yeastgenome.org}). 
}

\usage{data(sgd)}

\format{
  A data frame with 14080 observations on the following 8 variables.
  \describe{
    \item{\code{SGDID}}{SGD feature ID.}
    \item{\code{type}}{
      Only four feature types have been retatined: \code{CDS},
      \code{five_prime_UTR_intron}, \code{intron}, and \code{ORF}. Note
      that \emph{ORF} correspond to a whole gene while \code{CDS}, to an
      exon. \emph{S. cerevisae} does not, however, have many
      multi-exonic genes.
    }
    \item{\code{feature_name}}{A character vector}
    \item{\code{parent_feature_name}}{
      The \code{feature_name} of the a larger element to which the
      current feature belongs. All retained \code{CDS} entries, for
      example, belong to an \code{ORF} entry.
    }
    \item{\code{chr}}{
      The chromosome on which the feature occurs.
    }
    \item{\code{start}}{Feature start base.}
    \item{\code{stop}}{Feature stop base.}
    \item{\code{strand}}{Is the feature on the Watson or Crick strand?}
  }
}

\examples{
data(sgd)
## maybe str(sgd) ; plot(sgd) ...
}

\keyword{datasets}
