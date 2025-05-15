\name{utilities}
\alias{utilities}

\alias{listResources}
\alias{loadResources}


\title{
  Utility functions for discovering package-specific Hub resources.
}

\description{
  List and load resources from ExperimentHub filtered by package
  name and optional search terms. Methods not implemented for
  AnnotationHub.
}

\details{
  Currently \code{listResources} and \code{loadResources} are only meaningful
  for \code{ExperimentHub} objects.
}

\examples{
    getGeneric("listResources")
    getGeneric("loadResources")
}

\keyword{utilities}
