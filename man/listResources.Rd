\name{utilities}
\alias{utilities}

\alias{listResources}
\alias{listResources,AnnotationHub-method}
\alias{loadResources}
\alias{loadResources,AnnotationHub-method}


\title{
  Utility functions for discovering package-specific Hub resources.
}

\description{
  List and load resources from ExperimentHub filtered by package
  name and optional search terms.
}

\details{
  Currently \code{listResources} and \code{loadResources} are only meaningful
  for \code{ExperimentHub} objects. Methods for \code{AnnotationHub} 
  objects may be implemented in the future.
}

\usage{
listResources(hub, package, filterBy = character())
loadResources(hub, package, filterBy = character())
}

\arguments{
  \item{hub}{
    A \code{Hub} object; currently only meaningful for \code{ExperimentHub}.
  }
  \item{package}{
    A \code{character(1)} name of a package with resources hosted in the Hub.
  }
  \item{filterBy}{
    A \code{character()} vector of search terms for additional filtering. 
    Can be any terms found in the metadata (mcols()) of the resources. 
    When not provided, there is no additional filtering and all resources 
    associated with the given package are returned.
  }
}

\value{
  \code{listResources} returns a character vector; 
  \code{loadResources} returns a list of data objects.
}

\seealso{
}

\examples{
\dontrun{
## Packages with resources hosted in ExperimentHub:
require(ExperimentHub)
eh <- ExperimentHub()
unique(package(eh))

## All resources associated with the 'GSE62944' package:
listResources(eh, "GSE62944")

## Resources associated with the 'curatedMetagenomicData' package
## filtered by 'plaque.abundance':
listResources(eh, "curatedMetagenomicData", "plaque.abundance")

## 'loadResources()' returns a list of the data objects:
loadResources(eh, "curatedMetagenomicData", "plaque.abundance")
}
}
\keyword{utilities}
