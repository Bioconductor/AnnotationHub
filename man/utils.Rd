\name{utils}
\alias{utils}

\alias{DispatchClassList}

\title{
  Utility function to list currently available dispatchClass. 
}

\description{
   When submitting resources to AnnotationHub or ExperimentHub a valid
   DispatchClass field must be specified in the
   inst/extdata/metadata.csv file for each resource.  This list the
   currently available DispatchClass values and briefly how that class
   loads a resource. If your resource does not qualify for one of these
   methods contact Lori Shepherd \email{Lori.Shepherd@RoswellPark.org}
   to request a new DispatchClass be added
}

\examples{

  DispatchClassList()
}
\keyword{utilities}