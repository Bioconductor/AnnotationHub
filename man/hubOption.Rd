\name{hubOption}
\alias{hubOption}
\alias{setHubOption}

\title{Set or get options for default hub behavior.}

\description{

  These functions get or set options for creation of new
  \sQuote{AnnotationHub} instances.

}

\usage{
hubOption(arg)
setHubOption(arg, value)
}

\arguments{

  \item{arg}{The character(1) hub options to set. see \sQuote{Details}
    for current options.}

  \item{value}{The (scalar) value to be assigned to the hub option.}

}

\details{
  Supported options include:

  \describe{

    \item{\dQuote{URL}:}{character(1). The base URL of the annotation
      hub. Default: \url{https://annotationhub.bioconductor.org}}

    \item{\dQuote{CACHE}:}{character(1). The location of the hub
      cache. Default: \dQuote{.AnnotationHub} in the user home
      directory.}

    \item{\dQuote{MAX_DOWNLOADS}:}{numeric(1). The integer number of
      downloads allowed before triggering an error. This is to help
      avoid accidental download of a large number of AnnotationHub
      members.}
  }

  Default values may also be determined by system and global R
  environment variables visible \emph{before} the package is loaded. Use
  options or variables preceeded by \dQuote{ANNOTATION_HUB_}, e.g.,
  \code{options(ANNOTATION_HUB_MAX_DOWNLOADS=10)} prior to package load
  sets the default number of downloads to 10. 

}

\value{The requested or successfully set option.}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\examples{
hubOption("URL")
\dontrun{
setHubOption("CACHE", "~/.myHub")
}
}
\keyword{ manip }
