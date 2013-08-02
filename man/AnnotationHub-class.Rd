\name{AnnotationHub-objects}
\docType{class}

% Classes
\alias{AnnotationHub}
\alias{class:AnnotationHub}
\alias{AnnotationHub-class}

% Accessor-like methods
\alias{show,AnnotationHub-method}

\alias{$,AnnotationHub-method}

\alias{[[,AnnotationHub-method}
\alias{[[,AnnotationHub,ANY,ANY-method}

\alias{filters} % only if we have the generic here do we do this
\alias{filters,AnnotationHub-method}
\alias{filters<-}
\alias{filters<-,AnnotationHub-method}

\alias{hubUrl} 
\alias{hubUrl,AnnotationHub-method}
\alias{hubUrl,missing-method}

\alias{hubCache}
\alias{hubCache,AnnotationHub-method}
\alias{hubCache,missing-method}
\alias{hubCache<-}
\alias{hubCache<-,AnnotationHub-method}
\alias{hubCache<-,missing-method}
\alias{hubCache<-,AnnotationHub,character-method}


\alias{hubResource}   % Do we really want to export this?
\alias{hubResource,AnnotationHub-method}
\alias{hubResource,missing-method}

\alias{columns}
\alias{columns,AnnotationHub-method}

\alias{keytypes}
\alias{keytypes,AnnotationHub-method}

\alias{keys}
\alias{keys,AnnotationHub-method}

\alias{length,AnnotationHub-method}

\alias{metadata,AnnotationHub-method}
\alias{metadata,missing-method}

\alias{names,AnnotationHub-method}

\alias{possibleDates}
\alias{possibleDates,AnnotationHub-method}
\alias{possibleDates,missing-method}

\alias{snapshotDate}
\alias{snapshotDate,AnnotationHub-method}
\alias{snapshotDate,missing-method}
\alias{snapshotDate<-}
\alias{snapshotDate<-,AnnotationHub-method}

\alias{snapshotPaths}
\alias{snapshotPaths,AnnotationHub-method}
\alias{snapshotPaths,missing-method}

\alias{snapshotUrl}
\alias{snapshotUrl,AnnotationHub-method}
\alias{snapshotUrl,missing-method}

\alias{snapshotUrls}
\alias{snapshotUrls,AnnotationHub-method}

\alias{snapshotVersion}
\alias{snapshotVersion,AnnotationHub-method}
\alias{snapshotVersion,missing-method}

% % another way to DL
% \alias{getResource}
% \alias{getResource,AnnotationHub-method}


% help
\alias{ahinfo}
  


\title{AnnotationHub objects and their related methods and functions}

\description{
  \code{AnnotationHub} is the base class for interacting with the
  AnnotationHub services using Bioconductor.  When using the
  \code{AnnotationHub} package, users will create an instance of this
  class and then apply filters on it in order to narrow down their
  annotation options to an amount that a human can reasonably look at.

  Once an \code{AnnotationHub} is created, the user can see what
  resources are available by tab completion using the \code{$}
  argument.  But before doing that, it is usually a good idea to use the
  \code{filters} method to set some restrictions.

  What kind of restrictions?  Well those can be listed by using the
  \code{columns} and \code{keys} methods to list them.  Possible values for
  those can be found by using \code{keys}.

  Once this is determined, a list object can be assigned using
  \code{filters<-}.  The list needs to be named after values returned by
  the \code{columns} and \code{keys} methods, and to contain character
  vectors that contain results from the matching \code{keys} method.

  All \code{AnnotationHub} have a snapshot date that by default is set
  to the most recent one.  The \code{snapshotDate} method indicates
  which one is in use, but this value can also be set to previous dates.

  Whenever you download a file from \code{AnnotationHub} it will
  automatically put the data in a local cache for future reference. The
  location of this cache can be found and even changed with the
  \code{hubCache} setters and getters.  This cache provides a performance
  boost for users but it does not mean that you can use this on a plane
  without needing wireless access.  
}

\usage{
  filters(x, ...)
  filters(x, ...) <- value
  hubUrl(x, ...)
  hubCache(x, ...)
  hubCache(x, ...) <- value
  hubResource(x, path = character(), ...)
  possibleDates(x, ...)
  snapshotDate(x, ...)
  snapshotDate(x, ...) <- value
  snapshotUrl(x, ...)
  snapshotUrls(x, ...)
  snapshotVersion(x, ...)
  keys(x, keytype, ...)
  columns(x)
  keytypes(x)
  ahinfo(x, path, returnMeta=FALSE)
}

\arguments{
  \item{x}{the \code{AnnotationHub} object. }
  \item{value}{the value to be assigned.  For filters method, this is a
  named list where the names are the kind of filters, and the values are
  character vectors of acceptable values}
  \item{path}{a path string}
  \item{keytype}{the type of key you want to look up possible values
    for}
  \item{returnMeta}{return metadata as DataFrame (instead of printing it)}
  \item{...}{other arguments}
}


\author{Marc Carlson}


\examples{
  ## create an AnnotationHub object
  library(AnnotationHub)
  ah = AnnotationHub()

  ## what is the version of this snapshot?
  snapshotVersion(ah)

  ## and what is the date we are using?
  snapshotDate(ah)

  ## how many resources?
  length(ah)

  ## list currently active filters
  filters(ah)

  ## list values that can be used to filter on:
  columns(ah)
  keytypes(ah)

  ## list possible values for one of these filter types
  head(keys(ah, keytype="Species"))

  ## OR retrieve metadata values about several keys at once
  ## (This approach may not always scale the way you want it to)
  metadata(ah, columns = c("Species","RDataPath"))

  ## create and apply a new filter to only include people
  filters(ah) <- list(Species="Homo sapiens")

  ## now how many resources are there?
  length(ah)

  ## what are the names for these resources?
  head(names(ah))

  ## What are the URLs for these resources?
  head(snapshotUrls(ah))

  ## what web service is this AnnotationHub pointing to?
  hubUrl()
  ## and more explicitly
  snapshotUrl()

  ## Where are the files that get downloaded being cached?
  ## (there is also a setter if you wish to assign this to another location)
  hubCache(ah)


  ## Download a resource (using tab completion) and put it into "res"
  res <- ah$goldenpath.hg19.encodeDCC.wgEncodeUwTfbs.wgEncodeUwTfbsMcf7CtcfStdPkRep1.narrowPeak_0.0.1.RData

  ## extract paths another way (more friendly for programming):
  res <- ah[["goldenpath.hg19.encodeDCC.wgEncodeUwTfbs.wgEncodeUwTfbsMcf7CtcfStdPkRep1.narrowPeak_0.0.1.RData"]]

  ## Get metadata information about a resource.
  ahinfo(ah,"goldenpath.hg19.encodeDCC.wgEncodeUwTfbs.wgEncodeUwTfbsMcf7CtcfStdPkRep1.narrowPeak_0.0.1.RData")

}


\keyword{classes}
\keyword{methods}

