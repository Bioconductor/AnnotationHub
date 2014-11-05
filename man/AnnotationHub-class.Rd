\name{AnnotationHub-objects}
\docType{class}

% Classes
\alias{AnnotationHub}
\alias{class:AnnotationHub}
\alias{AnnotationHub-class}

% Accessor-like methods
\alias{show,AnnotationHub-method}
\alias{show,AnnotationHubResource-method}

\alias{$,AnnotationHub-method}

\alias{[[,AnnotationHub,character,missing-method}
\alias{[[,AnnotationHub,numeric,missing-method}

\alias{[,AnnotationHub,character,missing-method} 
\alias{[,AnnotationHub,logical,missing-method} 
\alias{[,AnnotationHub,numeric,missing-method} 

% used for replacing ranges - give an example
\alias{[<-,AnnotationHub,character,missing,AnnotationHub-method} 
\alias{[<-,AnnotationHub,logical,missing,AnnotationHub-method} 
\alias{[<-,AnnotationHub,numeric,missing,AnnotationHub-method} 


\alias{mcols,AnnotationHub-method} 

\alias{cache}
\alias{cache<-}

\alias{subset,AnnotationHub-method}

\alias{query}
\alias{query,AnnotationHub-method}

\alias{length,AnnotationHub-method}

\alias{names,AnnotationHub-method}

\alias{hubUrl} 
\alias{hubCache}

\alias{possibleDates}

\alias{snapshotDate}
\alias{snapshotDate,AnnotationHub-method}
\alias{snapshotDate<-}
\alias{snapshotDate<-,AnnotationHub-method}

\alias{display}
\alias{display,AnnotationHub-method}



\title{AnnotationHub objects and their related methods and functions}

\description{
  \code{AnnotationHub} is the base class for interacting with the
  AnnotationHub services using Bioconductor.  When using the
  \code{AnnotationHub} package, users will create an instance of this
  class and then subset it down to those resources that are of interest.

  Once an \code{AnnotationHub} is created users can extract data by using 
  the subset operators (along with an assignment operator to put the contents 
  of one of these resources into a local variable).  Basically you just need to 
  look at the contents of the \code{AnnotationHub} object using grep or a 
  display method until you have decided which records you want to retrieve and 
  then you can use the \code{[[} operator to retrieve the relevant records. 
  Using the \code{[} operator will just subset the \code{AnnotationHub} object 
  so that it refers to fewer records.

  All \code{AnnotationHub} have a snapshot date that by default is set
  to the most recent one.  The \code{snapshotDate} method indicates
  which one is in use, but this value can also be set to previous dates.
  Other dates can be accessed with the \code{possibleDates} function.

  Whenever you download a file from \code{AnnotationHub} it will
  automatically put the data in a local cache for future reference. The
  location of this cache can be found and even changed with the
  \code{hubCache} setters and getters.  This cache provides a performance
  boost for users but it does not mean that you can use this on a plane
  without needing wireless access.  
}

\usage{
AnnotationHub(..., hub=hubOption("URL"), cache=hubOption("CACHE"),
    max.downloads=hubOption("MAX_DOWNLOADS"))
cache(x, ..., max.downloads)
cache(x, ...) <- value
hubUrl(x)
hubCache(x)
possibleDates(x)
\S4method{mcols}{AnnotationHub}(x)
\S4method{snapshotDate}{AnnotationHub}(x)
\S4method{snapshotDate}{AnnotationHub}(x) <- value
\S4method{query}{AnnotationHub}(x, pattern, ...)
\S4method{subset}{AnnotationHub}(x, resource_table, tags, ...)
\S4method{display}{AnnotationHub}(object)
}

\arguments{

  \item{x, object}{The \code{AnnotationHub} object.}

  \item{value}{The value to be assigned.}

  \item{pattern}{A character regular expression to query metadata of
    \code{x}.}

  \item{subset}{An expression referring to columns of
    \code{metadata(x)}. See Methods for additional detail.}

  \item{resource_table}{An expression that indicates which part of the
    data set you want to keep indicated as a subset expression.}

  \item{hub}{The URI where the online hub metadata is stored.}

  \item{cache}{the local cache where the data is stored after being
    downloaded or updated.}

  \item{max.downloads}{The maximum number of items to try and download
    without prompting.  Used as a safety net to prevent users from
    trying to cache the entire hub.}

  \item{tags}{Tags used to label the initial object.}
  
  \item{...}{Additional arguments.}
}


\section{Methods}{
  In the code snippets below, \code{x} is an AnnotationHub object.
  \describe{
    \item{}{
      \code{AnnotationHub()}:
      Constructs an AnnotationHub object.
    }      
    \item{}{
      \code{hubUrl(x)}:
      Gets the URL for the online hub.
    }      
    \item{}{
      \code{hubUrl(x)}:
      Gets the path to the local hub cache.
    }      
    \item{}{
      \code{possibleDates(x, ...)}:
      Lists dates for snapshots that the hub could potentially use.
    }
    \item{}{
      \code{snapshotDate(x)} and \code{snapshotDate(x, ...) <- value}:
      Gets or sets the date for the snapshot in use.
    }
    \item{}{
      \code{cache(x, ...)} and \code{cache(x, ...) <- value}: Adds
      (downloads) all resources in \code{x}, or (caution!) removes all
      local resources from the cache.
    }      

    \item{}{
      \code{query(x, pattern, ...)}:
      Return an AnnotationHub subset containing only those elements
      whose metadata matches \code{pattern}. Matching uses
      \code{pattern} as in \code{\link{grepl}} to search the
      \code{as.character} representation of each column, performing a
      logical \code{OR} across columns. Arguments \code{...} are passed
      to \code{grepl} for within-column matching.}
    \item{}{
      \code{subset(x, subset, ...)}:
      Return an AnnotationHub subset containing only those elements
      whose metadata satisfies the \emph{expression} in
      \code{subset}. The expression can reference columns of
      \code{metadata(x)}, and should return a logical vector of length
      \code{length(x)}.}
    \item{}{
      \code{display(object, ...)}:
      Implements the display generic from interactiveDisplay package.
      Users can select metadata rows from a web interface, the values in
      the rows selected by the user become filters applied to the
      AnnotationHub object.
    }
    \item{}{
      \code{mcols(x)}:
      returns a DataFrame with metadata for the \code{AnnotationHub} object.
    }      
}
}

\author{Martin Morgan, Marc Carlson and Dan Tenenbaum}


\examples{
  ## create an AnnotationHub object
  library(AnnotationHub)
  ah = AnnotationHub()

  ## and what is the date we are using?
  snapshotDate(ah)

  ## how many resources?
  length(ah)

  ## what are the names for these resources?
  head(names(ah))

  ## what web service and local cache does this AnnotationHub point to?
  hubUrl(ah)
  hubCache(ah)

  ## query and subset
  ahs <- query(ah, 'inparanoid8') 
  ahs <- subset(ah, ah$genome=='ailMel1')

  ## We can extract a result by name like this (using a list semantic):
  res <- ahs[[1]]
  ## And we can also extract it by the names like this:
  res <- ahs[["AH2"]]

  ## And we can also use "[" to restrict the things that are in the
  ## AnnotationHub object (by position, character, or logical vector).
  ## Here is a demo of position:
  subHub <- ah[1:3]


  if(interactive()) {
    ## Display method involves user interaction through web interface
    ah2 <- display(ah)
  }
}



\keyword{classes}
\keyword{methods}

