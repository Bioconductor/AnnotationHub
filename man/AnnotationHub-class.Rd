\name{AnnotationHub-objects}
\docType{class}

% Classes
\alias{class:AnnotationHub}
\alias{AnnotationHub-class}
\alias{class:Hub}
\alias{Hub-class}

% Constructor
\alias{.Hub}
\alias{AnnotationHub}

% Accessor-like methods
\alias{mcols,Hub-method} 

\alias{cache}
\alias{cache,Hub-method}
\alias{cache,AnnotationHub-method}
\alias{cache<-}
\alias{cache<-,Hub-method}

\alias{hubUrl} 
\alias{hubUrl,Hub-method} 
\alias{hubCache}
\alias{hubCache,Hub-method}
\alias{hubDate}
\alias{hubDate,Hub-method}
\alias{package}
\alias{package,Hub-method}
\alias{removeCache}
\alias{isLocalHub}
\alias{isLocalHub,Hub-method}
\alias{isLocalHub<-}
\alias{isLocalHub<-,Hub-method}

\alias{possibleDates}
\alias{snapshotDate}
\alias{snapshotDate,Hub-method}
\alias{snapshotDate<-}
\alias{snapshotDate<-,Hub-method}

\alias{dbconn,Hub-method}
\alias{dbfile,Hub-method}
\alias{.db_close}
\alias{recordStatus}
\alias{recordStatus,Hub-method}

% List-like
\alias{length,Hub-method}
\alias{names,Hub-method}
\alias{fileName,Hub-method}

% Subsetting:
\alias{$,Hub-method}

\alias{[[,Hub,character,missing-method}
\alias{[[,Hub,numeric,missing-method}

\alias{[,Hub,character,missing-method} 
\alias{[,Hub,logical,missing-method} 
\alias{[,Hub,numeric,missing-method} 

\alias{[<-,Hub,character,missing,Hub-method} 
\alias{[<-,Hub,logical,missing,Hub-method} 
\alias{[<-,Hub,numeric,missing,Hub-method} 

\alias{subset,Hub-method}

\alias{query}
\alias{query,Hub-method}

\alias{display}
\alias{display,Hub-method}

% as.list / c
\alias{as.list.Hub}
\alias{as.list,Hub-method}
\alias{c,Hub-method}

% show method:
\alias{show,Hub-method}
\alias{show,AnnotationHub-method}
\alias{show,AnnotationHubResource-method}


\title{AnnotationHub objects and their related methods and functions}

\description{
  Use \code{AnnotationHub} to interact with Bioconductor's AnnotationHub
  service.  Query the instance to discover and use resources that are of
  interest, and then easily download and import the resource into R for
  immediate use.

  Use \code{AnnotationHub()} to retrieve information about all records
  in the hub. If working offline, add argument \code{localHub=TRUE} to
  work with a local, non-updated hub; It will only have resources
  available that have previously been downloaded. 

  Discover records in a hub using \code{mcols()}, \code{query()},
  \code{subset()}, \code{[}, and \code{display()}.

  Retrieve individual records using \code{[[}. On first use of a
  resource, the corresponding files or other hub resources are
  downloaded from the internet to a local cache. On this and all
  subsequent uses the files are quickly input from the cache into the R
  session. If a user wants to download the file again and not use the
  cache version add the argument \code{force=TRUE}.

  \code{AnnotationHub} records can be added (and sometimes removed) at
  any time. \code{snapshotDate()} restricts hub records to those
  available at the time of the snapshot. \code{possibleDates()} lists
  snapshot dates valid for the current version of Bioconductor.

  The location of the local cache can be found (and updated) with
  \code{getAnnotationHubCache} and \code{setAnnotationHubCache}; 
  \code{removeCache} removes all cache resources.
}

\section{Constructors}{
  \describe{
    \item{}{
      \code{AnnotationHub(..., hub=getAnnotationHubOption("URL"), 
        cache=getAnnotationHubOption("CACHE"),
        proxy=getAnnotationHubOption("PROXY"),
        localHub=FALSE)}:

      Create an \code{AnnotationHub} instance, possibly updating the
      current database of records.
    }
  }
}

\section{Accessors}{
  In the code snippets below, \code{x} and \code{object} are
  AnnotationHub objects.

  \describe{
    \item{}{
      \code{hubCache(x)}:
      Gets the file system location of the local AnnotationHub cache.
    }
    \item{}{
      \code{hubUrl(x)}:
      Gets the URL for the online hub.
    }
   \item{}{
      \code{isLocalHub(x)}:
      Get whether or not constructor was called with \code{localHub=TRUE}.
    }
    \item{}{
      \code{length(x)}:
      Get the number of hub records.
    }
    \item{}{
      \code{names(x)}:
      Get the names (AnnotationHub unique identifiers, of the form
      AH12345) of the hub records.
    }
    \item{}{
      \code{fileName(x)}:
      Get the file path of the hub records as stored in the local cache
      (AnnotationHub files are stored as unique numbers, of the form
      12345).  NA is returned for those records which have not been
      cached.
    }
    \item{}{
      \code{mcols(x)}:
      Get the metadata columns describing each record. Columns include:
      \describe{

        \item{title}{Record title, frequently the file name of the
          object.}

        \item{dataprovider}{Original provider of the resource, e.g.,
          Ensembl, UCSC.}

        \item{species}{The species for which the record is most
          relevant, e.g., \sQuote{Homo sapiens}.}

        \item{taxonomyid}{NCBI taxonomy identifier of the species.}

        \item{genome}{Genome build relevant to the record, e.g., hg19.}

        \item{description}{Textual description of the resource,
          frequently automatically generated from file path and other
          information available when the record was created.}

        \item{tags}{Single words added to the record to facilitate
          identification, e.g,. TCGA, Roadmap.}

        \item{rdataclass}{The class of the R object used to represent
          the object when imported into R, e.g., \code{GRanges},
          \code{VCFFile}.}

        \item{sourceurl}{Original URL of the resource.}

        \item{sourectype}{Format of the original resource, e.g., BED
          file.}
      }
    }

    \item{}{
      \code{dbconn(x)}:
      Return an open connection to the underyling SQLite database.}

    \item{}{
      \code{dbfile(x)}:
      Return the full path the underyling SQLite database.}

    \item{}{
      \code{.db_close(conn)}:
      Close the SQLite connection \code{conn} returned by \code{dbconn(x)}.}

  }
}

\section{Subsetting and related operations}{
  In the code snippets below, \code{x} is an AnnotationHub object.

  \describe{
    \item{}{
      \code{x$name}:
      Convenient reference to individual metadata columns, e.g.,
      \code{x$species}.
    }
    \item{}{
      \code{x[i]}:
      Numerical, logical, or character vector (of AnnotationHub names)
      to subset the hub, e.g., \code{x[x$species == "Homo sapiens"]}.
    }
    \item{}{
      \code{x[[i, force=FALSE, verbose=TRUE]]}:
      Numerical or character scalar to retrieve (if necessary) and
      import the resource into R. If a user wants to download the file
      again and not use the cache version add the argument
      \code{force=TRUE}. \code{verbose=FALSE} will quiet status messages.
    }
    \item{}{
      \code{query(x, pattern, ignore.case=TRUE, pattern.op= `&`)}:
      Return an AnnotationHub subset containing only those elements
      whose metadata matches \code{pattern}. Matching uses
      \code{pattern} as in \code{\link{grepl}} to search the
      \code{as.character} representation of each column, performing a
      logical \code{`&`} across columns.
      e.g., \code{query(x, c("Homo sapiens", "hg19", "GTF"))}.
      \describe{
        \item{\code{pattern}}{A character vector of patterns to search
          (via \code{grepl}) for in any of the \code{mcols()} columns.}
        \item{\code{ignore.case}}{A logical(1) vector indicating whether
          the search should ignore case (TRUE) or not (FALSE).}
        \item{\code{pattern.op}}{Any function of two arguments,
          describing how matches across pattern elements are to be
          combined. The default \code{`&`} requires that only records
          with \emph{all} elements of \code{pattern} in their metadata
          columns are returned. \code{`&`}, \code{`|`} and \code{`!`}
	  are most notably available. See \code{"?&"} or
	  \code{?base::Ops} for more information.}
      }
    }
    \item{}{
      \code{subset(x, subset)}:
      Return the subset of records containing only those elements whose
      metadata satisfies the \emph{expression} in \code{subset}. The
      expression can reference columns of \code{mcols(x)}, and should
      return a logical vector of length \code{length(x)}.
      e.g., \code{subset(x, species == "Homo sapiens" &
        genome=="GRCh38")}.
    }
    \item{}{
      \code{display(object)}:
      Open a web browser allowing for easy selection of hub records via
      interactive tabular display. Return value is the subset of hub
      records identified while navigating the display.
    }
    \item{}{
      \code{recordStatus(hub, record)}:
      Returns a \code{data.frame} of the record id and status. \code{hub} must 
      be a \code{Hub} object and \code{record} must be a \code{character(1)}.
      Can be used to discover why a resource was removed from the hub.
    }
  }
}

\section{Cache and hub management}{
  In the code snippets below, \code{x} is an AnnotationHub object.
  \describe{
    \item{}{
      \code{snapshotDate(x)} and \code{snapshotDate(x) <- value}:
      Gets or sets the date for the snapshot in use. \code{value} should
      be one of \code{possibleDates()}.
    }
    \item{}{
      \code{possibleDates(x)}:
      Lists the valid snapshot dates for the version of Bioconductor that
      is being run (e.g., BiocManager::version()).
    }
    \item{}{
      \code{cache(x)} and \code{cache(x) <- NULL}: Adds (downloads) all
      resources in \code{x}, or removes all local resources
      corresponding to the records in \code{x} from the cache. In this case,
      \code{x} would typically be a small subset of AnnotationHub resources.
    } 
    \item{}{
      \code{hubUrl(x)}:
      Gets the URL for the online AnnotationHub.
    }
    \item{}{
      \code{hubCache(x)}:
      Gets the file system location of the local AnnotationHub cache.
    }
    \item{}{
      \code{removeCache(x)}:
      Removes local AnnotationHub database and all related resources. After
      calling this function, the user will have to download any AnnotationHub
      resources again.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} is an AnnotationHub object.
  \describe{
    \item{}{
      \code{as.list(x)}:
      Coerce x to a list of hub instances, one entry per
      element. Primarily for internal use.
    }
    \item{}{
      \code{c(x, ...)}:
      Concatenate one or more sub-hub. Sub-hubs must reference the same
      AnnotationHub instance. Duplicate entries are removed.
    }
  }
}

\author{Martin Morgan, Marc Carlson, Sonali Arora, and Dan Tenenbaum}

\examples{
  ## create an AnnotationHub object
  library(AnnotationHub)
  ah = AnnotationHub()

  ## Summary of available records
  ah

  ## Detail for a single record
  ah[1]

  ## and what is the date we are using?
  snapshotDate(ah)

  ## how many resources?
  length(ah)

  ## from which resources, is data available?
  head(sort(table(ah$dataprovider), decreasing=TRUE))

  ## from which species, is data available ? 
  head(sort(table(ah$species),decreasing=TRUE)) 

  ## what web service and local cache does this AnnotationHub point to?
  hubUrl(ah)
  hubCache(ah)

  ### Examples ###

  ## One can  search the hub for multiple strings 
  ahs2 <- query(ah, c("GTF", "77","Ensembl", "Homo sapiens"))
  
  ## information about the file can be retrieved using 
  ahs2[1]

  ## one can further extract information from this show method
  ## like the sourceurl using:
  ahs2$sourceurl 
  ahs2$description
  ahs2$title

  ## We can download a file by name like this (using a list semantic):
  gr <- ahs2[[1]]
  ## And we can also extract it by the names like this:
  res <- ah[["AH28812"]]

  ## the gtf file is returned as a GenomicRanges object and contains
  ## data about which organism it belongs to, its seqlevels and seqlengths
  seqinfo(gr) 

  ## each GenomicRanges contains a metadata slot which can be used to get 
  ## the name of the hub object and other associated metadata. 
  metadata(gr) 
  ah[metadata(gr)$AnnotationHubName]
   
  ## And we can also use "[" to restrict the things that are in the
  ## AnnotationHub object (by position, character, or logical vector).
  ## Here is a demo of position:
  subHub <- ah[1:3]

  if(interactive()) {
    ## Display method involves user interaction through web interface
    ah2 <- display(ah)
  }

  ## recordStatus
  recordStatus(ah, "TEST")
  recordStatus(ah, "AH7220")
}

\keyword{classes}
\keyword{methods}
