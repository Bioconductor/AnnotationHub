\name{AnnoationHub-objects}
\docType{class}

% Classes
\alias{AnnotationHub}
\alias{class:AnnotationHub}
\alias{AnnotationHub-class}

% Accessor-like methods
\alias{show,AnnotationHub-method}

\alias{$,AnnotationHub-method}

\alias{[[,AnnotationHub-method}

\alias{filters} % only if we have the generic here do we do this
\alias{filters,AnnotationHub-method}
\alias{filters<-}
\alias{filters<-,AnnotationHub-method}

\alias{hubUrl} 
\alias{hubUrl,AnnotationHub-method}

\alias{hubCache}
\alias{hubCache,AnnotationHub-method}
\alias{hubCache<-}
\alias{hubCache<-,AnnotationHub-method}

\alias{hubResource} 
\alias{hubResource,AnnotationHub-method}

\alias{keys,AnnotationHub-method}

\alias{keytypes,AnnotationHub-method}

\alias{cols,AnnotationHub-method}

\alias{length,AnnotationHub-method}

\alias{metadata,AnnotationHub-method}

\alias{names,AnnotationHub-method}

\alias{possibleDates}
\alias{possibleDates,AnnotationHub-method}

\alias{snapshotDate}
\alias{snapshotDate,AnnotationHub-method}
\alias{snapshotDate<-}
\alias{snapshotDate<-,AnnotationHub-method}

\alias{snapshotPaths}
\alias{snapshotPaths,AnnotationHub-method}

\alias{snapshotUrl}
\alias{snapshotUrl,AnnotationHub-method}

\alias{snapshotUrls}
\alias{snapshotUrls,AnnotationHub-method}

\alias{snapshotVersion}
\alias{snapshotVersion,AnnotationHub-method}



% supporting functions
\alias{availableUniprotSpecies}
\alias{lookupUniprotSpeciesFromTaxId}


\title{AnnotationHub objects and their related methods and functions}

\description{
  \code{AnnotationHub} is the base class for interacting with the Uniprot
  web services from Bioconductor.

  In much the same way as an \code{AnnotationDb} object allows acces to
  select for many other annotation packages, \code{AnnotationHub} is meant
  to allow usage of \code{select} methods and other supporting methods
  to enable the easy extraction of data from the Uniprot web services.

  \code{select}, \code{cols} and \code{keys} are used together to
  extract data via an \code{AnnotationHub} object. 

  \code{cols} shows which kinds of data can be returned for the
  \code{AnnotationHub} object.
  
  \code{keytypes} allows the user to discover which keytypes can be
  passed in to \code{select} or \code{keys} via the \code{keytype}
  argument.
  
  \code{keys} returns keys for the database contained in the
  \code{AnnotationHub} object .  By default it will return the primary keys
  for the database, which are UNIPROTKB keys, but if used with the
  \code{keytype} argument, it will return the keys from that keytype.
  
  \code{select} will retrieve the data as a data.frame based on
  parameters for selected \code{keys} and \code{cols} and \code{keytype}
  arguments.
  
  The \code{AnnotationHub} will be loaded whenever you load the
  \code{AnnotationHub} package.  This object will be set up to retrieve
  information from Homo sapiens by default, but this value can be
  changed to any of the species supported by Uniprot.  The
  \code{species} and \code{taxId} methods allow users to see what
  species is currently being accessed, and \code{taxId<-} allows them to
  change this value.
  
  \code{species} shows the genus and species label currently attached to
  the \code{AnnotationHub} objects database.
  
  \code{taxId} shows the NCBI taxonomy ID currently attached to
  the \code{AnnotationDb} objects database.  Using the equivalently
  names replace method (\code{taxId<-}) allows the user to change the
  taxon ID, and the species represented along with it.
  
  \code{availableUniprotSpecies} is a helper function to list out the
  available Species along with their official taxonomy IDs that are
  available by Uniprot.  Because there are so many species represented
  at UniProt, there is also a pattern argument that can be used to
  restrict the range of things returned to be only those whose species
  names match the searth term.  Please remember when using this argument
  that the Genus is always capitalized and the species never is.
  
  \code{lookupUniprotSpeciesFromTaxId} is another helper that will look
  up the species of any tax ID that is supported by Uniprot.
  
}

\usage{
  cols(x)
  keytypes(x)
  select(x, keys, cols, keytype, ...)
  species(x)
  taxId(x)

  availableUniprotSpecies(pattern, n=Inf)
  lookupUniprotSpeciesFromTaxId(id)
}

\arguments{
  \item{x}{the \code{AnnotationHub} object. }
  \item{keys}{the keys to select records for from the database.  All possible 
    keys are returned by using the \code{keys} method.}
  \item{cols}{the cols or kinds of things that can be retrieved from the 
    database.  As with \code{keys}, all possible cols are returned by using the
    \code{cols} method.}
  \item{keytype}{the keytype that matches the keys used.  For the
    \code{select} methods, this is used to indicate the kind of ID being used
    with the keys argument. For the \code{keys} method this is used to
    indicate which kind of keys are desired from \code{keys}
  }
  \item{pattern}{A string passed in to limit the results}
  \item{n}{the maximim number of results to return.}
  \item{id}{a taxonomy id}
  \item{...}{other arguments}
}

\value{
  \code{keys},\code{cols},\code{keytypes}, \code{species} and
  \code{lookupUniprotSpeciesFromTaxId} each return a character vector of
  possible values.
  
  \code{taxId} returns a numeric value that corresponds to the taxonomy
  ID.

  \code{select} and \code{availableUniprotSpecies} each return a
  data.frame.
}

\author{Marc Carlson}

\seealso{
  \code{select}
}

\examples{
## look at the object
AnnotationHub

## get the current species
species(AnnotationHub)

## look up available species with their tax ids
availableUniprotSpecies("musculus")

## get the current taxId
taxId(AnnotationHub)

## look up the species that goes with a tax id
lookupUniprotSpeciesFromTaxId(9606)

## set the taxId to something else
taxId(AnnotationHub) <- 10090
AnnotationHub

## list the possible key types
head(keytypes(AnnotationHub))

## list the cols that can be retreived
head(cols(AnnotationHub))

## list all possible keys of type entrez gene ID.
## (this process is not instantaneous)
if(interactive()){
  egs = keys(AnnotationHub, "ENTREZ_GENE")
}

## use select to extract some data
  keys <- c('1','2')
  cols <- c("PDB","UNIGENE","SEQUENCE")
  kt <- "ENTREZ_GENE"
  res <- select(AnnotationHub, keys, cols, kt)
  head(res)
}


\keyword{classes}
\keyword{methods}

