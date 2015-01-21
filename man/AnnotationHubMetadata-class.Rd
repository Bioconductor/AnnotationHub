\name{AnnotationHubMetadata-class}
\docType{class}
\alias{AnnotationHubMetadata}
\alias{AnnotationHubMetadata-class}
\alias{show,AnnotationHubMetadata-method}


\title{Class \code{"AnnotationHubMetadata"}}

\description{

  \code{AnnotationHubMetadata} is used to represent record(s) in the
  server data base.

}

\section{Objects from the Class}{

  Objects can be created by calls to the constructor,
  \code{AnnotationHubMetadata()}.

}

\usage{

AnnotationHubMetadata(AnnotationHubRoot, SourceFile, SourceUrl,
                 SourceVersion, SourceMd5, SourceSize, DataProvider,
                 Title, Description, Species, TaxonomyId, Genome, Tags,
                 Recipe, RecipeArgs = list(), RDataClass, RDataVersion,
                 RDataDateAdded, RDataPath, Maintainer, ...,
                 BiocVersion = biocVersion(), Coordinate_1_based =
                 TRUE, Notes = NA_character_, Location_Prefix)
}

\arguments{
    \item{AnnotationHubRoot}{\code{character(1)} Absolute path to
        directory structure containing resources to be added to
        AnnotationHub}

    \item{SourceFile}{\code{character(1)} A path, relative to
        \code{AnnotationHubRoot}, to a resource to be added to
        AnnotationHub.}
    \item{SourceUrl}{\code{character()} URL where resource(s) can be
        found}
    \item{SourceVersion}{\code{character(1)} Version of original file}
    \item{SourceMd5}{\code{character()} md5 hash of original file}
    \item{SourceSize}{\code{numeric(1)} Number of bytes in original file}
    \item{DataProvider}{\code{character(1)} Where did this resource come
        from?}

    \item{Title}{\code{character(1)} Title for this resource}
    \item{Description}{\code{character(1)} Description of the resource}
    \item{Species}{\code{character(1)} Species name}
    \item{TaxonomyId}{\code{character(1)}  NCBI code}
    \item{Genome}{\code{character(1)} Name of genome build}
    \item{Tags}{\code{character()} Free-form tags}

    \item{Recipe}{\code{character(1)} Name of recipe function}
    \item{RecipeArgs}{\code{list()} Arguments to recipe function}
    \item{RDataClass}{\code{character(1)} Class of derived object
        (e.g. \sQuote{GRanges})}
    \item{RDataVersion}{\code{numeric_version()} Version of resource}
    \item{RDataDateAdded}{\code{POSIXct()} Date added to
        AnnotationHub. Used to determine snapshots.}
    \item{RDataPath}{\code{character(1)} file path to serialized form}
    \item{Maintainer}{\code{character(1)} Maintainer name and email
        address, \sQuote{A Maintainer \url{a.maintainer@email.addr}}}
    \item{...}{Additional arguments, passed directly to the class
      initialize method.}
    \item{BiocVersion}{\code{character(1)} Under which resource was built}
    \item{Location_Prefix}{This was added for resources where the
    metadata only is stored and the resource itself comes from a third
    party web site.  The location prefix says the base path where the
    resource is coming from, and the default value will be from our own site.}
    
    \item{Coordinate_1_based}{\code{logical(1)} Do coordinates start
        with 1 or 0?}
    \item{Notes}{\code{character()} Notes about the resource.}

}

\value{
  \code{AnnotationHubMetadata} returns an instance of the class.
}

\author{Dan Tenenbaum and Marc Carlson}


\examples{
getClass("AnnotationHubMetadata")
}

\keyword{classes}
