\name{convertHub}
\alias{convertHub}
\title{Convert old Hub to new Hub structure}
\description{The Hub class was updated to utilize BiocFileCache to allow
for file level caching control. This update changed the way files were
stored and named. As a convenience for AnnotationHub and ExperimentHub
we have provided this helper function to try to re-download files and
add them into the BiocFileCache tracking database.}
\usage{
convertHub(oldcachepath=NULL,
           newcachepath=NULL,
           hubType=c("AnnotationHub", "ExperimentHub"),
           proxy=getAnnotationHubOption("PROXY"),
           max.downloads=getAnnotationHubOption("MAX_DOWNLOADS"),
           force=FALSE, verbose=TRUE)
}
\arguments{
  \item{oldcachepath}{character(1) complete file path location of the
    old hub to be converted. If left as \code{NULL}, will use the
    default path of the old code, which for unix systems was in a user's
    home directory "~\\" and for windows users was in a
    user's AppData directory "~\\AppData".} 
  \item{newcachepath}{character(1) complete file path to the new
    location for the cache. If left as \code{NULL}, will use the new
    default path which utilizes the \code{rappdir::user_cache_dir} to
    deterime the appropriate caching location.} 
  \item{hubType}{Either AnnotationHub or ExperimentHub. By default
    assumes AnnotationHub.}
  \item{proxy}{proxy connection allowing Internet access, usually
    through a restrictive firewall.  Default: NULL.}
  \item{max.downloads}{numeric(1). The integer number of downloads
    allowed before triggering a warning. This is to help avoid accidental
    download of a large number of AnnotationHub members}
  \item{force}{logical(1). Force re-download of a resource rather than
    using a cached version.}
  \item{verbose}{logical(1). Print out status messages.}
}
\value{character(1). File path of new cache location. If \code{verbose}
  also prints status messages for downloading files and any files that
  were not redownloaded.}
\examples{
# To transition over from old default to new default location
\dontrun{convertHub()}
}
\author{Lori Shepherd}
\seealso{\code{\link{AnnotationHub}},
  \code{\link{getAnnotationHubOption}}}
\keyword{utilities}
