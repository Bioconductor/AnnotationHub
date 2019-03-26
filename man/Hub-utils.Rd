\name{Hub-utils}
\alias{Hub-utils}
\alias{getInfoOnIds}
\alias{getInfoOnIds,missing-method}
\alias{getInfoOnIds,numeric-method}
\alias{getInfoOnIds,character-method}


\title{Get information for selected ids}
\description{Gets information from the Hub database for the given
  selection of ids. The information collected is ah_id, fetch_id, title,
  rdataclass, availablitiy status, biocversion when added, date when
  added, date when removed, and file size.}
\usage{
  getInfoOnIds(hub, ids)
}
\arguments{
  \item{hub}{Hub object.}
  \item{ids}{List of ids to get from database. Can be left unset to use
    all active ids in the hub. If given, it is either a numeric or
    character vector. See details section.}
}
\section{details}{
If a hub object is passed into the function with no ids given, it will
use all active ids associated with that hub object
(\code{names(ah)}). It is recommended to only run this option if you are
using a smaller subset Hub object. The ids argument can be specified as either a
character vector or a numeric vector. If using a character vector, the
function assumes the 'ah_ids' were used, and each entry takes the form
similar to \code{c("AH2", "AH5012")}. If a numeric vector is specified,
the function assume the 'fetch_ids' were used. The 'fetch_id' is the
identifier that is used for the file name. For older versions of the
cache these were the file names directly.

This function was designed as a helper function when converting between
old versions of Hubs to the newer versions that utilize
BiocFileCache. If files were not able to be redownloaded, one could put
the ids into this function to get more information on them.  Note: Some
resources may appear available  but could not be
redownloaded. Most likely these files are rdataclass 'OrgDb'. 'OrgDb' are only
valid for a given release cycle and are masked to any future release
cycle. It is recommended to update to the current 'OrgDb' but if the old
file was not able to be downloaded and still desired, one could download
manually download using the fetch_id. Example if the file not able to be
downloaded was "~./AnnotationHub/69303" then the fetch call is:
"https://annotationhub.bioconductor.org/fetch/69303".
While the convertHub function will not automatically download it is
still possible to keep track in the cache by doing a manually
addition. Although not recommended.  In reality these file will not be
updated so the original file could also still be used.

This function could also be a utility function to help determine any
given resources download size.
}
\value{data.frame of information for selected ids. The information
  collected is ah_id, fetch_id, title, rdataclass, availablitiy status,
  biocversion when added, date when added, date when removed, and file size.}
\examples{

\dontrun{
getInfoOnIds(hub, c("AH2","AH5012"))
getInfoOnIds(hub, 69303)

}

# If using in conjunction with convertHub,
#
# File not downloaded options:
#

\dontrun{
# 1. Use the original file. In reality the file is not going to be
  updated or should change. The original file does not need to be
  tracked and could now be referenced directly for usage. It will not be
  available in the Hub.

# 2. You could simply download the file for use
# The file will not change and not be updated so its static download not
# in the cache is fine

# You could type the following into a web browswer
"https://annotationhub.bioconductor.org/fetch/69303"

# or in R
httr::GET("https://annotationhub.bioconductor.org/fetch/69303",
	  write_disk(<pathToSave/69303>, overwrite=FALSE))

# 3. To add to a hub cache (not recommended)
hub <- AnnotationHub()
bfc <- AnnotationHub:::.get_cache(hub)

# the hub creates the rname is in the format of 'ah_id : fetch_id'
bfcadd(bfc, fpath="https://annotationhub.bioconductor.org/fetch/69303",
rname="AH62557 : 69303")
}
}
\author{Lori Shepherd}
\seealso{\code{\link{AnnotationHub}},
  \code{\link{convertHub}}}
\keyword{utilities}
