\name{Hub-utils}
\alias{Hub-utils}
\alias{getInfoOnIds}
  
\title{Get information for selected ids}
\description{Gets information from the Hub database for the given
  selection of ids. The information collected is ah_id, fetch_id, title,
  rdataclass, availablitiy status, biocversion when added, date when
  added, date when removed.}
\usage{
getInfoOnIds(hub,
             ids,
             type=c("fetch_id", "ah_id")
}
\arguments{
  \item{hub}{Hub object.} 
  \item{ids}{List of ids to get from database} 
  \item{type}{Either "fetch_id" or "ah_id". By default
    assumes fetch_id. The fetch_id is used for the naming of the
    resources downloaded.}
}
\section{details}{
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
addition. See example:
}
\value{data.frame of information for selected ids.}
\examples{

\dontrun{
getInfoOnIds(hub, "AH5012", type="ah_id")
getInfoOnIds(hub, 69303)

#
# File not downloaded options: 
#

# 1. You could simply download the file for use
# The file will not change and not be updated so its static download not
# in the cache is fine

# You could type the following into a web browswer 
"https://annotationhub.bioconductor.org/fetch/69303"

# or in R
httr::GET("https://annotationhub.bioconductor.org/fetch/69303",
          write_disk(<pathToSave/69303>, overwrite=FALSE))

# 2. To add to a hub cache
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
