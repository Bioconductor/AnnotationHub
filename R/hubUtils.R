## JSON utilities

.fromJSON_file <- function(url)
{
    tmp <- tempfile()
    download.file(url, tmp, quiet=TRUE)
    fromJSON(paste0(readLines(tmp), collapse=""))
}

## queries

.hostUrl <- function() {
    getOption("AnnotationHub.Host",
              "http://annotationhub.bioconductor.org")
}

.hubUrl <- function() {
    paste(.hostUrl(), "ah", sep="/")
}

.snapshotPaths <- function(snapshotUrl) {
    url <- paste(snapshotUrl, "getAllResourcePaths", sep="/")
    urls <- fromJSON(url)
    setNames(urls, make.names(urls))
}

.snapshotVersion <- function() biocVersion()

.snapshotDate <- function(hubUrl, snapshotVersion) {
    url <- paste(hubUrl, snapshotVersion, "getLatestSnapshotDate",
                 sep="/")
    as.POSIXlt(fromJSON(url))
}

.snapshotUrl <- function(hubUrl, snapshotVersion, snapshotDate) {
    paste(hubUrl, snapshotVersion, snapshotDate, sep="/")
}

.possibleDates <- function(hubUrl, snapshotVersion) {
    url <- paste(hubUrl, snapshotVersion, "getSnapshotDates",
                  sep="/")
    sort(as.POSIXlt(fromJSON(url)))
}


## TODO: vectorize for both paths and fields.
## I should be able to vectorize on fields in .metadata, and then just
## use the helper below to also vectorize on paths.
## ALSO: this function should not just assume that we want to use "RDataPath"
## Instead it should be using field there in a general way.
## So in both cases, I need a helper for processing the URLs.
## .metadataURLPath() ???

.metadataPathField <- function(x, path, field) {
    ## get the metadata of a particular path and type
    escapedPath <- paste0('"', path, '"')
    url <- paste(snapshotUrl(x), "query", "RDataPath", escapedPath, sep="/")
    fromJSON(url)[[1]][[field]]
}

## ## a vectorized version of the above.
## .metadataPathsField <- function(x, path, field) {
##     lapply(path, .metadataPathField, x=x, field=field)
## }



## TODO: fix .metadata()
## This function is nice and terse, but it won't work because we store
## filter values in a list object (for the case where we have more
## than one thing kind of TaxonomyId etc.)  Also, the URL needs both
## names and values to be put together. "/TaxonomyId/9606" etc.

## SO:
## 1) filter needs to be a list
## 2) needs to create a URL to do it's job
## 3) needs to return a (filtered) DataFrame to complete it's job.
## .metadata <- function(snapshotUrl, filter=character()) {
##     url <- if (length(filter)) {
##         paste(snapshotUrl, "query", filter, sep="/")
##     } else paste(snapshotUrl, "query", sep="/")
##     meta <- .fromJSON_file(url)
##     ## Concerned: that this may become too slow as more metadata piles on.
##     ## 1st sort based on names (THEN rbind together)
##     sorted <- lapply(meta, function(x) x[sort(names(x))])
##     data.frame(do.call(rbind, sorted))
## }

.metadata <-
    function(x, filterValues=list()) {
    ## 1st validate filters
    .validFilterValues(x, filterValues)
    ## then make a url
    url <- if (length(filter)) { ## get some
        ## URL must be specific
        filters <- unlist(Map(.processFilter, filterValues,
                              names(filterValues)))
        filters <- paste(filters, collapse="/")
        url <- paste(snapshotUrl(), "query", filters, sep="/") ## vectorized?
    } else {## get all of them
        paste(snapshotUrl(), "query", sep="/")
    }
    ## get the metadata
    meta <- .fromJSON_file(url)
    ## This will change if the json changes.
    sorted <- lapply(meta, function(x) x[sort(names(x))])
    data.frame(do.call(rbind, sorted))  ## TODO: changeover to DataFrame
}




.keytypes <-function(snapshotUrl) {
    url <- paste(snapshotUrl, 'getAllKeytypes', sep="/")
    fromJSON(url)
}

.keys <-
    function(snapshotUrl, keytype)
{
    if (!is.character(keytype) || length(keytype) > 1L)
        stop("'keytype' must be a character vector of length 1")
    ## then retrieve values from host
    url <- paste(snapshotUrl, "getAllKeys", "keytype", keytype, sep="/")
    unique(fromJSON(url))
}

setMethod("snapshotVersion", "missing", function(x, ...) {
    .snapshotVersion()
})

setMethod("hubUrl", "missing", function(x, ...) {
    .hubUrl()
})

setMethod("snapshotDate", "missing", function(x, ...) {
    .snapshotDate(.hubUrl(), snapshotVersion())
})

setMethod("snapshotUrl", "missing", function(x, ...) {
    .snapshotUrl(hubUrl(), snapshotVersion(), snapshotDate())
})

setMethod("snapshotPaths", "missing", function(x, ...) {
    .snapshotPaths(snapshotUrl())
})

setMethod("possibleDates", "missing", function(x, ...) {
    .possibleDates(.hubUrl(), snapshotVersion())
})


.cacheResource <- function(hubCache, path=character()) {
    url <- file.path(hubCache, "resources")
    if (length(path))
        url <- file.path(url, .pathToLocalPath(path))
    url
}

.hubResource <- function(hubUrl, path=character()) {
    file <- paste(hubUrl, "resources", sep="/")
    if (length(path))
        file <- paste(file, path, sep="/")
    url(file)
}

setMethod("hubResource", "missing", function(x, path=character(), ...) {
    .hubResource(hubUrl(), ...)
})

setMethod("metadata", "missing", function(x, ...) {
    .metadata(snapshotUrl())
})

setMethod("metadata", "list", function(x, ...) {
    .metadata(snapshotUrl())
})
