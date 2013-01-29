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

.metadataPathField <- function(x, path, field) {
    ## get the metadata of a particular path and type
    escapedPath <- paste0('"', path, '"')
    url <- paste(snapshotUrl(x), "query", "RDataPath", escapedPath, sep="/")
    fromJSON(url)[[1]][[field]]
}

.metadata <- function(snapshotUrl, filter=character()) {
    url <- if (length(filter)) {
        paste(snapshotUrl, "query", filter, sep="/")
    } else paste(snapshotUrl, "query", sep="/")
    meta <- .fromJSON_file(url)
    ## Concerned: that this may become too slow as more metadata piles on.
    ## 1st sort based on names (THEN rbind together)
    sorted <- lapply(meta, function(x) x[sort(names(x))])
    data.frame(do.call(rbind, sorted))
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
