## JSON utilities
.printf <- function(...) print(noquote(sprintf(...)))

## helper to add some error handling for when the server is throwing errors.
## TODO: make this work and then replace all fromJSON calls with it.
.parseJSON <- function(url){
    ## process url to get rid of any spaces.
    url <- gsub(" ", "%20", url)
    
    tryCatch({
        tmp <- tempfile()
        if (getOption("AnnotationHub.debug", FALSE))
            .printf("Visiting %s", url)
        download.file(url, tmp, quiet=TRUE)
        fromJSON(paste0(readLines(tmp), collapse=""))
    }, error=function(err){
        stop("An error occured when parsing the JSON: ", err)
    } )
}

## .parseJSON("http://annotationhub.bioconductor.org/ah/2.12/2013-01-22/getAllResourcePaths")
## VS a bad URL:
## .parseJSON("http://foo/bar")


## .parseJSON_file <- function(url)
## {
##     tmp <- tempfile()
##     download.file(url, tmp, quiet=TRUE)
##     .parseJSON(paste0(readLines(tmp), collapse=""))
## }

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
    urls <- .parseJSON(url)
    setNames(urls, make.names(urls))
}

.snapshotVersion <- function() biocVersion()

.snapshotDate <- function(hubUrl, snapshotVersion) {
    url <- paste(hubUrl, snapshotVersion, "getLatestSnapshotDate",
                 sep="/")
    as.POSIXlt(.parseJSON(url))
}

.snapshotUrl <- function(hubUrl, snapshotVersion, snapshotDate) {
    paste(hubUrl, snapshotVersion, snapshotDate, sep="/")
}

.possibleDates <- function(hubUrl, snapshotVersion) {
    url <- paste(hubUrl, snapshotVersion, "getSnapshotDates",
                  sep="/")
    sort(as.POSIXlt(.parseJSON(url)))
}



## metadata takes a filter list and cols and returns a DataFrame
.metadata <- function(x, filters=list(), cols=c("Title","Species",
                                           "TaxonomyId","Genome","Description",
                                           "Tags","RDataClass","Notes")) {    
    ## format cols
    cols <- paste("cols",cols, sep="/", collapse="/")
    ## then make a url
    url <- if (length(filters)>0 && filters!="" &&
               !is.null(filters)) { ## get some
        ## URL must be specific
        filters <- .makeURLFilters(filters)
        paste(snapshotUrl(), "query", filters, cols, sep="/") ##vectorized?
    } else {## get all of them
        paste(snapshotUrl(), "query", cols, sep="/")
    }
    ## get the metadata
    meta <- .parseJSON(url) ## list form (by row)   ## BOOM
    
    ## make a data.frame (remove this later)
    if(class(meta)=="list"){
        ls <- lapply(meta, as, "List") ##Converting to "List" allows compression
        DataFrame(ls)
    }else{
        ## double cast so label is the colname, and return val is consistent.
        DataFrame(as.list(meta))
    }
}


.keytypes <-function(snapshotUrl) {
    url <- paste(snapshotUrl, 'getAllKeytypes', sep="/")
    .parseJSON(url)
}


.keys <-
    function(snapshotUrl, keytype)
{
    if (!is.character(keytype) || length(keytype) > 1L)
        stop("'keytype' must be a character vector of length 1")
    ## then retrieve values from host
    url <- paste(snapshotUrl, "getAllKeys", "keytype", keytype, sep="/")
    unique(.parseJSON(url))
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
    .metadata(snapshotUrl(), list())
})

