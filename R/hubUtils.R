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

## .metadataPathField <- function(x, path, field) {
##     ## get the metadata of a particular path and type
##     escapedPath <- paste0('"', path, '"')
##     url <- paste(snapshotUrl(x), "query", "RDataPath", escapedPath, sep="/")
##     fromJSON(url)[[1]][[field]]
## }


## ## a vectorized version of the above.
## .metadataPathsField <- function(x, path, field) {
##     lapply(path, .metadataPathField, x=x, field=field)
## }


## metadata takes a filter list and keytypes and returns a DataFrame
.metadata <- function(x, filters=list(), keytypes=c("Title","Species",
                                           "TaxonomyId","Genome","Description",
                                           "Tags","RDataClass","Notes")) {
    ## format keytypes
    keytypes <- paste("cols",keytypes, sep="/", collapse="/")
    ## then make a url
    url <- if (length(filters)>0 && filters!="" &&
               !is.null(filters)) { ## get some
        ## URL must be specific
        filters <- .makeURLFilters(filters)
        paste(snapshotUrl(), "query", filters, keytypes, sep="/") ##vectorized?
    } else {## get all of them
        paste(snapshotUrl(), "query", keytypes, sep="/")
    }
    ## get the metadata
    meta <- .fromJSON_file(url) ## list form (by row)

    ## make a data.frame (remove this later)
    if(class(meta)=="list"){
        mat <- do.call(cbind, meta) ## as matrix
        data.frame(mat)  ## TODO: changeover to DataFrame
    }else{
        ## double cast so label is the colname, and return val is consistent.
        as.data.frame(as.list(meta)) 
    }
    
## TODO switch from data.frame to DataFrame (compute this from the
## list obj "meta")
    
##     ## So split that into a list of columns...
##     ## TEMP HACK to remove problem columns
##     smat <- mat[,c(1:15)]
## ##   cols <- apply(smat, 2, function(x) unlist(as.vector(x), recursive=FALSE)) )
## ##    smat <- mat[1:4,c(1,15)]
## ##    smat <- mat[1:4,c(1,15:16)] ## still causes grief for some compound things
##     cols <- apply(smat, 2, function(x){as(x, "List")} )
##     DataFrame(cols)
    
}

## TODO: I need to use splitAsList() with a factor for each column
## that I want to squish? : not quite

## So I want to convert everything to "List" with as()
## And for the complicated ones I want to do it like this
## (notice unlist2 from AnntoationDbi is making an appearance):
## foo = lapply(smat[,3], unlist2)
## OR lapply(smat[,3] I) (less invasive)
## And then:
## as(foo, "List")





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
    .metadata(snapshotUrl(), list())
})

## setMethod("metadata", "list", function(x, filters, ...) {
##     .metadata(x, filters, ...)
## })
