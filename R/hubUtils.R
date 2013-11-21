## JSON utilities
.printf <- function(...) print(noquote(sprintf(...)))

## helper to add some error handling for when the server is throwing errors.
## TODO: make this work and then replace all fromJSON calls with it.
## .parseJSON <- function(url){
##     ## process url to get rid of any spaces.
##     url <- gsub(" ", "%20", url)
    
##     tryCatch({
##         tmp <- tempfile()
##         if (getOption("AnnotationHub.debug", FALSE))
##             .printf("Visiting %s", url)
##         download.file(url, tmp, quiet=TRUE)
##         fromJSON(paste0(readLines(tmp), collapse=""))
##     }, error=function(err){
##         stop("An error occured when parsing the JSON: ", err)
##     } )
## }

## Now in theory we should no longer have to check if things coming
## through json are going to parse since the complex stuff is always
## this weird double list thing.

## Helper to replace "NA" with NA
.na2na <- function(x){
    x[x=="NA"] <- NA
    x
}

## maybe replace .parseJSON with version that always cleans NAs?
.parseJSON <- function(url, ...){
    ## process url to get rid of any spaces.
    url <- gsub(" ", "%20", url)
    if (getOption("AnnotationHub.debug", FALSE))
        .printf("Visiting %s", url)
    res <- fromJSON(file=url, ...)
    .na2na(res)
}

## This one just cleans the big list object that comes back from metadata.
.parseJSONMetadataList <- function(url){
    ## process url to get rid of any spaces.
    url <- gsub(" ", "%20", url)
    ## then parse the JSON
    if (getOption("AnnotationHub.debug", FALSE))
        .printf("Visiting %s", url)
    ## It turns out that if you give fromJSON a file= argument that is a URL,
    ## it does not always succesfully retrieve the file. For example, on 
    ## Nov 7, 2013, trying to do this:
    ## fromJSON(file="http://annotationhub.bioconductor.org/ah/2.14/1.3.1/2013-10-30/query/cols/Title/cols/Species/cols/TaxonomyId/cols/Genome/cols/Description/cols/Tags/cols/RDataClass/cols/RDataPath")
    ## ...returns an error. We can work around this by downloading to a tempfile and 
    ## then calling fromJSON() on that. We need to use the "curl"  method for downloading.
    ## (using the default method produces the same error).
    ## The real problem is probably that it takes too long for the request to be fulfilled, 
    ## so it's really a matter of optimizing the server performance.
    t <- tempfile()
    download.file(url, destfile=t, method="curl", quiet=TRUE)
    j <- fromJSON(file=t)
    #lapply(j, function(x){lapply(x, .na2na)})
    rapply(j, .na2na, how="replace")
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

.clientVersion <- function() {
    as.character(packageVersion("AnnotationHub"))
}

.snapshotPaths <- function(snapshotUrl) {
    url <- paste(snapshotUrl, "getAllResourcePaths", sep="/")
    urls <- .parseJSON(url)
    setNames(urls, make.names(urls))
}

.snapshotVersion <- function() paste(biocVersion(), .clientVersion(), sep="/")

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

.toDataFrame <- function(lst)
{
    ## 1st decide if we need character or characterLists
    whichMulti <- unlist(lapply(lst, function(x){max(x[[2]]) > 1}))

    lens1 <- unlist(lapply(lst, function(x){length(x[[1]])}))
    if(any(lens1 == 0)){stop("Some of the metadata fields are empty.")}
    lens2 <- unlist(lapply(lst, function(x){length(x[[2]])}))
    if(length(unique(lens2)) > 1){
        stop("All partitions must be the same length.")}
    if(any(lens1 < lens2)){
        stop("Some data is missing from the vector to be partitioned.")}
    
    ## make list of lists into a set of character and or characterList vectors.
    .makeVecs <- function(l, isMulti){
         if(isMulti){ ## make characterList
             splitAsList(l[[1]],f= rep(seq_along(l[[2]]), l[[2]]))
         }else{ ## make character
             as.character(l[[1]])
         }
    }
    cols <- mapply(.makeVecs, lst, whichMulti, SIMPLIFY=FALSE)
    DataFrame(cols)
    
}


## metadata takes a filter list and cols and returns a DataFrame
.metadata <- function(snapshotUrl, filters=list(), cols=c("Title","Species",
                                           "TaxonomyId","Genome","Description",
                                           "Tags","RDataClass","RDataPath")) {  
    ## format cols
    cols <- paste("cols",cols, sep="/", collapse="/")
    ## then make a url
    url <- if (length(filters)>0 && filters!="" &&
               !is.null(filters)) { ## get some
        ## URL must be specific
        filters <- .makeURLFilters(filters)
        paste(snapshotUrl, "query", filters, cols, sep="/") ##vectorized?
    } else {## get all of them
        paste(snapshotUrl, "query", cols, sep="/")
    }

    
    ## get the metadata
    ## meta <- .parseJSON(url) ## list form (by row)  (USUALLY)
    meta <- .parseJSONMetadataList(url)
    ## make a DataFrame (remove this later)
    .toDataFrame(meta)


    
    ## ## make a data.frame (remove this later)
    ## if(class(meta)=="list"){
    ##     idx <- sapply(meta, is, "list")
    ##     meta[idx] <- lapply(meta[idx], function(elt) {
    ##         ## named character vectors come from json as named lists
    ##         subidx <- sapply(elt, is, "list")
    ##         elt[subidx] <- lapply(elt[subidx], unlist)
    ##         elt
    ##     })
    ##     meta[idx] <- lapply(meta[idx], as, "List")
    ##     DataFrame(meta)
    ## }else{
    ##     ## double cast so label is the colname, and return val is consistent.
    ##     DataFrame(as.list(meta))
    ## }
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
    as.character(.snapshotVersion())
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
    if (getOption("AnnotationHub_Use_Disk", FALSE))
    {
        file <- paste(hubUrl, .snapshotVersion(), "resources", sep="/")
        if (length(path))
            file <- paste(file, path, sep="/")
        return(file)
    } else { # S3 is the default
        ## At some point we may want to support restricted-access buckets...
        bucketname <- getOption("ANNOTATION_HUB_BUCKET_NAME", "annotationhub")
        s3url <- getOption("ANNOTATION_HUB_S3_URL",
            paste0("http://", bucketname,".s3.amazonaws.com/"))
        file <- s3url
        path <- gsub("//", "/", path, fixed=TRUE)
        path <- sub("^\\/", "", path)
        if (length(path))
            file <- paste0(file, path)
        return(file)
    }
}

setMethod("hubResource", "missing", function(x, path=character(), ...) {
    .hubResource(hubUrl(), ...)
})

setMethod("metadata", "missing", function(x, ...) {
    .metadata(snapshotUrl(), list())
})

 