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
.parseJSONMetadataListViaGET <- function(url){
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
    download.file(url, destfile=t, quiet=TRUE)
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
    ## url <- paste(snapshotUrl, "getAllResourcePaths", sep="/")
    ## urls <- .parseJSON(url)
    urls <- unlist(.metadata(snapshotUrl, columns="RDataPath"))
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
    sort(as.POSIXlt(.parseJSON(url), tz = "GMT"))
}

.convertDatesToStandard <- function(dates){
    as.POSIXlt(as.numeric(dates), origin='1970-01-01 00:00.00 UTC', tz="GMT")
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
    columns <- mapply(.makeVecs, lst, whichMulti, SIMPLIFY=FALSE)
    meta <- DataFrame(columns)
    ## clean up for WHEN the a col is RDataDateAdded
    if('RDataDateAdded' %in% colnames(meta)){
        meta$RDataDateAdded <- .convertDatesToStandard(meta$RDataDateAdded)
    }
    meta
}


##############################################################################
## helper for quickly getting the max date (want this to be fast as possible)
.latestDate <- function(x) {
    hubUrl <- hubUrl()
    snapshotVersion <- snapshotVersion()
    url <- paste(hubUrl, snapshotVersion, "getSnapshotDates",
                  sep="/")
    max(as.POSIXlt(.parseJSON(url)))
}
## usage: system.time(AnnotationHub:::.latestDate(ah))
## time comparison
## system.time(replicate(100, AnnotationHub:::.latestDate(ah), simplify=FALSE))
## system.time(replicate(100, max(possibleDates()), simplify=FALSE))
## So I do save a very TINY amount of time with my helper (so lets use it).

##############################################################################
## helper to save snapShotdate to cache
.snapshotDateFile <- function()
    file.path(hubCache(), "snapshotDate.Rda")

.updateSnapshotDateFile <- function(date)
    save(date, file=.snapshotDateFile())

## helper to get latest SnapshotDate from cache. (only works if
## caching is enabled)
.lastSnapshotDate <- function() {
    snapshotLoc <- .snapshotDateFile()    
    if(file.exists(snapshotLoc)){
        ## Filename shenanigans! (in case the file gets renamed on us)
        objName <- load(snapshotLoc)
        date <- eval(parse(text=objName))
    }else{
        date <- snapshotDate()
        .updateSnapshotDateFile(date)
    }
    date
}

## .metadataRemote returns a DataFrame
.metadataRemote <-
    function(snapshotUrl, columns=.DEFAULT_COLUMNS)
{
    message("updating metadata from server...")
    ## format columns
    colsVec <- paste("cols", columns, sep="/", collapse="/")
    ## then make a url
    url <- paste(snapshotUrl, "query", colsVec, sep="/")
    meta <- .parseJSONMetadataListViaGET(url)
    ## make a DataFrame
    .toDataFrame(meta)
}

## helper for filtering a Data.frame locally
.filterMetadata <- function(meta, filters, columns)
{
    .subset <- function(meta, cname, fval){
        meta[meta[[cname]] %in% fval,]
    }
    ## inneficient loop - mapply strategy requires complex re-assembly...
    ## But at least there are not that many metadata field types...
    for(i in seq_along(filters)){
        cname <- names(filters)[i]
        fval <- filters[[i]]
        meta <- .subset(meta, cname, fval)
    }
    ## always drop cols AFTER we drop rows...
    meta[, columns, drop=FALSE]
}
## example test filter and cols:
## filters = list(Species=c("Homo sapiens","Mus musculus"), RDataClass="FaFile")
## cols = c("Species", "TaxonomyId", "Genome", "RDataClass", "Tags")


#############################################################################

## trouble when included with certain others?: "SourceSize"
## IOW, if I drop the last 6 or so above, I can get SourceSize, but
## not at the same time..
## "Toxic" fields: "RDataSize", "RDataLastModifiedDate", "RecipeArgs", 


## What I want is just call the remote metadata (and get EVERYTHING)
## extractCols <- keytypes(ah)
## Or failing that be able to do this:
## extractCols <- extractCols[!(extractCols %in% "RecipeArgs")]
## TODO: work out why I can't get everything and fix that!
## For now lets press on...
#############################################################################

.metadataCache <- new.env(parent=emptyenv())

.metadataFile <- function()
    file.path(hubCache(), "metadata.Rda")

.metadataFromCache <- function()
{
    if (!exists("meta", envir=.metadataCache))
        load(.metadataFile(), .metadataCache)
    .metadataCache[["meta"]]
}

.updateMetadata <- function(snapshotUrl) {
    ## FIXME: This could be an incremental update, but instead simply
    ## replaces the current cache.
    meta <- .metadataRemote(snapshotUrl(), columns=.ALL_COLUMNS)
    save(meta, file=.metadataFile())
    .updateSnapshotDateFile(.latestDate())
    load(.metadataFile(), .metadataCache)
}

.metadata <-
    function(snapshotUrl=snapshotUrl(), filters=list(),
             columns=.DEFAULT_COLUMNS)
{
    if (!file.exists(.metadataFile()) ||
            (.lastSnapshotDate() != .latestDate()))
        .updateMetadata(snapshotUrl)

    metadata <- .metadataFromCache()
    .filterMetadata(metadata, filters, columns)
}
## some tests
## library(AnnotationHub);ah = AnnotationHub(); system.time(m <- metadata(ah))
## res <- ah$goldenpath.hg19.encodeDCC.wgEncodeUwTfbs.wgEncodeUwTfbsMcf7CtcfStdPkRep1.narrowPeak_0.0.1.RData
##

.keytypes <-function(snapshotUrl) {
    ## url <- paste(snapshotUrl, 'getAllKeytypes', sep="/")
    ## .parseJSON(url)
    ## TEMP use the .ALL_COLUMNS (for consistency)
    .ALL_COLUMNS
}


.keys <-
    function(snapshotUrl, keytype)
{
    if (!is.character(keytype) || length(keytype) > 1L)
        stop("'keytype' must be a character vector of length 1")
    ## then retrieve values from host
    url <- paste(snapshotUrl, "getAllKeys", "keytype", keytype, sep="/")
    keys <- unique(.parseJSON(url))
    if(keytype == 'RDataDateAdded'){
        keys <- .convertDatesToStandard(keys)
    }
    keys
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


##########################################################
## Strangely enough the following seems to work:
## filters(ah) <- list(RDataPath=r)

## So does this work?  -- YES
## r2 = head(missingPaths); filters(ah) <- list(RDataPath=r2)

## which suggests that either I have 1) bad data, 2) limits on how many things can be in a URL OR 3) some other limitation on how many things can be returned...

## Does this fail?
## filters(ah) <- list(RDataPath=missingPaths)
## NO - forehead smack - This is because by the time I run this, we have already updated the metadata? - this test is suspect, try doing it after replacing metadata and date info.

## There was some interest in the fact that his does not fail.
## #filters(ah) <- list(RDataPath=missingPaths)
## BUT, this bug will NEVER be hit this way (as long as caching is in place), because the cache will always be updated BEFORE you hit this bug...



############################################################################
## Old .metadataRemote would fail like this (when the request was too big):
## cannot open: HTTP status was '414 Request-URI Too Large'
############################################################################

############################################################################
## Example of something that we should expect should work (by calling
## the other helper)
##  m <- AnnotationHub:::.metadataRemote(snapshotUrl, cols=c("RDataPath","Species"))
## And that works out OK as long as I don't include "Notes" - "Notes
## is actually NOT a real field that can come back (according to
## columns).


######################################################################
## What if I limit the cols returned to only one thing?

## table(missingPaths %in% m$RDataPath) ## TWO false values!
## fasta files for  erinaceus_europaeus and pan_troglodytes:
##[1] "ensembl/release-74/fasta/erinaceus_europaeus/dna/Erinaceus_europaeus.HEDGEHOG.74.dna_rm.toplevel.fa.rz"
##[2] "ensembl/release-74/fasta/pan_troglodytes/pep/Pan_troglodytes.CHIMP2.1.4.74.pep.all.fa.rz"              

## definitely these two are lost on server side only...
## AND they are also ON the server side (they come back if I query a
## different way)
## Hmmm.  The problem might be the contents of missingPaths are
## somehow goofy for those records.  Because if I call the above for
## the custom pathStr below it retrieves them fine...








########################################
## Lets look more closely at the bug that I just found

## pathStr <- paste("ensembl/release-69/fasta/ailuropoda_melanoleuca/cdna/Ailuropoda_melanoleuca.ailMel1.69.cdna.all.fa.rz","ensembl/release-69/fasta/ailuropoda_melanoleuca/dna/Ailuropoda_melanoleuca.ailMel1.69.dna.toplevel.fa.rz", sep=",")
## pathStr <- paste(pathStr,"ensembl/release-74/fasta/erinaceus_europaeus/dna/Erinaceus_europaeus.HEDGEHOG.74.dna_rm.toplevel.fa.rz","ensembl/release-74/gtf/anas_platyrhynchos/Anas_platyrhynchos.BGI_duck_1.0.74.gtf_0.0.1.RData", sep=",")

## And then re-run it like:

## library(httr); library(rjson); res <- content(POST("http://annotationhub.bioconductor.org/cgi-bin/R/query", body=list(BiocVersion="2.14", RDataDateAdded="2013-12-27", RDataPath=pathStr, cols="RDataPath,Species")), as="text")
## fromJSON(res)

## And that TOTALLY works.  So I am doing something wrong?




########################################################################
## Lets build a way to test this on the server:

## load('metaTestVars.Rda');
