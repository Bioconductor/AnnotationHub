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


## make this more general, filters will be a list...
.parseJSONMetadataListViaPOST <- function(filters){
    url <- "http://annotationhub.bioconductor.org/cgi-bin/R/query"
    BiocVersion=unlist(strsplit(snapshotVersion(), "/"))[1]
    RDataDateAdded=as.character(.getLastSnapShotDate())    
    res <- content(POST(url,
                        body=c(BiocVersion=BiocVersion,
                          RDataDateAdded=RDataDateAdded,
                          filters)), as="text")
    j <- fromJSON(res)
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
    urls <- unlist(.metadata(snapshotUrl, cols="RDataPath"))
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
## helper to standardize snapshotLoc
.snapshotLoc <- function(){
    file.path(hubCache(),"snapshotDate.Rda")
}

## helper to save snapShotdate to cache
.saveLatestSnapShotDate <- function(date) {
    snapshotLoc <- .snapshotLoc() 
    save(date, file=snapshotLoc)
}

## helper to get latest SnapshotDate from cache. (only works if
## caching is enabled)
.getLastSnapShotDate <- function() {
    snapshotLoc <- .snapshotLoc()    
    if(file.exists(snapshotLoc)){
        ## Filename shenanigans! (in case the file gets renamed on us)
        load(snapshotLoc)
        objName <- load(snapshotLoc)
        date <- eval(parse(text=objName))
        return(date)
    }else{
        date <- snapshotDate()
        .saveLatestSnapShotDate(date)
        return(date)
    }
}

## metadata takes a filter list and cols and returns a DataFrame
.metadataRemote <- function(snapshotUrl, filters=list(),
                            cols=c("Title","Species",
                              "TaxonomyId","Genome","Description",
                              "Tags","RDataClass","RDataPath")) {  
    ## format cols
    colsVec <- paste("cols",cols, sep="/", collapse="/")
    ## then make a url
    if (length(filters)>0 && filters!="" &&
        !is.null(filters)) { ## get some
        ## URL must be specific
        filters <- lapply(filters,paste,collapse=",")

        ## Append the cols as one more list element
        filters <- c(filters,cols=paste(cols, collapse=","))
        
        ## make url and body
        meta <- .parseJSONMetadataListViaPOST(filters)
    } else {## get all of them (use get method)
        url <- paste(snapshotUrl, "query", colsVec, sep="/")
        meta <- .parseJSONMetadataListViaGET(url)
    }   
    ## make a DataFrame
    .toDataFrame(meta)
}

## helper for filtering a Data.frame locally
.filterMeta <- function(meta, filters, cols){
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
    meta[,cols,drop=FALSE]
}
## example test filter and cols:
## filters = list(Species=c("Homo sapiens","Mus musculus"), RDataClass="FaFile")
## cols = c("Species", "TaxonomyId", "Genome", "RDataClass", "Tags")


#############################################################################
## TEMP list of all cols we can extract...
extractCols <- c("BiocVersion","DataProvider","Title","SourceFile",
                 "Species","SourceUrl","SourceVersion",
                 "TaxonomyId","Genome","Description",
                 "Tags","RDataClass","RDataPath", ## new stuff follows
                 "Coordinate_1_based","Maintainer",
                 "RDataVersion","RDataDateAdded","Recipe")

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


## This is what we will call when we pull data only from the cache.
## It gets metadata from local cache - ALL fields will be present so I
## need to remove fields that I are not in the list of cols requested.
.metadataLocal <- function(snapshotUrl,
                           filters=list(),
                           cols=c("Title","Species",
                             "TaxonomyId","Genome","Description",
                             "Tags","RDataClass","RDataPath")){
    metadataLoc <- file.path(hubCache(),"metadata.Rda")
    if(file.exists(metadataLoc)){
        ## then load it etc.
        load(metadataLoc)
        ## then filter it
        .filterMeta(meta, filters, cols)
    }else{ ## 1st time case where there isn't any local data yet.
        meta <- .metadataRemote(snapshotUrl, cols=extractCols)
        ## then save it
        save(meta, file=metadataLoc)
        .filterMeta(meta, filters, cols)
    }
}

##library(AnnotationHub); ah = AnnotationHub(); debug(AnnotationHub:::.updateLocalMetadata); debug(AnnotationHub:::.parseJSONMetadataList); metadata(ah)

## Called when we need to update the LocalMetadata Cache
.updateLocalMetadata <- function(snapshotUrl, filters=filters, cols=cols){

    ## TODO: work out why the following fails to work (why we can't
    ## just get the records that we want using the RDataPath filter) -
    ## and why we have to instead get ALL the records to replace them
    ## instead of being able to increment them.
    
    ## remotePaths <- as.character(t(as.data.frame(.metadataRemote(snapshotUrl,
    ##                                                  cols="RDataPath"))))
    ## localPaths <- as.character(t(as.data.frame(.metadataLocal(snapshotUrl,
    ##                                                 cols="RDataPath"))))
    ## missingPaths <- setdiff(remotePaths, localPaths)
    ## 
    ## ##Then get all the data for the paths we don't have yet and update
    ## missingData <- .metadataRemote(snapshotUrl,
    ##                                filters=list(RDataPath=missingPaths),
    ##                                cols=extractCols)
    ## metadataLoc <- file.path(hubCache(),"metadata.Rda")
    ## load(metadataLoc)
    ## meta <- rbind(meta,missingData)
    ## save(meta, file=metadataLoc)
    
    ## OK so for now, just get ALL the data, and then just replace it.
    meta <- .metadataRemote(snapshotUrl,
                               cols=extractCols)
    metadataLoc <- file.path(hubCache(),"metadata.Rda")
    save(meta, file=metadataLoc)

    
    ## Continuing on:
    ## Then filter so that what comes back is properly subsetted
    meta <- .filterMeta(meta, filters, cols)
    ## Don't forget to also update (save) local snapshotDate as well
    .saveLatestSnapShotDate(.latestDate())
    meta
}


.metadata <- function(snapshotUrl=snapshotUrl(), filters=list(),
                      cols=c("Title","Species",
                        "TaxonomyId","Genome","Description",
                        "Tags","RDataClass","RDataPath"), useWebOnly=FALSE) {
    if(useWebOnly==TRUE){## For internal testing only
        .metadataRemote(snapshotUrl, filters, cols)
    }else{
        if(.getLastSnapShotDate() == .latestDate()){
            .metadataLocal(snapshotUrl, filters=filters, cols=cols)
        }else{ ## the dates don't match... So update!
            .updateLocalMetadata(snapshotUrl, filters=filters, cols=cols)
        }
    }
}
## some tests
## library(AnnotationHub);ah = AnnotationHub(); system.time(m <- metadata(ah))
## res <- ah$goldenpath.hg19.encodeDCC.wgEncodeUwTfbs.wgEncodeUwTfbsMcf7CtcfStdPkRep1.narrowPeak_0.0.1.RData
##
## web only:
## library(AnnotationHub);ah = AnnotationHub(); system.time(m <- AnnotationHub:::.metadata(snapshotUrl(ah), useWebOnly=TRUE))


.keytypes <-function(snapshotUrl) {
    ## url <- paste(snapshotUrl, 'getAllKeytypes', sep="/")
    ## .parseJSON(url)
    ## TEMP use the extractCols (for consistency)
    extractCols
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
    .metadata(snapshotUrl(), ...)
})





## TODO: work out why I can't get all the metadata types back from the
## DB when calling .metadataRemote()

## this doesn't work (requires old metadata to attempt):
## snapshotUrl <- "http://annotationhub.bioconductor.org/ah/2.14/1.3.13/2013-12-27"
## remotePaths <- as.character(t(as.data.frame(AnnotationHub:::.metadataRemote(snapshotUrl, cols="RDataPath"))))
## localPaths <- as.character(t(as.data.frame(AnnotationHub:::.metadataLocal(snapshotUrl, cols="RDataPath"))))
## missingPaths <- setdiff(remotePaths, localPaths)

##  m <- AnnotationHub:::.metadataRemote(snapshotUrl, filters=list(RDataPath=missingPaths), cols=extractCols)


##########################################################
## Strangely enough the following seems to work:
## filters(ah) <- list(RDataPath=r)

## So does this work?  -- YES
## r2 = head(missingPaths); filters(ah) <- list(RDataPath=r2)

## AND THIS WORKS ALSO:
## m <- AnnotationHub:::.metadataRemote(snapshotUrl, filters=list(RDataPath=r2), cols=extractCols)

## But this doesn't work:
## m <- AnnotationHub:::.metadataRemote(snapshotUrl, filters=list(RDataPath=missingPaths), cols=extractCols)

## which suggests that either I have 1) bad data, 2) limits on how many things can be in a URL OR 3) some other limitation on how many things can be returned...

## Does this fail?
## filters(ah) <- list(RDataPath=missingPaths)
## NO - forehead smack - This is because by the time I run this, we have already updated the metadata? - this test is suspect, try doing it after replacing metadata and date info.

## So this should work (with defaults)? - NO IT DOES NOT...  Hmmm...
## m <- AnnotationHub:::.metadataRemote(snapshotUrl, filters=list(RDataPath=missingPaths))

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
## library(AnnotationHub); load('metaTestVars.Rda'); ah = AnnotationHub(); debug(AnnotationHub:::.metadataRemote); debug(AnnotationHub:::.toDataFrame); debug(AnnotationHub:::.parseJSONMetadataListViaPOST)

## pathStr <- missingPaths[1:2]
## m <- AnnotationHub:::.metadataRemote(snapshotUrl, filters=list(RDataPath=pathStr), cols="RDataPath")
## Seems to work OK now, but now I seem to be missing some rows (rows dissappear on the server side?) - and this seems to be a real problem.  That is:


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






