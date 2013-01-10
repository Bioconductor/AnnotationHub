## This file contains methods for finishing the tab-completion and searching
## the web service for available resources.


## One of the most common things we need to do is to replace URL
## separated paths with equivalent ones from the local FS
.reformatFilePath <- function(filePath){
    paste(.splitFilePath(filePath), collapse=.Platform$file.sep)
}

## This is the workhorse for the tab stuff.
## When the user hits tab, we want to complete what we can here
.DollarNames.AnnotationHub <- function(x, pattern=""){
    grep(pattern, names(x), value=TRUE)
}

## Helper assembles correct base path for getting online files.  NOTE:
## this curPath does NOT need to have date or version information.
.getBaseServe <- function(){
    paste(.getServer() ,"ah", "resources", sep="/")
}


## the more specific path for stuff from the local cache
.localCacheDir <- function(x){
    file.path(.baseUserDir(), as.character(x@versionString),
              versionDate(x), "resources")
}


## helper to split file path in platform independent way
.splitFilePath <- function(path){
    fsep <- .Platform$file.sep
    unlist(strsplit(path, split=fsep))
}

## This should help us to get the file path sorted so that we can save it.
.createFilePathIfNeeded <- function(path){
    pathVec <- .splitFilePath(path) ## split
    pathVec <- pathVec[-length(pathVec)] ## and remove actual file
    ## then I need to just make any dirs that don't exist already.
    posPaths <- character()
    for(i in seq_len(length(pathVec))){
        posPaths[[i]] <- paste(pathVec[1:i], collapse= .Platform$file.sep)
    }
    ## And then I have to make sure 100% of the possible paths are created.
    ## And yes this has to be done in sequence...  :P
    for(i in seq_len(length(posPaths))){
        if(!file.exists(posPaths[i])){
            dir.create(posPaths[i])
        }
    }
}


## is the file cached?  Lets look and see
.isTheFileInCache <- function(x, file){
    ## Then we have to test if the file is cached already...
    cacheFile <- file.path(.localCacheDir(x), .reformatFilePath(file))
    !is.na(file.info(cacheFile)[1])
}

## what should we use as a path?
.chooseForeignOrLocalFileSource <- function(x, file){
    if(x@cachingEnabled == TRUE && .isTheFileInCache(x, file) == TRUE){
        ## is caching enabled AND is the file ALSO present?
        basePath <- .localCacheDir(x)
    }else{
        basePath <- .getBaseServe()
    }
    basePath
}


## $ is what is called when I hit enter, so this method actually gets the data
## once we have a full path to it.
.getResource <- function(x, name){
    file <- x@paths[name]   
    ## Set up the basePath based on caching.
    basePath <- .chooseForeignOrLocalFileSource(x, file)
    
    ## Assuming that we have only got one item...
    if(!is.na(file)) {
        ## append full URL
        filePath <- paste(basePath, file, sep="/")
        ## load it
        message("Retrieving: ", filePath)
        if(x@cachingEnabled == FALSE || .isTheFileInCache(x, file) == FALSE){
            objName <- load(file=url(filePath))
        }else{
            objName <- load(file=.reformatFilePath(filePath))
        }
        ## then get it        
        obj <- get(objName)
        
        ## for platform independence, reformat file string to match FS
        localFile <- .reformatFilePath(file)
        ## Then see if we need to interact with local FS for caching
        if(x@cachingEnabled == TRUE && .isTheFileInCache(x, localFile) ==FALSE){
            ## only save if we are using cache AND file is not saved yet
            ## localPath of interest will NOW have to be the local one
            localPath <- file.path(.localCacheDir(x), localFile)
            ## make sure that the dir exists.
            .createFilePathIfNeeded(localPath)
            save(obj,file=localPath)
        }
        obj
    } else {
        ## otherwise list possible results
        possiblePaths <- x@paths[grep(name, names(x))]
        possiblePaths <- paste(basePath, possiblePaths, sep="/")
        warning("incomplete path")
        possiblePaths
    }
}

setMethod("$", "AnnotationHub", function(x, name) .getResource(x, name) )


## example
## obj = AnnotationHub$foo.adir.gr1.rda


### A few other helpful methods.  TODO: move these?
setMethod("names", "AnnotationHub",
    function(x)
    {
        names(x@paths)
    }
)

setMethod("length", "AnnotationHub",
    function(x)
    {
        length(x@paths)
    }
)

setMethod("urls", "AnnotationHub",
    function(x)
    {
      base <- .getBaseServe()
      paste0(base, x@paths)
    }
)

########################################
## Methods for exploring metatdata
#####################################

## functions to explore what is on the server
.keytypes <-function(x){
    fromJSON(paste0(x@curPath,'/getAllKeytypes'))
}

setMethod("keytypes", "AnnotationHub",
    function(x)
    {
        .keytypes(x)
    }
)

## Need a method to list possible values for a given metadata keytype
.keys <-
    function(x, keytype)
{
    if(!is.character(keytype) || length(keytype)>1)
        stop("'keytype' must be a character vector of length 1")
    if(!any(keytype %in% keytypes(x)))
        stop("invalid 'keytype', see keytypes()")
    ## then retrieve values from server
    url <- paste(x@curPath, "getAllKeys/keytype", keytype, sep="/")
    unique(fromJSON(url))
}

setMethod("keys", "AnnotationHub",
    function(x, keytype)
    {
        if(missing(keytype))
            keytype <- "Organism"
        .keys(x, keytype)
    }
)

## Before I go any further I need helper methods that will get me a list of
## files based on only the metadata:

## So a URL like this allows me to get info based on keytypes and keys.
## http://wilson2.fhcrc.org/ah/query/Organism/9606/GenomeVersion/hg19

## return true all filters valid
.validFilterValues <-
    function(x, filters)
{
    for (i in seq_along(filters)) {
        filterName <- names(filters)[[i]]
        value <- filters[[i]]
        ## test if it is named and if the name is legit.
        keytype <- filterName
        if ((length(keytype) != 1) || (!keytype %in% keytypes(x)))
            stop("'keytype' must be character(1) in 'keytypes(x)")
        ## test that the values it contains are also legit.
        keys <- value[[1]]
        if (!all(keys %in% keys(x, keytype)))
            stop("'keys' not in keys(x, ", keytype, ": ",
                 paste(sQuote(keys[!keys %in% keys(x, keytype)]), sep=","))
    }
    TRUE
}

## Test:
## library(AnnotationHub)
## a = AnnotationHub()

## filterValues <- list();filterValues[[1]] <- keys(a, "Organism")
## filterValues[[2]] <- keys(a, "BiocVersion")
## names(filterValues) <- c("Organism","BiocVersion")

## helper to take a single filterName and process it
.processFilter <-
    function(filter, filterName)
{
    res <- character()
    for(i in seq_along(filter)){
        if(grepl("/", filter[i])){
            res[i] <- paste(filterName,paste('%22',filter,'%22',sep=""),sep='/')
        }else{
            res[i] <- paste(filterName,filter,sep='/')
        }
    }
    paste(res,collapse="/")
}

## get list of metadata character vectors that match the specified
## keys/keytypes
.getMetadata <-
    function(x, filterValues)
{
    if(length(filterValues) == 0){
        return(fromJSON(paste0(x@curPath,"/query")))
    } else {
        .validFilterValues(x, filterValues)
        ## and assuming we get past that, we have to now assemble a URL from
        ## the pieces.
        filters <- unlist(Map(.processFilter, filterValues,
                              names(filterValues)))
        filters <- paste(filters, collapse="/")
        url <- paste(x@curPath, "query", filters, sep="/") ## vectorized?
        ## Concerned: that this may become too slow as more metadata piles on.
        return(fromJSON(url)) ## returns a list with metadata for each
    }
}

## get character vector of ResourcePath values that match the keys/keytypes
.getFilesThatMatchFilters <- function(x, filterValues) {
    ## get the ResourcePath for each. item that comes back from .getMetadata
    meta <- .getMetadata(x, filterValues) ## returns a list.
    res <- unlist(sapply(meta, function(x) x[names(x) %in% "RDataPath"]))
    setNames(res, make.names(res))
}

## This function gets new @paths values based new values for @filters
## It can't just check the object for @filters though because it is needed in
## middle of change to @filters
.getNewPathsBasedOnFilters <- function(x, value) {
    if (length(value) > 0) {
        newPaths <- .getFilesThatMatchFilters(x, value)
    } else {                            # there are no filters
        newPaths <- .retrievePathVals(x@curPath)
        newPaths <- setNames(newPaths, make.names(newPaths))  
    }
    newPaths
}
## a= AnnotationHub()
## filters(a) <- list(TaxonomyId="9606", Title="stamConnectivity")
## Not 100% sure WHERE the warning is coming from actually...

## a filter is a combination of keys and keytypes that the user wants to
## specify that they are interested in.

## A method to extact the currently set filters
setMethod("filters", "AnnotationHub",
    function(x) {
        x@filters
    }
)
## filters(a)

## Setter method to set filters
## This method needs to be cumulative...  So whatever is currently in the slot
## needs to be added to (as appropriate) by what is coming in via values.
.replaceFilter <-
    function(x, value)
{
    ##   if (!is.null(value)) {
    ## Then check
    .validFilterValues(x, value)
    ## If legit, then we want to add that to existing vals
    if (length(value) > 0) { ## if there is anything in value, merge it in...
        curFilters <- x@filters
        curNames <- names(curFilters)
        newNames <- names(value)
        ## drop any repeats from the old set 
        curFilters <- curFilters[!(curNames %in% newNames)] 
        value <- c(curFilters, value) ## append
    } else { ## user signaled that they want it wiped out.
        value <- list()
    }
    ## If values are now empty, then remove that filter from the list...
    x@filters <-  value[sapply(value, length) != 0]
    
    ## ALSO assign a new value to @paths!
    x@paths <- .getNewPathsBasedOnFilters(x, value)
    ## Finally we can return the object back
    x
}

## setReplaceMethod("filters", "AnnotationHub", .replaceFilter)

## YES. I know this is more verbose.
## But please leave it so I can debug things later...
setReplaceMethod("filters", "AnnotationHub",
                 function(x, value){.replaceFilter(x, value)})



## TODO: filter removal doesn't work as expected...

## filters(a) <- filterValues
## filters(a) <- NULL  ## resets everything if I special case it
## filters(a) <- filterValues
## filters(a)[[1]] <- NULL  ## doesn't work
## filters(a)["BiocVersion"] <- NULL  ## doesn't work
## filters(a)[["BiocVersion"]] <- NULL  ## doesn't work

## TODO: make it so that the object narrows contents of paths based on filters

## Test:
## library(AnnotationHub)
## a = AnnotationHub()
## filterValues <- list();filterValues[[1]] <- keys(a, "Organism");filterValues[[2]] <- keys(a, "BiocVersion");names(filterValues) <- c("Organism","BiocVersion")
## filters(a) <- filterValues  ## narrows things down a bit... (though maybe it shouldn't)

## filterValues2 <- list(File=c("all.footprints.gz"))
## filters(a) <- filterValues2




## Add metadata method here
## TODO: this metadata needs some controls on it.  For example: "GRanges" is
## NOT a valid tax ID)

setMethod("metadata", "AnnotationHub",
          function(x){
            res <- .getMetadata(x, x@filters)
            ## TODO: make res into a data.frame()
            data.frame(do.call(rbind,res))
          })






## A method to extact the currently set date
setMethod("versionDate", "AnnotationHub",
    function(x) {
        x@dateString
    }
)
## date(a)

## exporting this function as a method too...
.possibleDates <- function(){
    version <- BiocInstaller:::BIOC_VERSION
    url <- paste(paste0(.getServer(),"/ah"),version,"getSnapshotDates",sep="/")
    fromJSON(url)
}
setMethod("possibleDates", "AnnotationHub", function(x) .possibleDates() ) 
## possibleDates(x)


## Setter method to set date (also needs to adjust the curPath)
.replaceVersionDate <-
    function(x, value)
{
    possibleDates <- .possibleDates()
    if(value %in% possibleDates && value != x@dateString){
        x@dateString<- value
        ## also update the curPath.
        curPath <-  .baseCurPath()
        versionString <- .getCurVersion()
        dateString <- value
        x@curPath <- paste(curPath, versionString, dateString, sep="/")
        ## I also need to update @paths because different paths might
        ## be available depending on the version date.  But this is
        ## tricky, because I also want to try and keep their previous
        ## filters...
        ## 1st trap their current filters
        filters = x@filters
        ## then get paths assuming a fresh start.
        locPaths <- .getNewPathsBasedOnFilters(x, NULL)
        ## then ammend those to use the saved filters.
        locPaths <- .getNewPathsBasedOnFilters(x, filters)
        ## finally put those paths into @paths
        if(!is.null(locPaths[[1]])){
            x@paths <- locPaths
        }else{
            stop("There is not any data available for that version date.")
        }
    }else{
        stop("value must be an actual snapshot date that is not currently being used.  See possibleDates() for viable snapshot dates.")
    }
    x
}

setReplaceMethod("versionDate", "AnnotationHub",
                 function(x, value){.replaceVersionDate(x, value)})


## TODO:
## 2) add unit tests for these three new methods.
