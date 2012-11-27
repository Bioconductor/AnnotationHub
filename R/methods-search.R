## This file contains methods for finishing the tab-completion and searching
## the web service for available resources.

## This is the workhorse for the tab stuff.
## When the user hits tab, we want to complete what we can here
.DollarNames.AnnotationHub <-
    function(x, pattern="")
{
    grep(pattern, names(x), value=TRUE)
}

## Helper assembles correct base path for getting files.
.getBaseServe <-
    function(x)
{
    paste(x@curPath, "serve?path=", sep="/")
}

## $ is what is called when I hit enter, so this method actually gets the data
## once we have a full path.
.getResource <-
    function(x, name) 
{
    file <- x@paths[name]
    basePath <- .getBaseServe(x)
    ## Assuming that we have only got one item...
    if(!is.na(file)) {
        ## append full URL
        file <- paste0(basePath, file)
        ## get something
        message("Retrieving: ", file)
        objName <- load(file=url(file))
        get(objName)
    } else {
        ## otherwise list possible results
        possiblePaths <- x@paths[grep(name, names(x))]
        possiblePaths <- paste0(basePath, possiblePaths)
        warning("incomplete path")
        possiblePaths
    }
}

setMethod("$", "AnnotationHub", .getResource)

## example
## obj = AnnotationHub$foo.adir.gr1.rda

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
      base <- .getBaseServe(x)
      paste0(base, x@paths)
    }
)

########################################
## Methods for exploring metatdata
#####################################

## functions to explore what is on the server
.keytypes <-
    function()
{
    fromJSON('http://wilson2.fhcrc.org/cgi-bin/R/getAllKeys')
}

setMethod("keytypes", "AnnotationHub",
    function(x)
    {
        .keytypes()
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
    url <- sprintf("%s/getAllValues?key=%s", x@curPath, keytype)
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
## http://wilson2.fhcrc.org/cgi-bin/R/query?Organism=9606&GenomeVersion=hg19

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

## helper to take a single filter and process it
.processFilter <-
    function(filter, filterName)
{
    sprintf("%s=%s", filterName, paste(filter, collapse=','))
}

## get list of metadata character vectors that match the specified
## keys/keytypes
.getMetadata <-
    function(x, filterValues)
{
    .validFilterValues(x, filterValues)
    ## and assuming we get past that, we have to now assemble a URL from the
    ## pieces. 
    filters <- unlist(Map(.processFilter, filterValues, names(filterValues)))
    filters <- paste(filters, collapse="&")
    url <- sprintf("%s/query?%s", x@curPath, filters)
    ## Concerned: that this may become too slow as more metadata piles on...
    fromJSON(url) ## returns a list with metadata for each
}

## get character vector of ResourcePath values that match the keys/keytypes
.getFilesThatMatchFilters <-
    function(x, filterValues)
{
    ## get the ResourcePath for each. item that comes back from .getMetadata
    meta <- .getMetadata(x, filterValues) ## returns a list.
    res <- sapply(meta, function(x) x[names(x) %in% "ResourcePath"])
    setNames(res, make.names(res))
}

## This function gets new @paths values based new values for @filters
## It can't just check the object for @filters though because it is needed in
## middle of change to @filters
.getNewPathsBasedOnFilters <-
    function(x, value)
{
    if (length(value > 0)) {
        newPaths <- .getFilesThatMatchFilters(x, value)
    } else {                            # there are no filters
        newPaths <- .retrievePathVals(x@curPath)
        newPaths <- setNames(newPaths, make.names(newPaths))  
    }
    newPaths
}
## TODO: this method is producing a warning: investigate that.
## TODO: once methods above exist, write some unit tests.

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

setReplaceMethod("filters", "AnnotationHub", .replaceFilter)

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
