## example
## obj = AnnotationHub$foo.adir.gr1.rda


### A few other helpful methods.  TODO: move these?

########################################
## Methods for exploring metatdata
#####################################


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
        if ((length(keytype) != 1L) || (!keytype %in% keytypes(x)))
            stop("'keytype' must be character(1) in 'keytypes(x)")
        ## test that the values it contains are also legit.
        keys <- value[[1]]
        idx <- keys %in% keys(x, keytype)
        if (!all(idx))
            stop("'keys' not in keys(x, ", keytype, ": ",
                 paste(sQuote(keys[!idx]), collapse=","))
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
    for (i in seq_along(filter)) {
        if (any(grepl("/", filter[i]))) {
            res[i] <- paste(filterName, paste0('%22', filter, '%22'),
                            sep='/')
        } else {
            res[i] <- paste(filterName, filter, sep='/')
        }
    }
    paste(res, collapse="/")
}

.makeURLFilters <- function(filters){
    filters <- unlist(Map(.processFilter, filters,
                              names(filters)))
    paste(filters, collapse="/")
}


######################################################################
## TODO: these need to be fixed up so that I can call:
## .getNewPathsBasedOnFilters()

## SO:
## 1) call .metadata() passing in list of filters.
## 2) get the RDataPath field out
## 3) setNames so that things are properly formatted.


## get character vector of ResourcePath values that match the keys/keytypes
.getFilesThatMatchFilters <- function(x, filters) {    
    ## get the ResourcePath for each. item that comes back from .getMetadata
    meta <- metadata(x, filters, cols="RDataPath") ## BOOM - comes back empty?
    res <- as.character(unlist(meta))
    setNames(res, make.names(res))
}

## and yet this works? (but didn't filter at all!)
## dim(metadata(ah, list(Species="Homo sapiens"), cols="RDataPath"))
## So there are "issues" with metadata...

## This function gets new @paths values based new values for @filters
## It can't just check the object for @filters though because it is needed in
## middle of change to @filters
.getNewPathsBasedOnFilters <- function(x, value) {
    if (length(value) > 0) {
        .getFilesThatMatchFilters(x, value)
    } else {                            # there are no filters
        snapshotPaths() ## MUST use the "missing" version here!
    }
}

## a= AnnotationHub()
## filters(a) <- list(TaxonomyId="9606", Title="stamConnectivity")
## Not 100% sure WHERE the warning is coming from actually...

## a filter is a combination of keys and keytypes that the user wants to
## specify that they are interested in.

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
    
    ## ALSO assign a new value to @snapshotPaths!
    x@snapshotPaths <- .getNewPathsBasedOnFilters(x, value)
    ## Finally we can return the object back
    x
}

## setReplaceMethod("filters", "AnnotationHub", .replaceFilter)




## TODO: filter removal doesn't work as expected...

## filters(a) <- filterValues
## filters(a) <- NULL  ## resets everything if I special case it
## filters(a) <- filterValues
## filters(a)[[1]] <- NULL  ## doesn't work
## filters(a)["BiocVersion"] <- NULL  ## doesn't work
## filters(a)[["BiocVersion"]] <- NULL  ## doesn't work

## TODO: make it so that the object narrows contents of urls based on filters

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



## TODO:
## 2) add unit tests for these three new methods.
