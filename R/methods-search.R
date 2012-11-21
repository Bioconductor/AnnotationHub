## This file contains methods for finishing the tab-completion and searching
## the web service for available resources.

  


## This is the workhorse for the tab stuff.
## When the user hits tab, we want to complete what we can here
.DollarNames.AnnotationHub <- function(x, pattern=""){
  grep(pattern, names(x@paths), value=TRUE)
}




## $ is what is called when I hit enter, so this method actually gets the data
## once we have a full path.

.getResource <- function(x, name) {
  file <- x@paths[name]
  ## Assuming that we have only got one item...
  if(!is.na(file)){
    ## append full URL
    file <- paste(x@curPath, file, sep="/")
    ## get something
    message("Retrieving: ", file)
    objName <- load(file=url(file))
    return(get(objName))
  }else{
    ## otherwise list possible results
    possiblePaths <- x@paths[grep(name, names(x@paths))]
    possiblePaths <- paste(x@curPath, possiblePaths, sep="/")
    warning("incomplete path")
    return(possiblePaths)
  }
}

setMethod("$", "AnnotationHub", .getResource)

## example
## obj = AnnotationHub$foo.adir.gr1.rda









########################################
## Methods for exploring metatdata
#####################################
## functions to explore what is on the server
.keytypes <- function(){
  fromJSON('http://wilson2.fhcrc.org/cgi-bin/R/getAllKeys')
}

setMethod("keytypes", "AnnotationHub",
          function(x){
            .keytypes()
          })


## Need a method to list possible values for a given metadata keytype
.keys <- function(x, keytype){
  if(!is.character(keytype) || length(keytype)>1) stop("keytype must be a character vector of length 1")
  if(!any(keytype %in% keytypes(x))) stop("keytype must be an actual keytype, please call listKeytypes() for viable options.")
  ## then retrieve values from server
  baseUrl <- 'http://wilson2.fhcrc.org/cgi-bin/R/getAllValues?key='
  url <- paste0(baseUrl, keytype)
  unique(fromJSON(url))
}

setMethod("keys", "AnnotationHub",
  function(x, keytype){
    if(missing(keytype)) keytype <- "Organism"
    .keys(x, keytype)
  }
)


## Before I go any further I need helper methods that will get me a list of
## files based on only the metadata:


## So a URL like this allows me to get info based on keytypes and keys.
## http://wilson2.fhcrc.org/cgi-bin/R/query?Organism=9606&GenomeVersion=hg19

## return true if filter is valid
.validFilterValue <- function(filter, filterName, x){
  ## test if it is named and if the name is legit.
  keytype = filterName
  if(!any(keytypes(x) %in% keytype)) stop("Keytypes for the filter must be an actual keytype.  Please call the keytypes() method to list viable options")
  ## test that the values it contains are also legit.
  keys = filter[[1]]
  if(!any(keys(x, keytype) %in% keys)) stop("Keys for the filter must be an actual keys.  Please call the keys() method to see viable options.")
}

## Test:
## library(AnnotationHub)
## a = AnnotationHub()
## filterValues <- list();filterValues[[1]] <- keys(a, "Organism");filterValues[[2]] <- keys(a, "BiocVersion");names(filterValues) <- c("Organism","BiocVersion")

## helper to take a single filter and process it
.processFilter <- function(filter, filterName){
  filters <- paste(filter,collapse=',')
  paste(filterName,'=',filters,'', sep="")
}


## get list of metadata character vectors that match the specified keys/keytypes
.getMetadata <- function(filterValues, x){
  baseUrl <- 'http://wilson2.fhcrc.org/cgi-bin/R/query?'
  ## Map lets me process on both names and contents of a list.
  Map(.validFilterValue, filterValues, names(filterValues), MoreArgs=list(x=x))
  ## and assuming we get past that, we have to now assemble a URL from the
  ## pieces. 
  filters <- unlist(Map(.processFilter, filterValues, names(filterValues)))
  filters <- paste(filters, collapse="&")
  url <- paste0(baseUrl, filters)
  ## Concerned: that this may become too slow as more metadata piles on...
  fromJSON(url) ## returns a list with metadata for each
}

## get character vector of ResourcePath values that match the keys/keytypes
.getFilesThatMatchFilters <- function(filterValues, x){
  ## get the ResourcePath for each. item that comes back from .getMetadata
  meta <- .getMetadata(filterValues, x) ## returns a list.
  res <- unlist(lapply(meta, function(x){x[names(x) %in% "ResourcePath"]}))
  res <- setNames(res, make.names(res))
  res
}


## This function gets new @paths values based new values for @filters
## It can't just check the object for @filters though because it is needed in
## middle of change to @filters
.getNewPathsBasedOnFilters <- function(x, value){
  if(length(value >0)){
    newPaths <- .getFilesThatMatchFilters(value, x)
  }else{ ## meaning there are no filters
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
  function(x){
    x@filters
  }
)
## filters(a)


## Setter method to set filters
## This method needs to be cumulative...  So whatever is currently in the slot
## needs to be added to (as appropriate) by what is coming in via values.
.replaceFilter <- function(x, value){
##   if (!is.null(value)) {
    ## Then check
  Map(.validFilterValue, value, names(value), MoreArgs=list(x=x))
  ## If legit, then we want to add that to existing vals
  if(length(value) > 0){ ## if there is anything in value, then merge it in...
    curFilters <- x@filters
    curNames <- names(curFilters)
    newNames <- names(value)
    ## drop any repeats from the old set 
    curFilters <- curFilters[!(curNames %in% newNames)] 
    value <- c(curFilters, value) ## append
  }else{ ## user signaled that they want it wiped out.
    value <- list()
  }
  x@filters <- value ## assign
  ## If values are now empty, then remove that filter from the list...
  lens <- (unlist(lapply(x@filters, function(x){length(x)!=0})))
  x@filters <-  x@filters[lens]
  
  ## ALSO assign a new value to @paths!
  x@paths <- .getNewPathsBasedOnFilters(x, value)
  ## Finally we can return the object back
  x
}
  
  
setReplaceMethod("filters", "AnnotationHub",
                 function(x,value){.replaceFilter(x,value)})

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
