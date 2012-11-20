## This file contains methods for finishing the tab-completion and searching
## the web service for available resources.


## .retrievePathVals is for listing the items from the web server.
.retrievePathVals <- function(curPath){
  ## look for values
  paths <- fromJSON(curPath)
  ## if there are some new values we set curPathExtendedYet back to FALSE
  if(length(paths) > 0 && exists("x")){
    x@curPathExtendedYet <- FALSE
  }
  ## return the options...
  paths
}
  


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
## obj = AnnotationHub$foo..adir..gr1.rda









########################################
## Methods for exploring metatdata
#####################################
## functions to explore what is on the server
## listFilters() method to show all filters that can be used
## listFilters()
## http://wilson2.fhcrc.org/cgi-bin/R/getAllKeys
listFilters <- function(){
  fromJSON('http://wilson2.fhcrc.org/cgi-bin/R/getAllKeys')
}


## listFilterValues("filter") method to list values that can be used by a
## filter of interest.
## listFilterValues("filter")
## http://wilson2.fhcrc.org/cgi-bin/R/getAllValues?key=Title

listFilterValues <- function(filter){
  if(!is.character(filter) || length(filter)>1) stop("filter must be a character vector of length 1")
  if(!any(filter %in% listFilters())) stop("filter must be an actual filter, please call listFilters() for viable options.")
  
  baseUrl <- 'http://wilson2.fhcrc.org/cgi-bin/R/getAllValues?key='
  url <- paste0(baseUrl, filter)
  res <- fromJSON(url)
  ## need to call unique, but we still want to keep the names (which are also
  ## redundant)  So instead we use duplicated.es
  res[!(duplicated(res))]
}


## And then I can also do like this to get ALL the metadata based on certain
## keys.  All I really need is to get just the URL or file fields from this so
## I can limit the search of my object...  But this is really cool.  It
## basically means that I can use this below with the filter function to
## narrow the results of the object.
## filter(obj, c(GenomeVersion='hg19'))
## http://wilson2.fhcrc.org/cgi-bin/R/query?Organism=9606&GenomeVersion=hg19


## functions to modify our object:
## activeFilters(obj) method to show all filters currently active
## activeFilters(obj)

## TODO: make it so that the object narrows contents of paths based on filters


## removeFilters(obj, filters=c("foo","bar")) method to remove filters from
## the object
## removeFilters(obj, filters=c("foo","bar"))

