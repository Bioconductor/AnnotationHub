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
.validFilterValue <- function(){

}

## get list of metadata character vectors that match the specified keys/keytypes
.getMedata <- function(filterValues){
  baseUrl <- 'http://wilson2.fhcrc.org/cgi-bin/R/query?'
  
  
}

## get character vector of ResourcePath values that match the keys/keytypes
.getFilesThatMatchFilters <- function(){

}


## TODO: once methods exist, write some unit tests.


## functions to modify our object:

## And then I can also do like this to get ALL the metadata based on certain
## keys.  All I really need is to get just the URL or file fields from this so
## I can limit the search of my object...  But this is really cool.  It
## basically means that I can use this below with the filter function to
## narrow the results of the object.
## filter(obj, c(GenomeVersion='hg19'))



## A method to show all filters currently active
## keytypes(obj)

## A method to 



## Setter method to set values:
## keys(obj) <- NULL
## keys(obj) <- c(Type=NULL)




## TODO: make it so that the object narrows contents of paths based on filters
