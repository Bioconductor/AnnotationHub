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
  

## take a name, some paths and a pattern and returns either the single value,
## or just the set that matches.  Basically it's a grep, but IF there's a name,
## then we have to have a special case and check for a single exact match.
.getPotentialNames <- function(paths, pattern, name=NA){
  if(!is.na(name) && name %in% paths){
    return(name)
  }else{
    #return(.DollarNames.AnnotationHub(AnnotationHub, pattern) )
    return(grep(pattern, paths, value=TRUE))
  }
}


## This is the workhorse for the tab stuff.
## When the user hits tab, we want to complete what we can here
.DollarNames.AnnotationHub <- function(x, pattern=""){
  ## always update the objects pattern value when the user hits tab
  x@pattern <- pattern

  ## substitute for tab completion. (but leave internal values unchanged)
  paths = gsub("/", "..",x@paths)
  
  ## But in spite of all that stuff happening above,
  ## this is what we always want to return:
  ## grep(x@pattern, paths, value=TRUE)
  .getPotentialNames(paths=paths, pattern=x@pattern)
}




## $ is what is called when I hit enter, so this method actually gets the data
## once we have a full path.

.getResource <- function(x, name){
  name <- gsub("\\.\\.", "/",name)
  file <- .getPotentialNames(paths=x@paths, pattern=name, name=name)
  ## append full URL
  file <- paste(x@curPath, file, sep="/")
  ## Assuming that we have only got one item...
  if(length(file)==1){
    ## get something
    message("Retrieving: ", file)
    objName <- load(file=url(file))
    return(get(objName))
  }else{
    return(file) ## TODO: why all 4 when I should only have two?
  }
}

setMethod("$", "AnnotationHub",
          function(x, name){
            .getResource(x, name)
          }
)

## example
## obj = AnnotationHub$foo..adir..gr1.rda



## TODO:
## 1) figure out the bug where we list all the results if nothing matches...
## 2) make code that downloads differently for when things are not .rda files?





