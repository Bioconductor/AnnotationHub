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



## TODO:
## 2) Make code that downloads differently for when things are not .rda files?
## 3) Plan out a way to handle oddball results (say 2000 of one thing an 2 of another...
