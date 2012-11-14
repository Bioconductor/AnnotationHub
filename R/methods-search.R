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
  ## always update the objects pattern value when the user hits tab
  x@pattern <- pattern

  ## substitute for tab completion. (but leave internal values unchanged)
  paths = gsub("/", "..",x@paths)
  
  ## But in spite of all that stuff happening above,
  ## this is what we always want to return:
  grep(x@pattern, paths, value=TRUE)
}




## $ is what is called when I hit enter, so this method actually gets the data
## once we have a full path.

.getResource <- function(x, name){
  name <- gsub("\\.\\.", "/",name)
  file <- grep(name,x@paths, value=TRUE)
  ## append full URL
  file <- paste(x@curPath, file, sep="/")
  ## Assuming that we have only got one item...
  if(length(file)==1){
    ## get something
    message("Retrieving: ", file)
    objName <- load(file=url(file))
    return(get(objName))
  }else{
    ## otherwise list possible results
    message("Available results: ")
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
## 2) Make code that downloads differently for when things are not .rda files?
## 3) Plan out a way to handle oddball results (say 2000 of one thing an 2 of another...




