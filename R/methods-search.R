## This file contains methods for finishing the tab-completion and searching
## the web service for available resources.


## Need to overload $

## There is apparently a way so that when $ is hit a function is called (maybe
## just in the $ method?) so that I can list the results.  Not sure how this
## gets the tab completion tho.

## High concept is that we want the tab completion to work via callbacks to
## the web service at each point (after each ".").

## ?completion
## .DollarNames

## and also define $ method for our object.

## > A = setClass("A", representation(paths="character"))
## > a = A(paths=c("foo", "bar", "baz"))
## > .DollarNames.A = function(x, pattern="") grep(pattern, x@paths, value=TRUE)
## > a
## An object of class "A"
## Slot "paths":
## [1] "foo" "bar" "baz"

## > a$ba
## a$bar  a$baz
## > getGeneric("$")
## standardGeneric for "$" defined from package "base"

## function (x, name)
## standardGeneric("$", .Primitive("$"))
## <environment: 0x450d858>
## Methods may be defined for arguments: x
## Use  showMethods("$")  for currently available ones.
## > setMethod("$", "A", function(x, name) name)


#### ## define method here?
#### .DollarNames.AnnotationHub <- function(x, pattern=""){
####   grep(pattern, x@paths, value=TRUE)
#### }




## ## and this (to define a method for $ (so we can actually return something)
## setMethod("$", "AnnotationHub", function(x, name){name})

.retrieveNextPathVals <- function(curPath){
  ## look for values
  paths <- fromJSON(curPath)
  ## if there are some new values we set curPathExtendedYet back to FALSE
  if(length(paths) > 0 && exists("x")){
    x@curPathExtendedYet <- FALSE
  }
  ## return the options...
  paths
}
  



## take a name, some paths and a pattern and returns either the single value, or the set that matches.
.getPotentialNames <- function(name, paths, pattern){
  if(name %in% paths){
    return(name)
  }else{
    #return(.DollarNames.AnnotationHub(AnnotationHub, pattern) )
    return(grep(pattern, paths, value=TRUE))
  }
}

## This code attempts to extend the curPath variable based on what is present
## and what is reasonable.

## .extendCurPath <- function(x, name, paths){
##   ## how many paths?
##   res <- .getPotentialNames(name, paths, x@pattern)
##   ## there is only one choice left
##   if(length(res)==1){ 
##     if(x@curPathExtendedYet==FALSE){ ## and we have not done so already
##       ## then append to the curPath
##       x@curPath <- paste(character, res, sep="")
##       ## then mark it as extended
##       x@curPathExtendedYet <- TRUE 
##     }
##   }
## }


## debug(AnnotationHub:::.DollarNames.AnnotationHub)

## This is the workhorse for the tab stuff.
## When the user hits tab, we want to complete what we can here
.DollarNames.AnnotationHub <- function(x, pattern=""){
  ## always update the objects pattern value when the user hits tab
  x@pattern <- pattern

##   ## And if curPathExtendedYet is TRUE, we can get the next set of path vals
##   ## TODO: change this so that it retrieves from Web Service
##   if( x@curPathExtendedYet==TRUE){
##     newPaths <- .retrieveNextPathVals(x@curPath)
##   }else{
##     newPaths <- x@paths
##   }

##   ## then try to modify the currentPath
##   .extendCurPath(x, name=pattern, newPaths)


  ## substitute for tab completion. (but leave internal values unchanged)
  paths = gsub("/", "..",x@paths)
  
  ## But in spite of all that stuff happening above,
  ## this is what we always want to return:
  grep(x@pattern, paths, value=TRUE)
}




## $ is what is called when I hit enter, so this method will have to actually
## get the data once we have a full path.
## And so this is to define a method for $ (so we can actually return something)

.getResource <- function(x, name){
  name <- gsub("\\.\\.", "/",name)
  file <- .getPotentialNames(name, x@paths, x@pattern)
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






## changes:
## .DollarNames() needs to get values from a web service.  How will it know
## which set of IDs to use? There will have to be a helper that requests the
## IDs, and this will have to "know" the path up to the current point. I will
## need a default (base dir) to get started.

## $ will need to basically do what it already does.  So when I hit enter, it
## needs to return a reasonable answer OR complain that we are not all the way
## there yet and call grep on what we have so far vs. the remaining
## possibilities...  $ needs to insist that the string it acts on is a full
## string in the current set retrieved from the web server, so it also needs
## access to the current set of paths.

## So to do this kind of incremental path build up, I need to store a couple
## variables. I think it's natural to stick those in the object (even if that
## is not very R-ish of me).

## the two things are the list of characters that .DollarNames and $ are
## checking against (or updating if it's .DollarNames).  And also the "path so
## far".  Default on path so far should probably be "resource" (the base URL).

