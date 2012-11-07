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


## ## define method here?
## .DollarNames.AnnotationHub <- function(x, pattern=""){
##   grep(pattern, x@paths, value=TRUE)
## }

## ## and this (to define a method for $ (so we can actually return something)
## setMethod("$", "AnnotationHub", function(x, name){name})

## .retrieveNextPathVals <- function(curPath){
##   ## whenever we get new values we set curPathExtendedYet back to FALSE
##   x@curPathExtendedYet <- FALSE 
## }




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

.extendCurPath <- function(){
  res <- .getPotentialNames(name, x@paths, x@pattern)
  ## if .getPotentialNames is length == 1, then we can save new value
  ## to curPath (paste onto current val), otherwise we just want to
  ## return the value and wait till the user gets more specific.
  if(length(res)==1){
    ## get last piece of curPath
    splitPath <- strSplit(x@curPath, ".")
    lastPiece <- splitPath[-1]
    if(!(lastPiece %in% splitPath)){
      ## this is a WEAK thing to check: foo.foo will fail!
      ## to do this right, I need to track whether or not something has been
      ## added since the last call to the web service via another variable.
      x@curPath <- paste(character, res, sep=".")
      ## TODO:
      ## Check curPathExtendedYet instead (once there is a service to ping)
      ## x@curPathExtendedYet <- TRUE 
    }
    return(res)
  }else{
    return(res)
  }
}



## This is the workhorse for the tab stuff.
## When the user hits tab, we want to complete what we can here
.DollarNames.AnnotationHub <- function(x, pattern=""){
  ## change this to store the pattern immediately
  x@pattern <- pattern

  ## then try to modify the currentPath
  ## res <- .extendCurPath

  ## And if curPathExtendedYet is TRUE, we can get the next set of path vals
  ## TODO: change this so that it retrieves from Web Service
  ## if( x@curPathExtendedYet==TRUE){
  ##   x@paths <- .retrieveNextPathVals(x@curPath)
  ## }
  
  ## This is what we return (always)
  grep(x@pattern, x@paths, value=TRUE)
}




## $ is what is called when I hit enter, so this method will have to actually
## get the data once we have a full path.
## And so this is to define a method for $ (so we can actually return something)
setMethod("$", "AnnotationHub",
          function(x, name){
            .getPotentialNames(name, x@paths, x@pattern)
          }
)








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

