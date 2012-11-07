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

## .retrieveNextPathVals <- function(curPath){}

## define method here?
.DollarNames.AnnotationHub <- function(x, pattern=""){
  ## change this to store the pattern immediately
  x@pattern <- pattern
  ## change this so that it retrieves from Web Service
  ## x@paths <- .retrieveNextPathVals(x@curPath)
  grep(x@pattern, x@paths, value=TRUE)
}


.validateName <- function(name, paths, pattern){
  if(name %in% paths){
    return(name)
  }else{
    return(.DollarNames.AnnotationHub(AnnotationHub, pattern) )
  }
}

## and this (to define a method for $ (so we can actually return something)
setMethod("$", "AnnotationHub",
          function(x, name){
            .validateName(name, x@paths, x@pattern)
            #name
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

