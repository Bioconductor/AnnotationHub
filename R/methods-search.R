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


## define method here?
.DollarNames.AnnotationHub <- function(x, pattern=""){
  grep(pattern, x@paths, value=TRUE)
}

## and this (to define a method for $ (so we can actually return something)
setMethod("$", "AnnotationHub", function(x, name){name})

