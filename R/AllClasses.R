setClass("AnnotationHub", representation(paths="character"))


## define method here?
.DollarNames.AnnotationHub <- function(x, pattern=""){
  grep(pattern, x@paths, value=TRUE)
}

## and this (to define a method for $ (so we can actually return something)
setMethod("$", "AnnotationHub", function(x, name){name})

