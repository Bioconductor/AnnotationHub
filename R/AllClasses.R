## Some internal value are used for knowing what this object "points" to...
## curPath = holds the path that the user has chosen "so far".

## paths = holds character vector of possible paths to choose from.

## pattern = what tab completion has guess so far based on the paths and the
## user interaction.

## keytypes = named list of character vectors that holds the keytypes (names)
## and also their acceptable values

setClass("AnnotationHub", representation(curPath = "character",
                                         paths="character",
                                         pattern="character",
                                         keytypes="list"))




## constructor
AnnotationHub <- function(
              curPath= "http://wilson2.fhcrc.org/cgi-bin/R/AnnotationHub"){
  paths <- .retrievePathVals(curPath)
  paths <- setNames(paths, make.names(paths))
  AnnotationHub <- new("AnnotationHub",
                       curPath=curPath,
                       paths=paths)

}
