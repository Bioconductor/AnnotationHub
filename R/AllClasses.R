## Some internal value are used for knowing what this object "points" to...
## curPath = holds the path that the user has chosen "so far".

## paths = holds character vector of possible paths to choose from.

## pattern = what tab completion has guess so far based on the paths and the
## user interaction.

## filters = named list of character vectors that holds the keytypes (names)
## and also their acceptable values.  It has to be a list because sometimes
## there may be multiple acceptable values for a given keytype.

setClass("AnnotationHub", representation(curPath = "character",
                                         paths="character",
                                         pattern="character",
                                         filters="list"))






## .retrievePathVals is for listing the items from the web server.
## This method is only called by the constructor.
.retrievePathVals <- function(curPath){
  curPath <- paste(curPath,"AnnotationHub",sep="/")
  ## look for values
  paths <- fromJSON(curPath)
  ## if there are some new values we set curPathExtendedYet back to FALSE
  if(length(paths) > 0 && exists("x")){
    x@curPathExtendedYet <- FALSE
  }
  ## return the options...
  paths
}


## constructor
AnnotationHub <- function(
              curPath= "http://wilson2.fhcrc.org/cgi-bin/R"){
  paths <- .retrievePathVals(curPath)
  paths <- setNames(paths, make.names(paths))
  AnnotationHub <- new("AnnotationHub",
                       curPath=curPath,
                       paths=paths)

}
