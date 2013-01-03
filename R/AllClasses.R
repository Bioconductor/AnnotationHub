## Some internal value are used for knowing what this object "points" to...
## curPath = holds the path that the user has chosen "so far".

## paths = holds character vector of possible paths to choose from.

## pattern = what tab completion has guess so far based on the paths and the
## user interaction.

## filters = named list of character vectors that holds the keytypes (names)
## and also their acceptable values.  It has to be a list because sometimes
## there may be multiple acceptable values for a given keytype.

.AnnotationHub <- setClass("AnnotationHub",
                           representation(curPath = "character",
                                          paths="character",
                                          pattern="character",
                                          filters="list"))

## .retrievePathVals is for listing the items from the web server.
## This method is only called by the constructor.
.retrievePathVals <-
    function(curPath)
{
    curPath <- paste(curPath, "getAllResourcePaths", sep="/")
    ## look for values
    fromJSON(curPath)
}

## Server Name.  (package-wide global means one change when it moves)
.getServer <- function()
{
    getOption("AnnotationHub.Server.Url",
              "http://annotationhub.bioconductor.org")
}

## constructor
AnnotationHub <-
    function(curPath= paste0(.getServer() ,"/ah"), ...)
{
    paths <- .retrievePathVals(curPath)
    paths <- setNames(paths, make.names(paths))
    .AnnotationHub(curPath=curPath, paths=paths, ...)
}
