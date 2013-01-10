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
                                          versionString="character",
                                          dateString="character",
                                          cachingEnabled="logical",
                                          pattern="character",
                                          filters="list"))

## This is just how I declare the "base" curPath:
.baseCurPath <- function(){
    paste0(.getServer() ,"/ah")
}

## .retrievePathVals is for listing the items from the web server.
## This method is only called by the constructor.
.retrievePathVals <- function(curPath){
    url <- paste(curPath, "getAllResourcePaths", sep="/")
    ## look for values
    fromJSON(url)
}

## Server Name.  (package-wide global means one change when it moves)
.getServer <- function(){
    getOption("AnnotationHub.Server.Url",
              "http://annotationhub.bioconductor.org")
}

.getCurVersion <- function(){
    as.character(BiocInstaller:::BIOC_VERSION)
}

## Takes the base path information and gets the version data for the constructor
.getDateString <- function(curPath, versionString){
    latestDateUrl <- paste(curPath, versionString,
                           "getLatestSnapshotDate", sep="/")
    fromJSON(latestDateUrl)
}


## constructor
AnnotationHub <- function(curPath=.baseCurPath(), ...){
    ## get the latest version info.
    versionString <- .getCurVersion()
    dateString <- .getDateString(curPath, versionString)
    ## set up curPath based on the latest snapshot
    curPath <- paste(curPath, versionString, dateString, sep="/")
    
    ## Then get the paths etc.
    paths <- .retrievePathVals(curPath)
    paths <- setNames(paths, make.names(paths))

    ## Check for caching etc.
    cachingEnabled <- .checkCaching()

    
    .AnnotationHub(curPath=curPath, paths=paths, versionString=versionString,
                   dateString=dateString, cachingEnabled=cachingEnabled, ...)
}


