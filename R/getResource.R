## This file contains methods for finishing the tab-completion and searching
## the web service for available resources.

.pathToLocalPath <- function(path) {
    if (.Platform$file.sep != "/")
        path <- gsub("/", .Platform$file.sep, path)
    path
}

.dirCreate <- function(dirPath) {
    if (!file.exists(dirPath))
        if (!dir.create(dirPath, showWarnings=FALSE, recursive=TRUE)) {
            warning(gettextf("unable to create %s", sQuote(dirPath)),
                    domain = NA)
        }
    else if (!file.info(dirPath)$isdir)
        stop("path exists but is not a directory: ", sQuote(dirPath))
    dirPath
}

## helper for downloading files
.downloadFile <- function(x, path) {
    message("Retrieving ", sQuote(path))
    url <- hubResource(x, path, cached=FALSE)
    destfile <- hubResource(x, path, cached=TRUE)
    .dirCreate(dirname(destfile)) 
    download.file(url=url, destfile=destfile, quiet=TRUE)
}

## Helper for getting rda files.
.getRda <- function(x, path) {
    message("Retrieving ", sQuote(path))
    obj <- local({
        .objName <- load(file=hubResource(x, path))
        get(.objName)
    })

    if (!.isCached(hubCache(x), path)) {
        localPath <- hubResource(x, path, cached=TRUE)
        .dirCreate(dirname(localPath))
        save(obj, file=localPath)
    }

    obj
}

## Gets fasta Files downloaded and then makes a FaFile handle for them
.getFasta <- function(x, path){
    require(Rsamtools)  ## only needed here

    ## download, if needed
    if (!.isCached(hubCache(x), path)) {
        indexPath <- paste(path, ".fai", sep="")
        .downloadFile(x, indexPath)    # index
        .downloadFile(x, path)         # file
    }

    ## get
    localPath <- hubResource(x, path, cached=TRUE)
    FaFile(localPath)
}

## $ is called on enter; this gets the data
.getResource <- function(x, name) {
    path <- snapshotPaths(x)[name]   
    if (is.na(path)) {
        warning("incomplete path")
        return(grep(name, names(x), value=TRUE))
    }

    ## Get the metadata
    m <- as.character(unlist(.metadata(x, filters=list(RDataPath=path),
                                keytypes="RDataClass")))

    ## Call correct function based on the results of the metadata
    FUN <- switch(m, fasta=.getFasta, .getRda)
    FUN(x, path)
}
