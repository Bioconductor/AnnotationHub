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
    if (getOption("AnnotationHub.debug", FALSE))
        print(paste("Downloading", url))
    download.file(url=url, destfile=destfile, quiet=TRUE)
}

## Helper for getting rda files.
.getRda <- function(x, path) {
    obj <- local({
        rsrc <- hubResource(x, path)
        if (grepl("^http:", rsrc, TRUE)) {
            message("Retrieving ", sQuote(path))
            rsrc <- url(rsrc)
        }
        if (getOption("AnnotationHub.debug", FALSE))
            print(paste("Downloading", hubResource(x, path)))

        .objName <- load(rsrc)
        get(.objName)
    })

    if (!.isCached(hubCache(x), path)) {
        localPath <- hubResource(x, path, cached=TRUE)
        .dirCreate(dirname(localPath))
        save(obj, file=localPath)
    }

    if (isS4(obj)) {
        pkg <- getClassDef(class(obj))@package
        withCallingHandlers({
            require(pkg, quietly=TRUE, character.only=TRUE)
        }, warning=function(w) {
            msg <- sprintf("%s is from package %s,
                             but \"require('%s')\" said: %s",
                           sQuote(path), sQuote(pkg), pkg,
                           conditionMessage(w))
            warning(paste(strwrap(msg), collapse="\n"), call.=FALSE)
            invokeRestart("muffleWarning")
        })
    }

    if (is(obj,"GRanges") || is(obj,"GRangesList")){
        len <- length(metadata(obj))
        meta <- ahinfo(x, names(path))
        ## its expected that usually this will be put into spot 1, but
        ## just in case...
        metadata(obj)[[len+1]] <- as.list(meta[[1]]) ## always just 1st one 
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
    Rsamtools::FaFile(localPath)
}

## Gets sqlite files downloaded and then uses loadDb to handle them
.getSQLite <- function(x, path){
    require(AnnotationDbi)  ## only needed here

    ## download, if needed
    if (!.isCached(hubCache(x), path)) {
        .downloadFile(x, path)         # file
    }

    ## get
    localPath <- hubResource(x, path, cached=TRUE)
    AnnotationDbi::loadDb(localPath)
}

## Downloads tabix files and makes a TabixFile handle
.getTabix <- function(x, path){
    require(Rsamtools)

    if (!.isCached(hubCache(x), path)) {
        indexPath <- paste(path, ".tbi", sep="")
        .downloadFile(x, indexPath)    # index
        .downloadFile(x, path)         # file
    }

    localPath <- hubResource(x, path, cached=TRUE)
    Rsamtools::TabixFile(localPath)
}

## $ is called on enter; this gets the data
.getResource <- function(x, name) {
    path <- snapshotPaths(x)[name]   
    if (is.na(path)) {
        warning("incomplete path")
        return(grep(name, names(x), value=TRUE))
    }

    ## Get the metadata
    m <- as.character(unlist(.metadata(snapshotUrl(x),
                                       filters=list(RDataPath=path),
                                       columns="RDataClass")))

    ## Call correct function based on the results of the metadata
    FUN <- switch(m,
                  FaFile=.getFasta,
                  TabixFile=.getTabix,
                  SQLiteFile=.getSQLite,
                  .getRda)
    FUN(x, path)
}
