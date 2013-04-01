## filters = named list of character vectors that holds the keytypes
## (names) and also their acceptable values.  It has to be a list
## because sometimes there may be multiple acceptable values for a
## given keytype.

setOldClass("package_version")          # needed for AnnotationHub definition

.AnnotationHub <- setClass("AnnotationHub",
                           representation(hubUrl="character",
                                          hubCache="character",
                                          snapshotVersion="package_version",
                                          snapshotDate="POSIXlt",
                                          snapshotPaths="character",
                                          filters="list"),
                           prototype(hubCache = NA_character_,
                                     snapshotVersion=package_version("0.0"),
                                     snapshotDate=as.POSIXlt("2012-01-01")))

## FIXME: validity -- filters

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnotationHub" constructor.
###

AnnotationHub <-
    function(hubUrl=.hubUrl(), hubCache = .hubCache(),
             snapshotVersion, snapshotDate, ...)
{
    snapshotVersion <-
        if (missing(snapshotVersion)) .snapshotVersion()
        else as.package_version(snapshotVersion)
    snapshotDate <-
        if (missing(snapshotDate)) .snapshotDate(hubUrl, snapshotVersion)
        else as.POSIXlt(snapshotDate)
    snapshotUrl <- .snapshotUrl(hubUrl, snapshotVersion, snapshotDate)
    snapshotPaths <- .snapshotPaths(snapshotUrl)

    .AnnotationHub(hubUrl=hubUrl, hubCache=hubCache,
                   snapshotVersion=snapshotVersion,
                   snapshotDate=snapshotDate,
                   snapshotPaths=snapshotPaths, ...)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("hubUrl", "AnnotationHub", function(x, ...) {
    x@hubUrl
})

setMethod("hubCache", "AnnotationHub", function(x, ...) {
    x@hubCache
})

.replaceHubCache <- function(x, ..., create=FALSE, value) {
    hubCache <- if (create) {
        suppressWarnings(normalizePath(value))
    } else {
        withCallingHandlers({
            normalizePath(value)
        }, warning = function(w) {
            if (!isTRUE(file.info(value)$isdir)) {
                msg <- "'value' does not exist or is not a directory, see ?hubCache"
                warning(msg)
            } else warning(w)
            invokeRestart("muffleWarning")
        })
    }
    if (create && !file.exists(hubCache))
        dir.create(hubCache, recursive=TRUE)

    initialize(x, hubCache=hubCache)
}

setReplaceMethod("hubCache", c("AnnotationHub", "character"),
                 .replaceHubCache)

setMethod("snapshotVersion", "AnnotationHub", function(x, ...) {
    as.character(x@snapshotVersion)
})

setMethod("snapshotDate", "AnnotationHub", function(x, ...) {
    x@snapshotDate
})

setMethod("snapshotUrl", "AnnotationHub", function(x, ...) {
    .snapshotUrl(hubUrl(x), snapshotVersion(x), snapshotDate(x))
})

setMethod("snapshotPaths", "AnnotationHub", function(x, ...) {
    x@snapshotPaths
})

setMethod("snapshotUrls", "AnnotationHub", function(x, ...) {
    urls <- snapshotPaths(x)
    setNames(paste(hubUrl(x), urls, sep="/"), names(urls))
})

setMethod("possibleDates", "AnnotationHub", function(x, ...) {
    .possibleDates(hubUrl(x), snapshotVersion(x))
})

.replaceSnapshotDate <- function(x, ..., value) {
    snapshotDate <- as.POSIXlt(value)
    if (snapshotDate == snapshotDate(x))
        return (x)
    possibleDates <- possibleDates(x)
    if (!snapshotDate %in% possibleDates)
        stop("'value' is not in possibleDates(x)")

    snapshotUrl <- .snapshotUrl(.hubUrl(), snapshotVersion(x),
                                snapshotDate)
    snapshotPaths <- .snapshotPaths(snapshotUrl)
    if (0L == length(snapshotPaths))
        stop("no 'snapshotPaths' for 'snapshotDate' and 'filters'")

    initialize(x, snapshotDate=snapshotDate,
               snapshotPaths=snapshotPaths)
}

setReplaceMethod("snapshotDate", "AnnotationHub",
                 .replaceSnapshotDate)

.isCached <- function(hubCache, path=character()) {
    cacheFile <- .cacheResource(hubCache, path)
    file.exists(cacheFile)
}

setMethod("hubResource", "AnnotationHub",
    function(x, path=character(), cached=.isCached(hubCache(x), path), ...)
{
    if (cached)
        .cacheResource(hubCache(x), path, ...)
    else
        .hubResource(hubUrl(x), path, ...)
})

setMethod("filters", "AnnotationHub", function(x) {
    x@filters
})

setReplaceMethod("filters", "AnnotationHub",
                 function(x, ..., value) .replaceFilter(x, value))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### vector-like interface
###

setMethod("names", "AnnotationHub", function(x) {
    names(snapshotPaths(x))
})

setMethod("length", "AnnotationHub", function(x) {
    length(snapshotPaths(x))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "select"-like and discovery interface
###

setMethod("keytypes", "AnnotationHub", function(x) {
    .keytypes(snapshotUrl(x))
})

## For this cols does the same thing as keytypes
setMethod("cols", "AnnotationHub", function(x) {
    .keytypes(snapshotUrl(x))
})

setMethod("keys", "AnnotationHub", function(x, keytype) {
    if (!any(keytype %in% keytypes(x)))
        stop("invalid 'keytype', see keytypes(x)")
    .keys(snapshotUrl(x), keytype)
})

setMethod("metadata", "AnnotationHub", function(x, cols, ...) {
    if(missing(cols)){
        cols <- c("Title","Species","TaxonomyId","Genome",
                      "Description","Tags","RDataClass")
    } else if(!all(cols %in% cols(x))){
        ## check cols to avoid user error (can't live in .metadata b/c of usage)
        stop("All cols arguments must be values returned by the cols method.")
    }
    .metadata(snapshotUrl(x), filters(x), cols)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### tab completion and retrieval
###

.DollarNames.AnnotationHub <-
    function(x, pattern="")
{
    pattern <- sub(" ... \\[\\d+\\]$", "", pattern)
    values <- grep(pattern, names(x), value=TRUE)
    .completion(values)
}

setMethod("$", "AnnotationHub", function(x, name){.getResource(x,name)})

setMethod("[[", "AnnotationHub",
          function(x, i, j, ...){
              if (nargs() >= 3) stop("too many subscripts")
              .getResource(x, name=i)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###
setMethod(show, "AnnotationHub", function(object) {
    cat("class:", class(object), "\n")
    cat("length:", length(names(object)), "\n")
    nfilt <- length(filters(object))
    cat("filters:", if (nfilt) nfilt else "none", "\n")
    cat("hubUrl:", hubUrl(object), "\n")
    cat("snapshotVersion: ", as.character(snapshotVersion(object)), "; ",
        "snapshotDate: ", as.character(snapshotDate(object)), "\n",
        sep="")
    cat("hubCache:", hubCache(object), "\n")
})
