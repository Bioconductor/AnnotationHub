### =========================================================================
### AnnotationHub objects
### -------------------------------------------------------------------------
###

setClass("AnnotationHub", contains="Hub")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor 
###

## Add code to check : https://annotationhub.bioconductor.org/metadata/highest_id
## And if not, delete the DB so it will be re-downloaded...
AnnotationHub <-
    function(..., hub=getAnnotationHubOption("URL"),
             cache=getAnnotationHubOption("CACHE"),
             proxy=getAnnotationHubOption("PROXY"),
             localHub=FALSE) 
{
    .Hub("AnnotationHub", hub, cache, proxy, localHub, ...)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting 
###

.Hub_get1 <-
    function(x)
{
    if (!length(x))
        stop("no records found for the given index")
    if (length(x) != 1L)
        stop("'i' must be length 1")

    ## Add 'Resource' postfix to DispatchClass name
    className <- sprintf("%sResource", .dataclass(x))
    if (is.null(getClassDef(className))) {
        msg <- sprintf("'%s' not available in this version of the
            package; use biocLite() to update?",
            names(x))
        stop(paste(strwrap(msg, exdent=2), collapse="\n"), call.=FALSE)
    }

    tryCatch({
        class <- new(className, hub=x)
    }, error=function(err) {
        stop("failed to create 'HubResource' instance",
             "\n  name: ", names(x),
             "\n  title: ", x$title,
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })

    tryCatch({
        .get1(class)
    }, error=function(err) {
        stop("failed to load resource",
             "\n  name: ", names(x),
             "\n  title: ", x$title,
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### cache method
###

setMethod("cache", "AnnotationHub",
    function(x, ...) {
        callNextMethod(x,
                       cache.root=".AnnotationHub", 
                       cache.fun=setAnnotationHubOption, 
                       proxy=getAnnotationHubOption("PROXY"), 
                       max.downloads=getAnnotationHubOption("MAX_DOWNLOADS"))
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show method
###

setMethod("show", "AnnotationHub", function(object) 
{
    len <- length(object)
    cat(sprintf("%s with %d record%s\n", class(object), len,
                ifelse(len == 1L, "", "s")))
    cat("# snapshotDate():", snapshotDate(object), "\n")
    callNextMethod(object)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### listResources and loadResources methods
###

.filterResources_AH <- function(package, filterBy=character()) {
    if (!is.character(filterBy))
        stop("'filterBy' must be a character vector")
    suppressMessages({ah <- AnnotationHub()})
    if (!package %in% unique(package(ah)))
        stop(paste0("no resources for package '", package, 
                    "' were found in AnnotationHub"))

    sub <- query(ah, package)
    if (length(filterBy))
        query(sub, filterBy)
    else
        sub
}

setMethod("listResources", "AnnotationHub", 
    function(hub, package, filterBy=character()) {
        metadata <- .filterResources_AH(package, filterBy)
        mcols(metadata)$title
})

setMethod("loadResources", "AnnotationHub",
    function(hub, package, filterBy=character()) {
        metadata <- .filterResources_AH(package, filterBy)
        ah <- AnnotationHub()
        lapply(names(metadata), function(i) ah[[i]]) 
})
