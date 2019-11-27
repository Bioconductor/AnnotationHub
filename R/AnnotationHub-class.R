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
             localHub=getAnnotationHubOption("LOCAL"),
             ask=getAnnotationHubOption("ASK"))
{
    if (is.null(proxy)){
        connect <- curl::has_internet()
    } else {
        connect <- TRUE
        message("Cannot determine internet connection.",
                "\n If you experience connection issues consider ",
                "using 'localHub=TRUE'")
    }
    if (!connect && !localHub){
        message("No internet connection using 'localHub=TRUE'")
        localHub <- !connect
    }
    .Hub("AnnotationHub", hub, cache, proxy, localHub, ask, ...)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### cache method
###

setMethod("cache", "AnnotationHub",
    function(x, ..., force=FALSE, verbose=FALSE) {
        callNextMethod(x,
                       cache.root="AnnotationHub", 
                       cache.fun=setAnnotationHubOption, 
                       proxy=getAnnotationHubOption("PROXY"), 
                       max.downloads=getAnnotationHubOption("MAX_DOWNLOADS"),
                       force=force,
                       verbose=verbose)
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
