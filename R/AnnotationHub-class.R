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
        connect <- !is.null(curl::nslookup("annotationhub.bioconductor.org", error=FALSE))
    } else {
        connect <- TRUE
        message("Assuming valid proxy connection through '", proxy, "'",
                "\n  If you experience connection issues consider ",
                "using 'localHub=TRUE'")
    }
    if (!connect && !localHub){
        message("Cannot connect to AnnotationHub server, using 'localHub=TRUE' instead")
        localHub <- !connect
    }
    .Hub("AnnotationHub", hub, cache, proxy, localHub, ask, ...)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### cache method
###

setMethod("cache", "AnnotationHub",
    function(x, ..., force=FALSE, verbose=FALSE)
{
    callNextMethod(
        x,
        cache.root="AnnotationHub",
        cache.fun=setAnnotationHubOption,
        proxy=getAnnotationHubOption("PROXY"),
        max.downloads=getAnnotationHubOption("MAX_DOWNLOADS"),
        force=force,
        verbose=verbose
    )
})
