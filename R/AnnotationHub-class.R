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

    if ((cache == R_user_dir("AnnotationHub", which="cache")) && (Sys.getenv("ANNOTATION_HUB_CACHE")=="")){
        olddefault = rappdirs::user_cache_dir(appname="AnnotationHub")
        if (dir.exists(olddefault) && (length(list.files(olddefault)) != 0)){
            stop("As of AnnotationHub (>2.23.2), default caching location has changed.\n",
                 "  Problematic cache: ", path.expand(olddefault), "\n",
                 "  To continue with default caching location, \n",
                 "  See AnnotationHub vignette TroubleshootingTheCache section on 'Default Caching Location Update'\n")
        }
    }

    if (is.null(proxy)){
        connect <- suppressWarnings(tryCatch({
            readBin(hub, n=1L, what="raw")
            TRUE
        }, error = function(...){
            FALSE
        }))
    } else {
        connect <- TRUE
        message("Assuming valid proxy connection through '",
                ifelse(is(proxy,"request"),
                       paste(unlist(proxy), collapse=":"),
                       proxy),
                "'",
                "\n If you experience connection issues consider ",
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
