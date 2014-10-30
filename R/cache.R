hubCache <-
    function(x)
{
    if (missing(x)) {
        hubOption("CACHE")
    } else .cache(x)
}

.cache_path_default <-
    function()
{
    cache <- hubOption("CACHE")

    if (file.exists(cache) && file.info(cache)$isdir)
        return(cache)

    if (file.exists(cache) && !file.info(cache)$isdir) {
        txt <- sprintf("default cache exists but is not a directory\n  %s",
                       sQuote(cache))
        warning(txt)
        cache <- NULL
    }

    if (!is.null(cache) && interactive()) {
        txt <- sprintf("create AnnotationHub cache at %s?", sQuote(cache))
        ans <- .ask(txt, values=c("y", "n"))
        if (ans == "n") {
            cache <- file.path(tempdir(), .CACHE_ROOT)
            message(sprintf("using temporary cache %s", sQuote(cache)))
        }
    } else {
        cache <- file.path(tempfile())
    }
    setHubOption("CACHE", cache)
}

.cache_path <-
    function(cache, path)
{
    if (missing(cache))
        .cache_path_default()
    else if (missing(path))
        cache
    else
        file.path(cache, path)
}

.cache_create <-
    function(cache)
{
    if (file.exists(cache)) {
        if (!file.info(cache)$isdir)
            stop("cache ", sQuote(cache), " exists but is not a directory")
    } else
        dir.create(cache, recursive=TRUE)
    
    file.path(cache, .DB_SQLITE)
}

## cache returns either the path to the URL or the local path (in a cache)
## along the way it downloads the resource that it locates 
## Already expecting multiple ids from .dataPathIds(), (potentially)
## This is the path based on the RdataPath? (currenntly its based on resouce_id)
cache <-
    function(x, ..., max.downloads=hubOption("MAX_DOWNLOADS"))
{
    stopifnot(is(x, "AnnotationHub"))
    path <- .datapathIds(x)
    cachepath <- setNames(.cache_path(.cache(x), path), .db_uid(x))
    need <- !file.exists(cachepath)
    if (sum(need) > max.downloads) {
        if (!interactive()) {
            txt <- sprintf("resources needed (%d) exceeds max.downloads (%d)",
                           sum(need), max.downloads)
            stop(txt)
        }
        ans <- .ask(sprintf("download %d resources?", sum(need)), c("y", "n"))
        if (ans == "n")
            return(cachepath[!need])
    }
    ok <- .hub_resource(.hub_data_path(.hub(x)), path[need], cachepath[need])
    if (!all(ok))
        stop(sprintf("%d resources failed to download", sum(!ok)))
    cachepath
}

`cache<-` <- function(x, ..., value)
{
    stopifnot(is(x, "AnnotationHub"))
    stopifnot(identical(value, NULL))
    cachepath <- .cache_path(.cache(x), .datapath(x))
    result <- unlink(cachepath)
    status <- file.exists(cachepath)
    if (any(status))
        warning("failed to unlink cache files:\n  ",
                paste(sQuote(.cache_path(x)[status]), collapse="\n  "))
    x
}
