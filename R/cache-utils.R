

.cache_path <- function(hub, path, cache.root=NULL, cache.fun=NULL)
{
    cache <- hubCache(hub)
    ## FIXME: when would 'cache' be missing?
    if (missing(cache)) {
        if (any(is.null(cache.root) | is.null(cache.fun)))
            stop("'cache.root' and 'cache.fun' must be set if cache is missing")

        ## this section replaces .cache_path_default
        if (file.exists(cache) && file.info(cache)$isdir)
            return(cache)

        if (file.exists(cache) && !file.info(cache)$isdir) {
            txt <- sprintf("default cache exists but is not a directory\n  %s",
                           sQuote(cache))
            warning(txt)
            cache <- NULL
        }

        if (!is.null(cache) && interactive()) {
            txt <- sprintf("create cache at %s?", sQuote(cache))
            ans <- .ask(txt, values=c("y", "n"))
            if (ans == "n") {
                cache <- file.path(tempdir(), cache.root)
                message(sprintf("using temporary cache %s", sQuote(cache)))
            }
        } else {
            cache <- file.path(tempfile())
        }
        ## FIXME:
        cache.fun("CACHE", cache)
    } else if (missing(path)) {
        cache
    } else {
        file.path(cache, path)
    }
}

.create_cache <-
    function(cache, .class)
{
    if (file.exists(cache)) {
        if (!file.info(cache)$isdir)
            stop("cache ", sQuote(cache), " exists but is not a directory")
    } else
        dir.create(cache, recursive=TRUE)

    sqlitefile <- paste0(tolower(.class), ".sqlite3")
    file.path(cache, sqlitefile)
}

.cache_internal <- function(x, cache.root, cache.fun, proxy, max.downloads)
{
    cachepath <- .named_cache_path(x, cache.root, cache.fun) 
    need <- !file.exists(cachepath)
    if (any(need)) {
        hubpath <- .hub_resource_path(.hub_data_path(hubUrl(x)),
            basename(cachepath)[need])
        message("downloading from ", paste0(sQuote(hubpath), collapse="\n    "))
    } else {
        message("loading from cache ", 
                paste0(sQuote(cachepath), collapse="\n    "))
    }
    if (any(need) > max.downloads) {
        if (!interactive()) {
            txt <- sprintf("resources needed (%d) exceeds max.downloads (%d)",
                           sum(need), max.downloads)
            stop(txt)
        }
        ans <- .ask(sprintf("download %d resources?", sum(need)), c("y", "n"))
        if (ans == "n")
            return(cachepath[!need])
    }
    ok <- .hub_resource(.hub_data_path(hubUrl(x)), basename(cachepath)[need], 
                         cachepath[need], proxy=proxy)
    if (!all(ok))
        stop(sprintf("%d resources failed to download", sum(!ok)))
    cachepath
}

.named_cache_path <- function(x, cache.root, cache.fun) 
{
    stopifnot(is(x, "Hub"))
    path <- .datapathIds(x)
    setNames(.cache_path(x, path, cache.root, cache.fun), names(path))
}

removeCache <- function(x)
{
    reply <- .ask("Delete cache file", c("y", "n"))
    cache <- hubCache(x)
    if (reply == "y") {
      rv <- tryCatch({
          if (file.exists(cache)) {
              status <- unlink(cache, recursive=TRUE, force=TRUE)
              if (status == 1)
                  stop("'unlink()' failed to remove directory")
          }

          TRUE
      }, error=function(err) {
          warning("'clearCache()' failed",
              "\n  database: ", sQuote(cache),
              "\n  reason: ", conditionMessage(err),
              call.=FALSE)

          FALSE
      })
    } else rv <- FALSE

    rv
}
