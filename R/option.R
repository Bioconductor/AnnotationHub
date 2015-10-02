.hub_options <- new.env(parent=emptyenv())

.hub_option_key <- function(key0=c("URL", "CACHE", "PROXY", "MAX_DOWNLOADS"))
    match.arg(key0)

hubOption <- function(arg) {
    arg <- .hub_option_key(toupper(arg))
    .hub_options[[arg]]
}

setHubOption <- function(arg, value)
{
    ## FIXME: validate 'value'
    key <- .hub_option_key(toupper(arg))
    if (length(value) != 1L)
        stop("'value' must be length 1")
    .hub_options[[key]] <-
        switch(key, URL=, CACHE=, PROXY={
            as.character(value)
        }, MAX_DOWNLOADS={
            as.integer(value)
        })
}

setHubOption <- function(arg, value)
{
    key <- .hub_option_key(toupper(trimws(arg)))
    if (!is.null(value) && !inherits(value, "request") && length(value) != 1L)
        stop("'value' must be length 1")
    .hub_options[[key]] <-
        switch(key, URL=, CACHE={
            as.character(value)
        }, MAX_DOWNLOADS={
            as.integer(value)
        }, PROXY={
            if (is.null(value))
                NULL
            else if (is.character(value)) {
                rm <- .parseUrl(value)
                if (length(rm) == 0)
                    stop("Malformed URL")
                use_proxyArgs <- as.list(formals(httr::use_proxy))
                rm$url <-
                    paste0(rm$scheme, ifelse(rm$scheme != "", "://", ""),
                        rm$hostname)
                if (rm$port != "")
                    rm$port = as.numeric(sub(":", "", rm$port))
                sapply(c("url", "port", "username", "password"),
                    function(s)
                        if (rm[[s]] != "") use_proxyArgs[[s]] <<- rm[[s]])

                do.call(httr::use_proxy, use_proxyArgs)
            } else if (inherits(value, "request")) {
                value
            }
        })
}

.regMatches <- function(pattern, x, g=FALSE, simplify=TRUE, ...)
{
    regFun <- base::regexpr
    if (g) regFun <- base::gregexpr

    matches <- regFun(pattern, x, ...)
    matchLength <- attr(matches, "match.length")
    rv <- vector("list", length(matchLength))
    captureNames <- attr(matches, "capture.names")
    captureStart <- attr(matches, "capture.start")
    captureLength <- attr(matches, "capture.length")

    for (i in seq_along(matchLength)) {
        submatches <- list()
        if (matchLength[i] != -1) {
            submatches <-
                mapply(FUN=function(x, y, z) substr(x, y, y + z - 1),
                    x[i], captureStart[i, ], captureLength[i, ],
                    SIMPLIFY=FALSE, USE.NAMES=FALSE)
            names(submatches) <- captureNames
        }

        if (i %in% grep(pattern, x, ...))
            submatches <- c(x[i], submatches)

        rv[[i]] <- submatches
    }

    if (simplify) {
        if (is.list(rv) && length(rv) <= 1)
            rv <- unlist(rv, recursive=FALSE)
    }

    ## return
    rv
}

.parseUrl <- function(x, ...)
{
    re <- gsub("\\s+", "", strwrap(
        "^(?:(?:
        (?P<scheme>[a-z][a-z0-9+\\-.]*):\\/)?\\/+)*(?:
        (?P<username>.*?)(?::
        (?P<password>.*?)|)@)?\
        (?P<hostname>[^:\\/\\s]+)
        (?P<port>:(?:[^\\/]*))?(?:
        (?P<path>(?:\\/\\w+)*\\/)
        (?P<filename>[-\\w.]+[^#?\\s]*)?
        (?P<query>\\?(?:[^#]*))?
        (?P<fragment>#(?:.*))?)*$",
        width=Inf))

    ## return 
    .regMatches(re, trimws(x), perl=T, ...)
}
