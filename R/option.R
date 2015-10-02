.hub_options <- new.env(parent=emptyenv())

.hub_option_key <- function(key0=c("URL", "CACHE", "PROXY", "MAX_DOWNLOADS"))
    match.arg(key0)

hubOption <- function(arg) {
    arg <- .hub_option_key(toupper(arg))
    .hub_options[[arg]]
}

setHubOption <- function(arg, value)
{
    key <- .hub_option_key(toupper(trimws(arg)))

    .hub_options[[key]] <- switch(key, URL=, CACHE={
        value <- as.character(value)
        stopifnot(isSingleString(value))
        value
    }, MAX_DOWNLOADS={
        value <- as.integer(value)
        stopifnot(isSingleInteger(value))
        value
    }, PROXY={
        if (is.null(value) || inherits(value, "request"))
            value
        else if (isSingleString(value)) {
            .httr_proxy(value)
        } else {
            txt <- "'value' must be an httr proxy request (use_proxy()),
                    character(1), or NULL"
            stop(paste(strwrap(txt, exdent=2), collapse="\n"))
        }
    })
}

.httr_proxy <- function(value)
{
    rm <- parse_url(value)
    if (is.null(rm$scheme))
        stop("PROXY 'value' does not include scheme (e.g., 'http://')")
    rm$url <- paste0(rm$scheme, "://", rm$hostname)
    if (!is.null(rm$port))
        rm$port <- as.integer(rm$port)
    do.call(use_proxy, rm[c("url", "port", "username", "password")])
}
