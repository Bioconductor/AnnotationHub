.hub_options <- new.env(parent=emptyenv())

.hub_option_key <- function(key0=c("URL", "CACHE", "MAX_DOWNLOADS"))
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
        switch(key, URL=, CACHE={
            as.character(value)
        }, MAX_DOWNLOADS={
            as.integer(value)
        })
}
