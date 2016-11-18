## and hubUrl (https://annotationhub.bioconductor.org/)

.hub_metadata_path <-
    function(hub)
{
    paste(hub, "metadata", sep="/")
}

## The following helper was previously not used (and now is), but we
## do need a way to have a path for the data that is separate from the
## path for the .sqlite metadata (they don't need to always be at the
## same server) - IOW these should be decoupled anyhow).
.hub_data_path <-
    function(hub)
{
##    "http://gamay:9393/fetch"
##    "https://annotationhub.bioconductor.org/fetch"
    sprintf("%s/fetch", hub)
}

.hub_resource_path <-
    function(hub, resource)
{
    sprintf("%s/%s", hub, resource)
}

## example of hub resource (sometimes convenient)
## hub = 'https://annotationhub.bioconductor.org/metadata/annotationhub.sqlite3'
.hub_cache_resource <- function(hubpath, cachepath, proxy) {
    ## retrieve file from hub to cache
    tryCatch({
        tmp <- tempfile()
        ## Download the resource in a way that supports https
        if (interactive() && (packageVersion("httr") > "1.0.0")) {
            response <-
                GET(hubpath, progress(), write_disk(tmp), proxy)
            cat("\n") ## line break after progress bar
        } else {
            response <- GET(hubpath, write_disk(tmp), proxy)
        }
        if (length(status_code(response)))  
        {
            # FTP requests return empty status code, see
            # https://github.com/hadley/httr/issues/190
            if (status_code(response) != 302L)
                stop_for_status(response)
        }
        if (!all(file.exists(dirname(cachepath))))
            dir.create(dirname(cachepath), recursive=TRUE)
        file.copy(from=tmp, to=cachepath)
        file.remove(tmp)
        TRUE
    }, error=function(err) {
        warning("download failed",
                "\n  hub path: ", sQuote(hubpath),
                "\n  cache path: ", sQuote(cachepath),
                "\n  reason: ", conditionMessage(err),
                call.=FALSE)
        FALSE
    })
}

## This is the function that gets stuff (metadata AND files) from S3
.hub_resource <-
    function(hub, resource, cachepath, proxy, overwrite=FALSE)
{
    len <- length(resource)
    if (len > 0L) {
        msg <- sprintf("retrieving %d resource%s", len,
                       if (len > 1L) "s" else "")
        message(msg)
    }

    if (length(cachepath)) {
        test <- file.exists(cachepath)
        if (!overwrite && any(test)) {
            bad <- cachepath[test]
            stop("download destination(s) exists",
                 "\n  ", paste(sQuote(bad), collapse="\n  "))
        }
    }

    hubpath <- .hub_resource_path(hub, resource)
    mapply(.hub_cache_resource, hubpath, cachepath, MoreArgs=list(proxy))
}

### --------------------------------------------------------------------------
### snapshotDate helpers

## returns the release date for biocVersion()
.biocVersionDate <- function(biocversion) {
    if (length(biocversion) > 1L)
        stop("length(biocversion) must == 1")

    yaml <- content(GET("http://bioconductor.org/config.yaml"), 
                    encoding="UTF8", as="text")
    obj <- yaml.load(yaml)
    release_dates <- obj$release_dates
    version_date <- release_dates[biocversion == names(release_dates)]
    ## convert to snapshot format 
    if (length(version_date))
        as.character(as.POSIXlt(version_date[[1]], format='%m/%d/%Y'))
    else
        character()
}

## single date closest to the release date for biocVersion()
.restrictDateByVersion <- function(path) {
    dates <- as.POSIXlt(.possibleDates(path), format='%Y-%m-%d')
    restrict <- as.POSIXlt(.biocVersionDate(biocVersion()), format='%Y-%m-%d')
    if (length(restrict))  ## release
        as.character(max(dates[dates < restrict]))
    else                   ## devel 
        as.character(max(dates))
}

## all possible dates 
.possibleDates <- function(path) {
    conn <- .db_open(path)
    on.exit(.db_close(conn))
    query <- 'SELECT DISTINCT rdatadateadded FROM resources'
    dateAdded <- .db_query(conn, query)[[1]]
    query <- 'SELECT DATE(timestamp) FROM timestamp'
    dateModified <- .db_query(conn, query)[[1]]
    c(dateAdded, dateModified)
}

## dates restricted by snapshotDate (and hence biocVersion())
possibleDates <- function(x) {
    path <- dbfile(x)
    dates <- .possibleDates(path)
    restrict <- .restrictDateByVersion(path)
    dates[as.POSIXlt(dates) <= as.POSIXlt(restrict)]
}
