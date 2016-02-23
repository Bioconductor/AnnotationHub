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
    mapply(.hub_cache_resource, hubpath, MoreArgs=list(cachepath, proxy))
}


.possibleDates <- function(conn) {
    query <- 'SELECT DISTINCT rdatadateadded FROM resources'
    dateAdded <- dbGetQuery(conn, query)[[1]]
    query <- 'SELECT DATE(timestamp) FROM timestamp'
    dateModified <- dbGetQuery(conn, query)[[1]]
    c(dateAdded, dateModified)
}

## Add possibleDates here (SELECT DISTINCT rdatadateadded FROM)
possibleDates <- function(x) 
    .possibleDates(dbconn(x))

## Date filter "1": will look kind of like this 
## it will be a subquery that will be passed to the next step...
## SELECT * FROM resources where rdatadateadded < '2013-03-21';

## Date filter "1": will look kind of like this
## To get most recent thing of each kind for the dates.
## SELECT rdatadateadded, count(title) FROM resources GROUP BY rdatadateadded;

## Final thing (or it's close anyhow)
## SELECT title,max(rdatadateadded), ah_id FROM 
## (SELECT * FROM resources where rdatadateadded <= '2013-04-04') 
## GROUP BY title 
## limit 4;

## The above will get us there, BUT: it requires some kind of assurance that 
## our titles will be used as proper IDs...  And this is NOT a call for a 
## unique constraint because that would mean that we could no longer group.  
## Basically, it treats title as a key...





## snapshotVersion i.e. BiocInstaller::biocVersion()


## TODO: I need to modify the uid setter (and make sure that this is used for 
## setting that slot package-wide) so that the filtering above is applied 
## whenever this slot is populated.
## And actually that just means that I need to actually just modify .uid0() so 
## that it also does date filtering.
