## and hubUrl (https://annotationhub.bioconductor.org/)
hubUrl <- function(x) {
    if (missing(x)) {
        hubOption("URL")
    } else .hub(x)
}

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

.getResource <- function(hubpath, cachepath) {
    tryCatch({
        tmp <- tempfile()
        ## Download the resource in a way that supports https
        .curl_writer_download(hubpath, tmp)
        if (!file.exists(dirname(cachepath)))
            dir.create(dirname(cachepath), recursive=TRUE)
        file.rename(tmp, cachepath)
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

.hub_resource <-
    function(hub, resource, cachepath, overwrite=FALSE)
{
    if (length(resource)) {
        msg <- sprintf("retrieving %d resources", length(resource))
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
    ## Then extract the resource(s)
    mapply(.getResource, hubpath, cachepath)
}


## Add possibleDates here (SELECT DISTINCT rdatadateadded FROM)
possibleDates <- function(x){
##    query <- 'SELECT DISTINCT rdatadateadded FROM resources'
##    dbGetQuery(.db_connection(x), query)[[1]]
    .possibleDates(conn = .db_connection(x))
}
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
