.updateHubDB <-
    function(bfc, .class, url, proxy, localHub)
{
    res <- bfcquery(bfc, paste0(tolower(.class), ".sqlite3"),
                    field="rname", exact=TRUE)
    cnt <- bfccount(res)
    if (is.null(proxy)) proxy=""

    # Using localHub (no internet)
    if (localHub){
        if (cnt == 0){
            stop("Invalid Cache: sqlite file",
                 "\n  Hub has not been added to cache",
                 "\n  Run again with 'localHub=FALSE'")
        }else if (cnt > 1){
            stop("Corrupt Cache: sqlite file",
                 "\n  See vignette section on corrupt cache",
                 "\n  cache: ", bfccache(bfc),
                 "\n  filename: ",
                 paste0(tolower(.class), ".sqlite3"),
                 call.=FALSE)
        } else {
            rid <- res %>% collect(Inf) %>% `[[`("rid")
            db_path <- bfcpath(bfc, rids=rid)
        }
    # Checking remote file for updates
    } else{
        # hub database yet to be added to cache
        if (cnt == 0){
            remote_db <- paste0(url, "/metadata/", tolower(.class), ".sqlite3")
            db_path <- bfcadd(bfc,
                              rname=paste0(tolower(.class), ".sqlite3"),
                              fpath=remote_db, proxy=proxy)
        } else if (cnt > 1){
            stop("Corrupt Cache: sqlite file",
                 "\n  See vignette section on corrupt cache",
                 "\n  cache: ", bfccache(bfc),
                 "\n  filename: ",
                 paste0(tolower(.class), ".sqlite3"),
                  call.=FALSE)
        # found! check if needs update
        } else {
            rid <- res %>% collect(Inf) %>% `[[`("rid")
            check_update <- bfcneedsupdate(bfc, rids=rid)
            if(is.na(check_update)) check_update = TRUE
            db_path <- ifelse(check_update,
                              bfcdownload(bfc, rid=rid, ask=FALSE,
                                          proxy=proxy),
                              bfcpath(bfc, rids=rid))
        }
    }
    unname(db_path)
}


.db_is_valid <- function(path) {
    conn <- .db_open(path)
    on.exit(.db_close(conn))
    ## Some very minor testing to make sure metadata DB is intact.
    tryCatch({
        ## required tables present?
        expected <- c("biocversions", "input_sources", "location_prefixes",
                      "rdatapaths", "recipes", "resources", "statuses",
                      "tags", "timestamp")
        if (!all(expected %in% dbListTables(conn)))
            stop("missing tables")
        ## any resources?
        sql <- "SELECT COUNT(id) FROM resources"
        if (.db_query(conn, sql)[[1]] == 0L)
            warning("empty 'resources' table; database may be corrupt")
    }, error=function(err) {
        stop("Corrupt Hub Database",
             "\n  See vignette section on corrupt database",
             "\n  database: ", sQuote(path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
    TRUE
}





### --------------------------------------------------------------------------
### snapshotDate helpers
###

## returns the release date for BiocManager::version()
.biocVersionDate <- function(biocversion) {
    if (length(biocversion) > 1L)
        stop("length(biocversion) must == 1")

    yaml <- httr::content(GET("http://bioconductor.org/config.yaml"),
                    encoding="UTF-8", as="text")
    obj <- yaml.load(yaml)
    release_dates <- obj$release_dates
    version_date <- release_dates[biocversion == names(release_dates)]
    ## convert to snapshot format
    if (length(version_date))
        as.character(as.POSIXlt(version_date[[1]], format='%m/%d/%Y'))
    else
        character()
}

## single date closest to the release date for BiocManager::version()
.restrictDateByVersion <- function(path) {
    dates <- as.POSIXlt(.possibleDates(path), format='%Y-%m-%d')
    restrict <- as.POSIXlt(.biocVersionDate(BiocManager::version()),
                           format='%Y-%m-%d')
    if (length(restrict))  ## release
        as.character(max(dates[dates <= restrict]))
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

## dates restricted by snapshotDate (and hence BiocManager::version())
possibleDates <- function(x) {
    path <- dbfile(x)
    dates <- .possibleDates(path)
    restrict <- .restrictDateByVersion(path)
    dates[as.POSIXlt(dates) <= as.POSIXlt(restrict)]
}

## The following helper was previously not used (and now is), but we
## do need a way to have a path for the data that is separate from the
## path for the .sqlite metadata (they don't need to always be at the
## same server) - IOW these should be decoupled anyhow).
.hub_data_path <-
    function(huburl)
{
##    "http://gamay:9393/fetch"
##    "https://annotationhub.bioconductor.org/fetch"
    sprintf("%s/fetch", huburl)
}

# make full fetch for each resource needed
.hub_resource_path <-
    function(hub, resource)
{
##    "https://annotationhub.bioconductor.org/fetch/5012"
    sprintf("%s/%s", hub, resource)
}

## This is the function that gets stuff (metadata AND files) from S3
.hub_resource <-
    function(x, resource, cachepath, proxy, verbose=FALSE)
{
    len <- length(resource)
    if (len > 0L) {
        msg <- sprintf("retrieving %d resource%s", len,
                       if (len > 1L) "s" else "")
        if (verbose) message(msg)
    }
    hub <-  .hub_data_path(hubUrl(x))
    bfc <- .get_cache(x)
    hubpath <- .hub_resource_path(hub, resource)
    mapply(.hub_cache_resource, hubpath, names(cachepath), cachepath, MoreArgs=list(proxy=proxy,
                                                        bfc=bfc))
}


## example of hub resource (sometimes convenient)
## hub = 'https://annotationhub.bioconductor.org/metadata/annotationhub.sqlite3'
.hub_cache_resource <- function(hubpath, namescachepath, cachepath, bfc, proxy)
{

    if (is.null(proxy)) proxy=""

    tryCatch({
        rnames <- paste(namescachepath, cachepath, sep=" : ")
        res <- bfcquery(bfc, rnames, fields="rname", exact=TRUE)
        cnt <- bfccount(res)
        rid <- res %>% collect(Inf) %>% `[[`("rid")
        if (cnt > 1){
            stop("Corrupt Cache: resource id",
                 "\n  More than one entry in cache for: ",
                 rnames,
                 "\n  See vignette section on corrupt cache", call.=FALSE)
        } else if (cnt == 0){
            bfcadd(bfc, rname=rnames, fpath=hubpath, proxy=proxy)
        } else {
            bfcdownload(bfc, rid=rid, ask=FALSE, proxy=proxy)
        }
        TRUE
    }, error=function(err) {
        warning("download failed",
                "\n  hub path: ", sQuote(hubpath),
                "\n  cache resource: ", sQuote(rnames),
                "\n  reason: ", conditionMessage(err),
                call.=FALSE)
        FALSE
    })

}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

.Hub_get1 <-
    function(x, force, verbose)
{
    if (!length(x))
        stop("no records found for the given index")
    if (length(x) != 1L)
        stop("'i' must be length 1")

    ## Add 'Resource' postfix to DispatchClass name
    className <- sprintf("%sResource", .dataclass(x))
    if (is.null(getClassDef(className))) {
        msg <- sprintf("'%s' not available in this version of the
            package; use BiocManager::install() to update?",
            names(x))
        stop(paste(strwrap(msg, exdent=2), collapse="\n"), call.=FALSE)
    }

    tryCatch({
        class <- new(className, hub=x)
    }, error=function(err) {
        stop("failed to create 'HubResource' instance",
             "\n  name: ", names(x),
             "\n  title: ", x$title,
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })

    tryCatch({
        fls <-  cache(getHub(class), force=force, verbose=verbose)
        .get1(class)
    }, error=function(err) {
        stop("failed to load resource",
             "\n  name: ", names(x),
             "\n  title: ", x$title,
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### converting Legacy to New
###

convertHub <- function(oldcachepath=NULL, newcachepath=NULL,
                       hubType=c("AnnotationHub", "ExperimentHub"),
                       proxy=getAnnotationHubOption("PROXY"),
                       max.downloads=getAnnotationHubOption("MAX_DOWNLOADS"),
                       force=FALSE, verbose=TRUE)
{

    hubType <- match.arg(hubType)

    if (is.null(oldcachepath)){
        .CACHE_ROOT_orig <- ifelse(hubType=="AnnotationHub", ".AnnotationHub", ".ExperimentHub")
        path <- switch(.Platform$OS.type, unix = path.expand("~/"),
                       windows= file.path(gsub("\\\\", "/",
                           Sys.getenv("HOME")), "AppData"))

        oldcachepath <- file.path(path, .CACHE_ROOT_orig)
    }
    orig_files <- dir(oldcachepath)
    download_files <- setdiff(orig_files,
                              c("annotationhub.sqlite3", "experimenthub.sqlite3",
                                "index.rds"))


    if(verbose) message("Attempting to redownload: ", length(download_files), " hub resources")

    if(is.null(newcachepath)){
        if(hubType=="AnnotationHub"){
            hub <- AnnotationHub()
        }else{
            hub <- ExperimentHub::ExperimentHub()
        }
    } else {
        if(hubType=="AnnotationHub"){
            hub <- AnnotationHub(cache=newcachepath)
        }else{
            hub <- ExperimentHub::ExperimentHub(cache=newcachepath)
        }
    }

    mapping_ids <- .named_cache_path(hub)
    cachepath <- mapping_ids[which(mapping_ids %in% as.numeric(download_files))]

    dxFnd <- as.numeric(download_files) %in% mapping_ids
    notFnd <- download_files[!dxFnd]

    subHub <- hub[which(hub$ah_id %in% names(cachepath))]

    tryCatch({
        .cache_internal(subHub,
                        proxy=proxy, max.downloads=max.downloads,
                        force=force, verbose=verbose)
    }, error = function(err) {
        warning("Not all resources downloaded correctly.",
                "It may be beneficial to rerun 'convertHub()'",
                "\n  reason: ", conditionMessage(err),
                call.=FALSE)
    })

    if (length(notFnd) != 0){
        warning("The following files could not be re-downloaded.",
                "\n      ",
                paste0(file.path(oldcachepath, notFnd), collapse = "\n      "),
        #    alldatainfo <- .IdsInfo(hub)
        #    datainfo <- alldatainfo[match(notFnd, alldatainfo$fetch_id),]
                "\n  For more information on these files, See: ?getInfoOnIds")
    }

    # should their be an option to
    # delete old cache and files?
    # could get complicated if older files not found etc.


    hubCache(hub)
}
