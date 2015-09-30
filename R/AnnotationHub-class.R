## Definition and constructor

.AnnotationHub <- setClass("AnnotationHub",
    representation(hub="character", cache="character", date="character",
                   .db_env="environment", .db_index="character",
                   .db_uid="integer")
)

.db_env <- new.env(parent=emptyenv())

## Add code to check : https://annotationhub.bioconductor.org/metadata/highest_id
## And compare to the highest ID locally (to see if we have the latest DB)
## And if not, delete the DB so it will be re-downloaded...
AnnotationHub <-
    function(..., hub=hubOption("URL"), cache=hubOption("CACHE"),
             max.downloads=hubOption("MAX_DOWNLOADS"))
{
    if (!isSingleString(hub))
        stop("'hub' must be a single string (url)")
    if (!isSingleString(cache))
        stop("'cache' must be a single string (directory path)")
    if (!isSingleInteger(max.downloads)) {
        msg <- "'max.downloads' must be a single integer
                (resource download throttle)"
        stop(paste(strwrap(msg), collapse="\n"))
    }

    .db_path <- .cache_create(cache)
    db_env <- new.env(parent=emptyenv())
    .db_connection <- .db_get(.db_path, hub)
    db_env[["db_connection"]] <- .db_connection
    db_env[["db_path"]] <- dbfile(db_env[["db_connection"]])
    .date <- max(.possibleDates(.db_connection))    
    .db_uid <- .db_uid0(.db_connection, .date, .db_path)
    hub <- .AnnotationHub(cache=cache, hub=hub, date=.date, .db_env=db_env,
                          .db_uid=.db_uid)
    message("snapshotDate(): ", snapshotDate(hub))
    .db_index <- .db_index_create(hub)
    initialize(hub, .db_index=.db_index)
}

## Helpers to get a fresh metadata DB connection
.db_get_db <- function(path, hub) {
    ## download or cache
    tryCatch({
        need <- !file.exists(path)
        .hub_resource(.hub_metadata_path(hub), basename(path)[need], path[need])
    }, error=function(err) {
        stop("'AnnotationHub' failed to create local data base",
             "\n  database: ", sQuote(path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
    .db_open(path)
}

.db_open <- function(path) {
    tryCatch({
        if (is.null(.db_env[["db_connection"]])) {
            .db_env[["db_connection"]] <- dbConnect(SQLite(), path)
        }
    }, error=function(err) {
        stop("'AnnotationHub' failed to connect to local data base",
             "\n  database: ", sQuote(path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
    .db_env[["db_connection"]]
}

.db_close <- function() {
    con <- .db_env[["db_connection"]]
    if (!is.null(con)) {
        if (RSQLite::dbIsValid(con))
            dbDisconnect(con)
        .db_env[["db_connection"]] <- NULL
    }
}

.db_is_current <- function(path, hub) {
    tryCatch({
        url <- paste0(hub, '/metadata/database_timestamp')
        onlineTime <- as.POSIXct(content(GET(url)))        

        con <- .db_get_db(path, hub)
        sql <- "SELECT * FROM timestamp"
        localTime <- as.POSIXct(dbGetQuery(con, sql)[[1]])
        .db_close()

        onlineTime == localTime
    }, error=function(e) {
        warning("'AnnotationHub' database may not be current",
                "\n  database: ", sQuote(path),
                "\n  reason: ", conditionMessage(e),
                call.=FALSE)
        ## TRUE even though not current, e.g., no internet connection
        TRUE
    })
}

.db_is_valid <- function(con) {
    ## Some very minor testing to make sure metadata DB is intact.
    tryCatch({
        ## required tables present?
        expected <- c("biocversions", "input_sources", "location_prefixes",
                      "rdatapaths", "recipes", "resources", "statuses",
                      "tags", "timestamp")
        if (!all(expected %in% dbListTables(con)))
            stop("missing tables")
        ## any resources?
        sql <- "SELECT COUNT(id) FROM resources"
        if (dbGetQuery(con, sql)[[1]] == 0L)
            stop("empty 'resources' table")
    }, error=function(err) {
        stop("'AnnotationHub' database corrupt; remove it and try again",
             "\n  database: ", sQuote(dbfile(con)),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
    TRUE
}

.db_create_view <- function(con) {
    ## Create a 'flat' view for simplified access, e.g., via dlyr
    sql <- 
        "CREATE VIEW IF NOT EXISTS AllCoreData AS
         SELECT
           ah_id, title, dataprovider, species, taxonomyid, genome,
           description, coordinate_1_based, maintainer, status_id,
           location_prefix_id, recipe_id, rdatadateadded,
           rdatadateremoved, record_id, preparerclass, rdatapath,
           dispatchclass, sourcesize, sourceurl, sourceversion,
           sourcemd5, sourcelastmodifieddate, rdp.resource_id,
           sourcetype
         FROM
           resources AS res,
           rdatapaths AS rdp, 
           input_sources AS ins
         WHERE res.id = rdp.resource_id 
         AND res.id = ins.resource_id;"
    dbGetQuery(con, sql)
}

.db_get <- function(path, hub) {
    update <- !file.exists(path)
    if (!update && !file.size(path)) {
        file.remove(path)
        update <- TRUE
    }
    if (!update && !.db_is_current(path, hub)) {
        file.remove(path)
        update <- TRUE
    }
    if (update)
        message("updating AnnotationHub metadata: ", appendLF=FALSE)
    con <- .db_get_db(path, hub)
    .db_is_valid(con)
    .db_create_view(con)
    con
}


.db_uid0 <- function(con, .date, path){
    tryCatch({
        uid <- .uid0(con, .date)
        sort(uid)
    }, error=function(err) {
        stop("'AnnotationHub' failed to connect to local data base",
             "\n  database: ", sQuote(path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
}

.db_index_file <- function(x)
    file.path(hubCache(x), "index.rds")

.db_index_create <- function(x) {
    fl <- .db_index_file(x)
    if (file.exists(fl) && (file.mtime(fl) > file.mtime(dbfile(x))))
        return(fl)
    
    tryCatch({
        tbl <- .resource_table(x)
        tbl <- setNames(do.call("paste", c(tbl, sep="\r")), rownames(tbl))
        saveRDS(tbl, fl)
    }, error=function(err) {
        stop("'AnnotationHub' failed to create index",
             "\n  hubCache(): ", hubCache(x),
             "\n  reason: ", conditionMessage(err))
    })

    fl
}

.db_index_load <- function(x)
    readRDS(.db_index_file(x))[names(x)]

## accessors

.cache <- function(x) slot(x, "cache")

.hub <- function(x) slot(x, "hub")

.db_index <- function(x) slot(x, ".db_index")

.db_uid <- function(x) slot(x, ".db_uid")

`.db_uid<-` <- function(x, ..., value)
{
    bad <- value[!value %in% .db_uid(x)]
    if (any(bad))
        stop("invalid subscripts: ",
             paste(sQuote(S4Vectors:::selectSome(bad)), collapse=", "))
    slot(x, ".db_uid") <- value
    x
}

setMethod("dbconn", "AnnotationHub", function(x) {
    conn <- slot(x, ".db_env")[["db_connection"]]
    if (!dbIsValid(conn)) {
        conn <- .db_open(dbfile(x))
        slot(x, ".db_env")[["db_connection"]] <- conn
    }
    conn
})

setMethod("dbfile", "AnnotationHub", function(x) {
    slot(x, ".db_env")[["db_path"]]
})

.snapshotDate <- function(x) slot(x, "date")

## TODO: This isn't working yet!  Not only does the date not get swapped yet, 
## but I also have to update the uids (reset those) when I do this step... 
## (uid0)
.replaceSnapshotDate <- function(x, value) {
 #   snapshotDate <- as.POSIXlt(value) ## TODO: use this class to store dates.
    snapshotDate <- value
    if (snapshotDate == snapshotDate(x))
        return (x)
    possibleDates <- possibleDates(x)
    if (!snapshotDate %in% possibleDates){
        newval <- strptime(snapshotDate, "%Y-%m-%d")
	if(is.na(newval))
	    stop("'value' should be in YYYY-MM-DD format")
	if(newval > Sys.time())
	    stop("this date is in the future") 
	res <- mapply(function(x,y) {
    	    x = strptime(x, "%Y-%m-%d")
            y = strptime(y, "%Y-%m-%d")
            newval>=x & newval<=y
        }, possibleDates[-length(possibleDates)],  possibleDates[-1])
	if(length(which(res))==1)
           snapshotDate <- possibleDates[res]
        else
           snapshotDate <- snapshotDate(x)
    }
    ## Changing the date is two step process.
    ## 1st we update the x@.db_uid
    conn <- dbconn(x)
    x@.db_uid <- .uid0(conn, snapshotDate)
    ## then we update the date slot    
    x@date <- snapshotDate
    x
} 

## snapshotDate (setter and getter and then the code to filter based on this...)
## max(possibleDates(mh)) is the default
setGeneric("snapshotDate", function(x, ...) standardGeneric("snapshotDate"))

setGeneric("snapshotDate<-", signature="x",
           function(x, value) standardGeneric("snapshotDate<-"))

setMethod("snapshotDate", "AnnotationHub", .snapshotDate)

setReplaceMethod("snapshotDate", "AnnotationHub", .replaceSnapshotDate)


## 
## API
##

## list-like

setMethod("names", "AnnotationHub",
    function(x)
{
    as.character(names(.db_uid(x)))
})

setMethod("length", "AnnotationHub",
    function(x)
{
    length(.db_uid(x))
})

setMethod("[", c("AnnotationHub", "numeric", "missing"),
    function(x, i, j, ..., drop=TRUE) 
{
    idx <- na.omit(match(i, seq_along(.db_uid(x)))) 
    ## seq_along creates a special problem for show()...
    .db_uid(x) <- .db_uid(x)[idx]
    x
})

setMethod("[", c("AnnotationHub", "logical", "missing"),
    function(x, i, j, ..., drop=TRUE)
{
    i[is.na(i)] <- FALSE
    .db_uid(x) <- .db_uid(x)[i]
    x
})

setMethod("[", c("AnnotationHub", "character", "missing"),
    function(x, i, j, ..., drop=TRUE) 
{
    idx <- na.omit(match(i, names(.db_uid(x))))
    .db_uid(x) <- .db_uid(x)[idx]
    x
})

setReplaceMethod("[", c("AnnotationHub", "numeric", "missing", "AnnotationHub"),
    function(x, i, j, ..., value)
{
    .db_uid(x)[i] <- .db_uid(value)
    x
})

setReplaceMethod("[", c("AnnotationHub", "logical", "missing", "AnnotationHub"),
    function(x, i, j, ..., value)
{
    .db_uid(x)[i] <- .db_uid(value)
    x
})

setReplaceMethod("[", 
    c("AnnotationHub", "character", "missing", "AnnotationHub"),
    function(x, i, j, ..., value)
{
    idx <- match(i, names(.db_uid(x)))
    isNA <- is.na(idx)
    .db_uid(x)[idx[!isNA]] <- .db_uid(value)[!isNA]
    x
})

setMethod("fileName", signature=(object="AnnotationHub"),
    function(object, ...)
{
    cachepath <- .named_cache_path(object)
    cachepath[!file.exists(cachepath)] <- NA_character_
    cachepath
})


## $, query / subset

setMethod("$", "AnnotationHub",
    function(x, name)
{
    switch(name,
     "tags"=unname(.collapse_as_string(x, .tags)),
     "rdataclass"=unname(.collapse_as_string(x, .rdataclass)),
     "sourceurl"=unname(.collapse_as_string(x, .sourceurl)),
     "sourcetype"=unname(.collapse_as_string(x, .sourcetype)),      
     .resource_column(x, name))    ## try to get it from main resources table
})

.DollarNames.AnnotationHub <-
    function(x, pattern="")
{
    values <- c(.resource_columns(), "tags", "rdataclass",
                "sourceurl", "sourcetype")
    grep(pattern, values, value=TRUE)
}

setGeneric("query", function(x, pattern, ...) standardGeneric("query"),
    signature="x")

setMethod("query", "AnnotationHub",
    function(x, pattern, ignore.case=TRUE, pattern.op=`&`)
{
    tbl <- .db_index_load(x)
    idx <- logical()
    for (pat in pattern) {
        idx0 <- grepl(pat, tbl, ignore.case=ignore.case)
        if (length(idx))
            idx <- pattern.op(idx, idx0) # pattern.op for combining patterns
        else
            idx <- idx0
    }

    x[idx]
})

## Test: ahs <- query(ah, 'ChainFile')

setMethod("subset", "AnnotationHub",
    function(x, subset)
{
    tbl <- mcols(x)
    idx <- S4Vectors:::evalqForSubset(subset, tbl)
    x[idx]
})

as.list.AnnotationHub <- function(x, ..., use.names=TRUE) {
    ans <- lapply(seq_along(x), function(i, x) x[i], x)
    if (use.names)
        names(ans) <- names(x)
    ans
}

setMethod("as.list", "AnnotationHub", as.list.AnnotationHub)

setMethod("c", "AnnotationHub",
    function(x, ..., recursive=FALSE)
{
    if (!identical(recursive, FALSE)) 
        stop("'recursive' argument not supported")
    if (missing(x)) 
        args <- unname(list(...))
    else args <- unname(list(x, ...))

    .test <- function(args, fun, what) {
        ans <- unique(vapply(args, fun, character(1)))
        if (length(ans) != 1L)
            stop(sprintf("'%s' differs between arguments", what))
    }
    .test(args, hubCache, "hubCache")
    .test(args, hubUrl, "hubUrl")
    .test(args, snapshotDate, "snapshotDate")
    .test(args, dbfile, "dbfile")

    db_uid <- unlist(lapply(unname(args), .db_uid))
    if (length(db_uid) == 0 && is.null(names(db_uid)))
        names(db_uid) <- character()
    udb_uid <- unique(db_uid)
    idx <- match(udb_uid, db_uid)
    .db_uid <- setNames(udb_uid, names(db_uid)[idx])
    initialize(args[[1]], .db_uid=.db_uid)
})

## trace (as below) wasn't working (not sure why)
## trace(subset, browser(), signature='AnnotationHub')
## debug(AnnotationHub:::.subset)
## Tests:
## library(AnnotationHub);debug(AnnotationHub:::.subset);ah = AnnotationHub()
## ahs <- subset(ah, ah$genome=='ailMel1')
## ahs <- subset(ah, ah$rdataclass=='VCF')

.AnnotationHub_get1 <-
    function(x)
{
    if (length(x) != 1L)
        stop("'i' must be length 1")

    className <- sprintf("%sResource", .dataclass(x))
    if (is.null(getClassDef(className))) {
        msg <- sprintf("'%s' not available in this version of the
            AnnotationHub package; use biocLite('AnnotationHub') to update?",
            names(x))
        stop(paste(strwrap(msg, exdent=2), collapse="\n"), call.=FALSE)
    }

    tryCatch({
        class <- new(className, hub=x)
    }, error=function(err) {
        stop("failed to create 'AnnotationHubResource' instance",
             "\n  name: ", names(x),
             "\n  title: ", x$title,
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })

    tryCatch({
        .get1(class)
    }, error=function(err) {
        stop("failed to load 'AnnotationHub' resource",
             "\n  name: ", names(x),
             "\n  title: ", x$title,
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
}

setMethod("[[", c("AnnotationHub", "numeric", "missing"),
    function(x, i, j, ...)
{
    .AnnotationHub_get1(x[i])
})

setMethod("[[", c("AnnotationHub", "character", "missing"),
    function(x, i, j, ...)
{
    idx <- match(i, names(.db_uid(x)))
    .AnnotationHub_get1(x[idx])
})

## 
## show
##

.pprintf0 <- function(fmt, ...)
    paste(strwrap(sprintf(fmt, ...), exdent=2), collapse="\n# ")

.pprintf1 <-
    function(column, values, width=getOption("width") - 1L)
{
    answer <- sprintf("# $%s: %s", column, paste(values, collapse=", "))
    ifelse(nchar(answer) > width,
           sprintf("%s...\n", substring(answer, 1L, width - 3L)),
           sprintf("%s\n", answer))
}

.show <-
    function(object)
{
    .some <-
        function(elt, nh, nt, fill="...", width=getOption("width") - 13L)
    {
        answer <- if (length(elt) < nh + nt + 1L)
            elt
        else
            c(head(elt, nh), fill, tail(elt, nt))
        ifelse(nchar(answer) > width,
               sprintf("%s...", substring(answer, 1L, width-3L)),
               answer)
    }

    cat(.pprintf1("dataprovider", .count_resources(object, "dataprovider")))
    cat(.pprintf1("species", .count_resources(object, "species")))
    cat(.pprintf1("rdataclass",
                  .count_join_resources(object, "rdatapaths", "rdataclass")))
    shown <- c("dataprovider", "species", "rdataclass", "title")
    cols <- paste(setdiff(names(mcols(object[0])), shown), collapse=", ")
    cat(.pprintf0("# additional mcols(): %s", cols), "\n")
    cat(.pprintf0("# retrieve records with, e.g., 'object[[\"%s\"]]'",
                  names(object)[[1]]), "\n")

    if (len <- length(object)) {
        nhead <- get_showHeadLines()
        ntail <- get_showTailLines()
        title <- .title_data.frame(object)
        rownames <- paste0("  ", .some(rownames(title), nhead, ntail))
        out <- matrix(c(.some(rep("|", len), nhead, ntail, fill=""),
                        .some(title[[1]], nhead, ntail)),
                      ncol=2L,
                      dimnames=list(rownames, c("", "title")))

        cat("\n")
        print(out, quote=FALSE, right=FALSE)
    }
}

.show1 <-
    function(object)
{
    rsrc <- .resource_table(object)
    size <- .collapse_as_string(object, .sourcesize)   
    date <- .collapse_as_string(object, .sourcelastmodifieddate)
  
    cat("# names(): ", names(object)[[1]], "\n", sep="")
    cat(.pprintf1("dataprovider", rsrc[["dataprovider"]]))
    cat(.pprintf1("species", rsrc[["species"]]))
    cat(.pprintf1("rdataclass", rsrc[["rdataclass"]]))
    cat(.pprintf1("title", rsrc[["title"]]))
    cat(.pprintf1("description", rsrc[["description"]]))
    cat(.pprintf1("taxonomyid", rsrc[["taxonomyid"]]))
    cat(.pprintf1("genome", rsrc[["genome"]]))
    cat(.pprintf1("sourcetype", rsrc[["sourcetype"]]))
    cat(.pprintf1("sourceurl", rsrc[["sourceurl"]]))
    cat(.pprintf1("sourcelastmodifieddate", date))
    cat(.pprintf1("sourcesize", size))
    cat(.pprintf0("# $tags: %s", rsrc[["tags"]]), "\n")
    cat(.pprintf0("# retrieve record with 'object[[\"%s\"]]'",
                  names(object)[[1]]), "\n")
}

setMethod("show", "AnnotationHub", function(object) 
{
    len <- length(object)
    cat(sprintf("%s with %d record%s\n", class(object), len,
                ifelse(len == 1L, "", "s")))
    cat("# snapshotDate():", snapshotDate(object), "\n")

    if (len > 1)
        .show(object)
    else if (len == 1)
        .show1(object)
})
