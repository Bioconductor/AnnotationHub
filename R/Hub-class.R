### =========================================================================
### Hub objects
### -------------------------------------------------------------------------
###

setClass("Hub",
    representation("VIRTUAL",
        hub="character", 
        cache="character", 
        date="character",
        .db_env="environment", 
        .db_index="character",
        .db_uid="integer"
    )
#    prototype(
#        .db_env=new.env(parent=emptyenv()),
#    )
)

## FIXME: remove this and use 'prototype'? Shouldn't we be accessing
##        .db_env via the hub object?
.db_env <- new.env(parent=emptyenv())


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.Hub <- function(object)
{
    message <- NULL
    if (!isSingleString(hubUrl(object)))
        stop("'hubUrl(object)' must be a single string (url)")
    if (!isSingleString(hubCache(object)))
        stop("'hubCache(object)' must be a single string (directory path)")
    message
}

setValidity("Hub",
    function(object)
    {
        problems <- .valid.Hub(object)
        if (is.null(problems)) TRUE else problems
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### db helpers 
###

## Helpers to get a fresh metadata DB connection
.db_get_db <- function(path, hub) {
    ## download or cache
    tryCatch({
        need <- !file.exists(path)
        .hub_resource(.hub_metadata_path(hub), basename(path)[need], path[need])
    }, error=function(err) {
        stop("failed to create local data base",
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
        stop("failed to connect to local data base",
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
        onlineTime <- as.POSIXct(content(GET(url, hubOption("PROXY"))))

        con <- .db_get_db(path, hub)
        sql <- "SELECT * FROM timestamp"
        localTime <- as.POSIXct(dbGetQuery(con, sql)[[1]])
        .db_close()

        onlineTime == localTime
    }, error=function(e) {
        warning("database may not be current",
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
        stop("database is corrupt; remove it and try again",
             "\n  database: ", sQuote(dbfile(con)),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
    TRUE
}

.db_create_view <- function(con) {
    ## Create a 'flat' view for simplified access, e.g., via dplyr
    sql <- 
        "CREATE VIEW IF NOT EXISTS AllCoreData AS
         SELECT DISTINCT
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
        message("updating metadata: ", appendLF=FALSE)
    con <- .db_get_db(path, hub)
    .db_is_valid(con)
    .db_create_view(con)
    con
}

.db_index_file <- function(x)
    file.path(hubCache(x), "index.rds")

.db_index_load <- function(x)
    readRDS(.db_index_file(x))[names(x)]

dbuid0 <- function(con, .date, path){
    tryCatch({
        uid <- .uid0(con, .date)
        sort(uid)
    }, error=function(err) {
        stop("failed to connect to local data base",
             "\n  database: ", sQuote(path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
}

dbcreateindex <- function(x) {
    fl <- .db_index_file(x)
    if (file.exists(fl) && (file.mtime(fl) > file.mtime(dbfile(x))))
        return(fl)
 
    tryCatch({
        tbl <- .resource_table(x)
        tbl <- setNames(do.call("paste", c(tbl, sep="\r")), rownames(tbl))
        saveRDS(tbl, fl)
    }, error=function(err) {
        stop("failed to create index",
             "\n  hubCache(): ", hubCache(x),
             "\n  reason: ", conditionMessage(err))
    })

    fl
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

.cache <- function(x) slot(x, "cache")

.hub <- function(x) slot(x, "hub")

dbindex <- function(x) slot(x, ".db_index")
`dbindex<-` <- function(x, ..., value) 
{
    if (length(value) > 1L)
        stop("'value' must be length 1")
    if (!is(value, "character"))
        stop("'value' must be a character")
    slot(x, ".db_index") <- value
    x
}

dbuid <- function(x) slot(x, ".db_uid")
`dbuid<-` <- function(x, ..., value)
{
    bad <- value[!value %in% dbuid(x)]
    if (any(bad))
        stop("invalid subscripts: ",
             paste(sQuote(S4Vectors:::selectSome(bad)), collapse=", "))
    slot(x, ".db_uid") <- value
    x
}

setMethod("dbconn", "Hub", function(x) {
    conn <- slot(x, ".db_env")[["db_connection"]]
    if (!dbIsValid(conn)) {
        conn <- .db_open(dbfile(x))
        slot(x, ".db_env")[["db_connection"]] <- conn
    }
    conn
})

setMethod("dbfile", "Hub", function(x) {
    slot(x, ".db_env")[["db_path"]]
})


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

setGeneric("snapshotDate", function(x, ...) standardGeneric("snapshotDate"))
setMethod("snapshotDate", "Hub", function(x) slot(x, "date"))

setGeneric("snapshotDate<-", signature="x",
           function(x, value) standardGeneric("snapshotDate<-"))
setReplaceMethod("snapshotDate", "Hub", .replaceSnapshotDate)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor-like methods 
###

setMethod("mcols", "Hub", function(x) DataFrame(.resource_table(x))) 

setMethod("names", "Hub",
    function(x)
{
    as.character(names(dbuid(x)))
})

setMethod("fileName", signature=(object="Hub"),
    function(object, ...)
{
    cachepath <- .named_cache_path(object)
    cachepath[!file.exists(cachepath)] <- NA_character_
    cachepath
})

setMethod("length", "Hub",
    function(x)
{
    length(dbuid(x))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting 
###

setMethod("[", c("Hub", "numeric", "missing"),
    function(x, i, j, ..., drop=TRUE) 
{
    idx <- na.omit(match(i, seq_along(dbuid(x)))) 
    ## seq_along creates a special problem for show()...
    dbuid(x) <- dbuid(x)[idx]
    x
})

setMethod("[", c("Hub", "logical", "missing"),
    function(x, i, j, ..., drop=TRUE)
{
    i[is.na(i)] <- FALSE
    dbuid(x) <- dbuid(x)[i]
    x
})

setMethod("[", c("Hub", "character", "missing"),
    function(x, i, j, ..., drop=TRUE) 
{
    idx <- na.omit(match(i, names(dbuid(x))))
    dbuid(x) <- dbuid(x)[idx]
    x
})

setReplaceMethod("[", c("Hub", "numeric", "missing", "Hub"),
    function(x, i, j, ..., value)
{
    dbuid(x)[i] <- dbuid(value)
    x
})

setReplaceMethod("[", c("Hub", "logical", "missing", "Hub"),
    function(x, i, j, ..., value)
{
    dbuid(x)[i] <- dbuid(value)
    x
})

setReplaceMethod("[", 
    c("Hub", "character", "missing", "Hub"),
    function(x, i, j, ..., value)
{
    idx <- match(i, names(dbuid(x)))
    isNA <- is.na(idx)
    dbuid(x)[idx[!isNA]] <- dbuid(value)[!isNA]
    x
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### $, query and subset methods
###

setMethod("$", "Hub",
    function(x, name)
{
    switch(name,
     "tags"=unname(.collapse_as_string(x, .tags)),
     "rdataclass"=unname(.collapse_as_string(x, .rdataclass)),
     "sourceurl"=unname(.collapse_as_string(x, .sourceurl)),
     "sourcetype"=unname(.collapse_as_string(x, .sourcetype)), 
     .resource_column(x, name))    ## try to get it from main resources table
})

.DollarNames.Hub <-
    function(x, pattern="")
{
    values <- c(.resource_columns(), "tags", "rdataclass",
                "sourceurl", "sourcetype")
    grep(pattern, values, value=TRUE)
}

setGeneric("query", function(x, pattern, ...) standardGeneric("query"),
    signature="x")

setMethod("query", "Hub",
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

setMethod("subset", "Hub",
    function(x, subset)
{
    tbl <- mcols(x)
    idx <- S4Vectors:::evalqForSubset(subset, tbl)
    x[idx]
})

as.list.Hub <- function(x, ..., use.names=TRUE) {
    ans <- lapply(seq_along(x), function(i, x) x[i], x)
    if (use.names)
        names(ans) <- names(x)
    ans
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setMethod("as.list", "Hub", as.list.Hub)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###

setMethod("c", "Hub",
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

    db_uid <- unlist(lapply(unname(args), dbuid))
    if (length(db_uid) == 0 && is.null(names(db_uid)))
        names(db_uid) <- character()
    udb_uid <- unique(db_uid)
    idx <- match(udb_uid, db_uid)
    .db_uid <- setNames(udb_uid, names(db_uid)[idx])
    initialize(args[[1]], .db_uid=.db_uid)
})

## FIXME:
## trace (as below) wasn't working (not sure why)
## trace(subset, browser(), signature='AnnotationHub')
## debug(AnnotationHub:::.subset)
## Tests:
## library(AnnotationHub);debug(AnnotationHub:::.subset);ah = AnnotationHub()
## ahs <- subset(ah, ah$genome=='ailMel1')
## ahs <- subset(ah, ah$rdataclass=='VCF')

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show 
###

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

setMethod("show", "Hub", function(object) 
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### display 
###

.display <- function(object, ...) { 
    #metadata- do not touch
    md <- mcols(object)
        
    #create a data frame copy for edits called df
    df <- as.data.frame(md)
        
    #dropping column names
    drops <-c("title", "taxonomyid", "sourceurl")
    df <- df[,!(names(df) %in% drops), drop=FALSE]
    
    summaryMessage = capture.output(show(object))
    serverOptions= list(
        bSortClasses=TRUE,
        aLengthMenu = c(1000, 5000, "All"),
        iDisplayLength = 1000,
        "sDom" = '<"top"i>rt<"top"f>lt<"bottom"p><"clear">')
        
    d <- display(object =df, summaryMessage = summaryMessage, 
                 serverOptions = serverOptions)
    idx <- rownames(d)
    object[idx]
}

setMethod("display", signature(object="Hub"),
          function(object) .display(object)
)
