## Definition and constructor

.AnnotationHub <- setClass("AnnotationHub",
    representation(hub="character", cache="character", date="character",
                   .db_connection="SQLiteConnection", .db_uid="integer")
)

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
        stop("'cache' must be a single string (file path)")
    if (!isSingleInteger(max.downloads)) {
        msg <- "'max.downloads' must be a single integer
                (resource download throttle)"
        stop(paste(strwrap(msg), collapse="\n"))
    }

    .db_path <- .cache_create(cache)
    .db_connection <- .getDbConn(.db_path, hub)
    .date <- max(.possibleDates(.db_connection))    
    .db_uid <- .getDbUid(.db_path, .db_connection, .date)
    .AnnotationHub(cache=cache, hub=hub, date=.date,
                   .db_connection=.db_connection, .db_uid=.db_uid)
}


.makeConnection <- function(db_path){
    tryCatch({
        dbConnect(SQLite(), db_path)
    }, error=function(err) {
        stop("'AnnotationHub' failed to open local data base",
             "\n  database: ", sQuote(db_path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })    
}

.createViewIfneeded <- function(con){
    ## Create a view for dlyr folks (can remove stuff here as needed)
    sql <- paste0("CREATE VIEW IF NOT EXISTS AllCoreData AS SELECT",
                  " ah_id,title,dataprovider,species,taxonomyid,genome,",
                  "description,coordinate_1_based,maintainer,status_id,",
                  "location_prefix_id,recipe_id,rdatadateadded,",
                  "rdatadateremoved,record_id,preparerclass,rdatapath,",
                  "dispatchclass,sourcesize,sourceurl,sourceversion,",
                  "sourcemd5,sourcelastmodifieddate,rdp.resource_id,",
                  "sourcetype FROM resources AS res, rdatapaths AS rdp, ",
                  "input_sources AS ins WHERE res.id = rdp.resource_id ",
                  "AND res.id = ins.resource_id ") 
    dbGetQuery(con, sql)
}

## Helper to make the metadata DB connection
.getDbConn <- function(db_path, hub){    
    if(!file.exists(db_path)){ .getMetadataDb(db_path, hub) }
    ## This just gets a formal DB connection object
    .db_connection <- .makeConnection(db_path)
    ## Before we try to connect to the DB and check for staleness, 
    ## lets 1st check that the DB we have is valid
    .checkDBIsValid(.db_connection)
    
    ## Here I need to test the DB that I have to make sure its current.
    if(.isDbStale(.db_connection)){ ## get another one
        ## Then disconnect from the old DB
        dbDisconnect(.db_connection)
        ## TODO: delete the existing one  
        file.remove(db_path)
        ## AND replace it with a new one
        .getMetadataDb(db_path, hub)
        ## And then reconnect to the updated DB 
        .db_connection <- .makeConnection(db_path)
        ## New DB?  So make sure the new one is valid as well.
        .checkDBIsValid(.db_connection)
    }
    ## if we need to, then make a view for dplyr folks
    .createViewIfneeded(.db_connection)
    
    ## always return a good connection
    .db_connection
}

## Some very minor testing to make sure metadata DB is intact.
.checkDBIsValid <- function(con){
    ## are required tables present?
    expected <- c("biocversions","input_sources","location_prefixes",
                  "rdatapaths", "recipes","resources", 
                  "statuses","tags", "timestamp")
    tables <- dbListTables(con)
    if (!all(expected %in% tables))
        stop("'AnnotationHub' database corrupt; remove it and try again",
             "\n  database: ", sQuote(con@dbname),
             "\n  reason: missing tables",
             call.=FALSE)
    ## are there values for resources?
    sql <- "SELECT count(id) FROM resources"
    numResources <- dbGetQuery(con, sql)[[1]]
    if (numResources < 1L)
        stop("'AnnotationHub' database corrupt; remove it and try again",
             "\n  database: ", sQuote(con@dbname),
             "\n  reason: database empty",
             call.=FALSE)
    TRUE
}


## Helper to just get a fresh the metadata DB connection
.getMetadataDb <- function(db_path, hub){
    ## This makes the DB (if it's absent)
    tryCatch({
            .hub_resource(.hub_metadata_path(hub), basename(db_path), db_path)
    }, error=function(err) {
        stop("'AnnotationHub' failed to create local data base",
             "\n  database: ", sQuote(db_path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
}

## Helper checks if local Db is stale, returns TRUE if it needs an update.
.isDbStale <- function(con){
    ## Compare the timestamp online to the one in the DB
    url <- paste0(hubOption('URL'), '/metadata/database_timestamp')
    tryCatch({
        onlineTime <- as.POSIXct(content(GET(url)))        
        ## then get the db timestamp
        sql <- "SELECT * FROM timestamp"
        localTime <- as.POSIXct(dbGetQuery(con, sql)[[1]])
        onlineTime > localTime
    }, error=function(e) {
        warning("'AnnotationHub' database may not be current",
                "\n  database: ", sQuote(con@dbname),
                "\n  reason: ", conditionMessage(e),
                call.=FALSE)
        FALSE
    })
}


## Helper to get relevant .uids for the object
.getDbUid <- function(db_path, .db_connection, .date){
    .db_uid <- tryCatch({
        uid <- .uid0(.db_connection, .date)
        sort(uid)
    }, error=function(err) {
        stop("'AnnotationHub' failed to connect to local data base ",
             "\n  database: ", sQuote(db_path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
    .db_uid
}


## accessors

.cache <- function(x) slot(x, "cache")

.hub <- function(x) slot(x, "hub")

.db_uid <- function(x) slot(x, ".db_uid")

`.db_uid<-` <- function(x, ..., value)
{
    bad <- value[!value %in% .db_uid(x)]
    if (any(bad))
        stop("invalid subscripts: ",
             paste(sQuote(BiocGenerics:::selectSome(bad)), collapse=", "))
    slot(x, ".db_uid") <- value
    x
}

setMethod("dbconn", "AnnotationHub", function(x) slot(x, ".db_connection"))

setMethod("dbfile", "AnnotationHub", function(x) dbconn(x)@dbname)

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
	    stop("This date is in the future") 
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
    function(object)
{
    cache(object)
})


## $, query / subset

setMethod("$", "AnnotationHub",
    function(x, name)
{
    switch(name,
     "tags"=unname(.collapse_as_string(x,.tags,'tag')),
     "rdataclass"=unname(.collapse_as_string(x,.rdataclass,'rdataclass')),
     "sourceurl"=unname(.collapse_as_string(x,.sourceurl,'sourceurl')),
     "sourcetype"=unname(.collapse_as_string(x,.sourcetype,'sourcetype')),      
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
    tbl <- mcols(x)
    idx <- logical()
    for (pat in pattern) {
        idx0 <- logical(nrow(tbl))
        for (column in names(tbl))   # '|' across columns
            idx0 <- idx0 | grepl(pat, tbl[[column]], ignore.case=ignore.case)
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

    tryCatch({
        class <- new(sprintf("%sResource", .dataclass(x)), hub=x)
    }, error=function(err) {
        msg <- sprintf("failed to create a 'AnnotationHubResource'
            instance for hub resource %s of class %s; reason: %s",
            sQuote(x$title), .dataclass(x), conditionMessage(err))
        stop(paste(strwrap(msg, exdent=4), collapse="\n"))
    })

    tryCatch({
        .get1(class)
    }, error=function(err) {
        msg <- sprintf(
            "failed to load hub resource %s of class %s; reason: %s",
            sQuote(x$title), .dataclass(x), conditionMessage(err))
        stop(paste0(strwrap(msg, exdent=4), collapse="\n"))
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
    cat("# names(): ", names(object)[[1]], "\n", sep="")
    cat(.pprintf1("dataprovider", rsrc[["dataprovider"]]))
    cat(.pprintf1("species", rsrc[["species"]]))
    cat(.pprintf1("rdataclass", rsrc[["rdataclass"]]))
    cat(.pprintf1("title", rsrc[["title"]]))
    cat(.pprintf1("description", rsrc[["description"]]))
    cat(.pprintf1("taxonomyid", rsrc[["taxonomyid"]]))
    cat(.pprintf1("genome", rsrc[["genome"]]))
    srcfields <- c("sourcetype", "sourcelastmodifieddate", "sourcesize")
    x <- .join_resource_columns(object, "input_sources", srcfields)
    cat(.pprintf1("sourcetype", x[["sourcetype"]]))
    cat(.pprintf1("sourceurl", rsrc[["sourceurl"]]))
    cat(.pprintf1("sourcelastmodifieddate", x[["sourcelastmodifieddate"]]))
    cat(.pprintf1("sourcesize", x[["sourcesize"]]))
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
