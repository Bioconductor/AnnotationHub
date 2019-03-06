### =========================================================================
### Hub objects
### -------------------------------------------------------------------------
###

setClass("Hub",
    representation("VIRTUAL",
        hub="character",       ## equivalent to cache URL
        cache="character",     ## equivalent to cache CACHE 
        ## FIXME: why was @date defined as data type 'character'
        date="character",
        .db_path="character", 
        .db_index="character",
        .db_uid="integer"
    )
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor for subclasses
###

.Hub <- function(.class, url, cache, proxy, localHub=FALSE, ...) {
 
    # create or use cache location
    # download hub sqlite file and add/update to BiocFileCache for tracking
    db_path <- .create_cache(.class, url, cache, proxy, localHub)

    # find snapshot date
    if (!localHub){
        tryCatch({
            db_date <- .restrictDateByVersion(db_path)
        }, error=function(err) {
            stop("failed to connect",
                 "\n  reason: ", conditionMessage(err),
                 "\n  Consider rerunning with 'localHub=TRUE'")
        })
    } else {
        dates <-.possibleDates(db_path)
        db_date <- dates[length(dates)]
    }
    
    db_uid <- .db_uid0(db_path, db_date)
    hub <- new(.class, cache=cache, hub=url, date=db_date, 
               .db_path=db_path, .db_uid=db_uid, ...)
    
    message("snapshotDate(): ", snapshotDate(hub))
    
    if (!localHub){
        index <- .db_create_index(hub)
        .db_index(hub) <- index 
    } else{
        
        index <- .db_index_file(hub)
        .db_index(hub) <- index
        hub <- .subsethub(hub)
    }    
    hub
}


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

.subsethub <- function(hub){
    myX <- .named_cache_path(hub)
    locFiles <- dir(hubCache(hub))
    locFiles <- locFiles[!(endsWith(locFiles, ".sqlite") |
                           endsWith(locFiles, ".sqlite3")|
                           endsWith(locFiles, "_index.rds"))]
    resource_num <- vapply(locFiles,
                           function(x){ vl <- strsplit(x, "_");
                                        vl[[1]][length(vl[[1]])] },
                           character(1))
    dx = which(as.character(myX) %in% resource_num)
    files = names(myX[dx])
    hub[files]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("hubCache", "Hub",
    function(x) x@cache
)

setMethod("hubUrl", "Hub",
    function(x) x@hub 
)

setMethod("hubDate", "Hub",
    function(x) x@date 
)

setMethod("package", "Hub",
    function(x) character() 
)

setMethod("snapshotDate", "Hub", function(x) x@date)

setReplaceMethod("snapshotDate", "Hub", 
    function(x, value) 
{
    if (length(value) != 1L)
        stop("'value' must be a single date or character string")
    tryCatch({
       fmt_value <- as.POSIXlt(value)
    }, error=function(err) {
        stop("'value' must be a single date or character string")
    })

    ## 'value' must be < BiocManager::version() release date
    restrict <- .restrictDateByVersion(dbfile(x))
    dates <- .possibleDates(dbfile(x))
    valid_range <- range(dates[as.POSIXlt(dates) <= as.POSIXlt(restrict)])
    if (as.POSIXlt(value) > max(valid_range) ||
        as.POSIXlt(value) < min(valid_range))
        stop("'value' must be in the range of possibleDates(x)")
 
    new(class(x), cache=hubCache(x), hub=hubUrl(x), 
        date=as.character(value), 
        .db_path=x@.db_path,
        .db_index=x@.db_index,
        .db_uid=.db_uid0(x@.db_path, value))
})


setMethod("length", "Hub",
    function(x) length(.db_uid(x))
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor-like methods 
###

setMethod("mcols", "Hub", function(x) DataFrame(.resource_table(x)))

setMethod("names", "Hub",
    function(x)
{
    as.character(names(.db_uid(x)))
})

setMethod("fileName", signature=(object="Hub"),
    function(object, ...)
{
    cachepath <- .named_cache_path(object)
    rnames <- paste(names(cachepath), cachepath, sep=" : ")
    bfc <- .get_cache(object)
    dx <- rep(NA_character_, length(rnames))
    fnd <- which(rnames %in% bfcinfo(bfc)$rname) 
    dx[fnd] <- unname(bfcrpath(bfc, rnames=rnames[fnd]))
    names(dx) <- names(cachepath)
    dx
    
})



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting 
###


setMethod("[", c("Hub", "numeric", "missing"),
    function(x, i, j, ..., drop=TRUE) 
{
    idx <- na.omit(match(i, seq_along(.db_uid(x)))) 
    ## seq_along creates a special problem for show()...
    .db_uid(x) <- .db_uid(x)[idx]
    x
})

setMethod("[", c("Hub", "logical", "missing"),
    function(x, i, j, ..., drop=TRUE)
{
    i[is.na(i)] <- FALSE
    .db_uid(x) <- .db_uid(x)[i]
    x
})

setMethod("[", c("Hub", "character", "missing"),
    function(x, i, j, ..., drop=TRUE) 
{
    idx <- na.omit(match(i, names(.db_uid(x))))
    .db_uid(x) <- .db_uid(x)[idx]
    x
})

setReplaceMethod("[", c("Hub", "numeric", "missing", "Hub"),
    function(x, i, j, ..., value)
{
    .db_uid(x)[i] <- .db_uid(value)
    x
})

setReplaceMethod("[", c("Hub", "logical", "missing", "Hub"),
    function(x, i, j, ..., value)
{
    .db_uid(x)[i] <- .db_uid(value)
    x
})

setReplaceMethod("[", 
    c("Hub", "character", "missing", "Hub"),
    function(x, i, j, ..., value)
{
    idx <- match(i, names(.db_uid(x)))
    isNA <- is.na(idx)
    .db_uid(x)[idx[!isNA]] <- .db_uid(value)[!isNA]
    x
})


setMethod("[[", c("Hub", "numeric", "missing"),
    function(x, i, j, ..., force=FALSE, verbose=TRUE)
{
    .Hub_get1(x[i], force=force, verbose=verbose)
})

setMethod("[[", c("Hub", "character", "missing"),
    function(x, i, j, ..., force=FALSE, verbose=TRUE)
{
    if (length(i) != 1L)
        stop("'i' must be length 1")
    idx <- match(i, names(.db_uid(x)))
    if (is.na(idx))
        stop(recordStatus(x, i)$status)
    .Hub_get1(x[idx], force=force, verbose=verbose)
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### cache methods
###

setMethod("cache", "Hub",
    function(x, ..., proxy, max.downloads, force=FALSE, verbose=FALSE)
        .cache_internal(x, 
                        proxy=proxy, max.downloads=max.downloads,
                        force=force, verbose=verbose)
)


# removes all files except for databases
setReplaceMethod("cache", "Hub",
    function(x, ..., value)
{
    stopifnot(identical(value, NULL))
    bfc <- .get_cache(x)
    rids <- BiocFileCache:::.get_all_rids(bfc)
    keep <- c(bfcrid(bfcquery(bfc, "annotationhub.sqlite3", fields="rname", exact=TRUE)),
              bfcrid(bfcquery(bfc, "annotationhub.index.rds", fields="rname", exact=TRUE)))
    rmid <- setdiff(rids, keep)
    if (length(rmid) > 0){
        bfcremove(bfc, rids=rmid)
    }    
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### $, query() and subset() methods
###

setMethod("$", "Hub",
    function(x, name)
{
    switch(name,
     "tags"=unname(.collapse_as_string(x, .tags)),
     "rdataclass"=unname(.collapse_as_string(x, .rdataclass)),
     "rdatapath"=unname(.collapse_as_string(x, .rdatapath)),
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
### Show 
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

.show <- function(object)
{
    if (!length(object))
        return(NULL)

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

.show1 <- function(object)
{
    rsrc <- .resource_table(object)
    size <- .collapse_as_string(object, .sourcesize)   
    date <- .collapse_as_string(object, .sourcelastmodifieddate)
 
    cat("# names(): ", names(object)[[1]], "\n", sep="")
    if (length(package(object)) > 0L)
        cat("# package(): ", package(object)[[1]], "\n", sep="")
    cat(.pprintf1("dataprovider", rsrc[["dataprovider"]]))
    cat(.pprintf1("species", rsrc[["species"]]))
    cat(.pprintf1("rdataclass", rsrc[["rdataclass"]]))
    cat(.pprintf1("rdatadateadded", rsrc[["rdatadateadded"]]))
    cat(.pprintf1("title", rsrc[["title"]]))
    cat(.pprintf1("description", rsrc[["description"]]))
    cat(.pprintf1("taxonomyid", rsrc[["taxonomyid"]]))
    cat(.pprintf1("genome", rsrc[["genome"]]))
    cat(.pprintf1("sourcetype", rsrc[["sourcetype"]]))
    cat(.pprintf1("sourceurl", rsrc[["sourceurl"]]))
    cat(.pprintf1("sourcesize", size))
    cat(.pprintf0("# $tags: %s", rsrc[["tags"]]), "\n")
    cat(.pprintf0("# retrieve record with 'object[[\"%s\"]]'",
                  names(object)[[1]]), "\n")
}

setMethod("show", "Hub", function(object) 
{
    if (length(object) == 1)
        .show1(object)
    else
        .show(object)
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
    # speeds up search by making character
    df$tags <- vapply(unname(unclass(md$tags)), base::paste,
                      character(1), collapse=", ")

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


setMethod("recordStatus", "Hub",
    function(hub, record) {
        if (!is.character(record))
            stop("'record' must be a character")
        if (length(record) != 1L)
            stop("'record' must be length 1")

        conn <- dbconn(hub)
        query <- paste0("SELECT status FROM statuses WHERE id IN ",
                        "(SELECT status_id FROM resources WHERE ah_id = '",
                        record, "')")
        status=.db_query(conn, query)
        if (nrow(status) == 0L)
            status <- "record not found in database"
        data.frame(record=record, status=status)
})

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

    db_uid <- unlist(lapply(unname(args), .db_uid))
    if (length(db_uid) == 0 && is.null(names(db_uid)))
        names(db_uid) <- character()
    udb_uid <- unique(db_uid)
    idx <- match(udb_uid, db_uid)
    .db_uid <- setNames(udb_uid, names(db_uid)[idx])
    initialize(args[[1]], .db_uid=.db_uid)
})
