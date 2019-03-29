### =========================================================================
### db helpers for Hub objects
### -------------------------------------------------------------------------
###

.db_open <- function(path) {
    tryCatch({
        conn <- dbFileConnect(path)
    }, error=function(err) {
        stop("failed to connect to local data base",
             "\n  database: ", sQuote(path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
    conn
}

.db_close <- function(conn) {
    if (!is.null(conn))
        if (RSQLite::dbIsValid(conn))
            dbDisconnect(conn)
    invisible(conn)
}

.db_query <- function(path, query) {
    if (is.character(path)) {
        path <- .db_open(path)
        on.exit(.db_close(path))
    }
    dbGetQuery(path, query)
}

.db_uid0 <- function(path, .date){
    tryCatch({
        uid <- .uid0(path, .date)
        sort(uid)
    }, error=function(err) {
        stop("failed to connect to local data base",
             "\n  database: ", sQuote(path),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
}


setMethod("dbconn", "Hub",
    function(x) .db_open(dbfile(x))
)

setMethod("dbfile", "Hub",
    function(x) x@.db_path
)

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

.db_create_index <- function(x) {

    bfc <- .get_cache(x)
    index_name <- paste0(tolower(as.character(class(x))),
                                ".index.rds")
    res <- bfcquery(bfc, index_name,
                    field="rname", exact=TRUE)
    cnt <- bfccount(res)
    rid <- res %>% collect(Inf) %>% `[[`("rid")

    if (cnt > 1){
        stop("Corrupt Cache: index file",
             "\n  See vignette section on corrupt cache",
             "\n  cache: ", bfccache(bfc),
             "\n  filename: ", index_name,
             call.=FALSE)
    } else {

        if (cnt == 1){
            index_path <- bfcpath(bfc, rids=rid)
            if (file.exists(index_path)) {
                if (file.mtime(index_path) > file.mtime(dbfile(x)) &&
                    length(x) == length(readRDS(index_path)))
                    return(index_path)
                }
            }
        }

        tryCatch({
            tbl <- .resource_table(x)
            tbl <- setNames(do.call("paste", c(tbl, sep="\r")), rownames(tbl))
            index_path <- ifelse(cnt == 0,
                                 bfcnew(bfc, rname=index_name, ext="_hub_index.rds"),
                                 bfcpath(bfc, rids=rid))
            saveRDS(tbl, unname(index_path))
        }, error=function(err) {
            stop("failed to create index",
                 "\n  hubCache(): ", hubCache(x),
                 "\n  reason: ", conditionMessage(err))
        })
    unname(index_path)
}

.db_index_file <- function(x){
    bfc <- .get_cache(x)
    index_name <- paste0(tolower(as.character(class(x))),
                                ".index.rds")
    res <- bfcquery(bfc, index_name,
                    field="rname", exact=TRUE)
    cnt <- bfccount(res)
    rid <- res %>% collect(Inf) %>% `[[`("rid")
    if (cnt != 1){

        msg <- switch(as.character(cnt),
                      "0"=
                      paste0("Invalid Cache: index file",
                             "\n  Missing entry in cache for: ", index_name,
                             "\n  Consider rerunning with 'localHub=FALSE'"),
                      paste0("Corrupt Cache: index file",
                             "\n  See vignette section on corrupt cache",
                             "\n  cache: ", bfccache(bfc),
                             "\n  filename: ", index_name
                             ))

         stop(msg, call.=FALSE)
    } else {
        unname(bfcpath(bfc, rids=rid))
    }
}

.db_index_load <- function(x)
    readRDS(.db_index_file(x))[names(x)]

.db_index <- function(x) slot(x, ".db_index")
`.db_index<-` <- function(x, ..., value)
{
    if (length(value) > 1L)
        stop("'value' must be length 1")
    if (!is(value, "character"))
        stop("'value' must be a character")
    slot(x, ".db_index") <- value
    x
}
