### =========================================================================
### db helpers for Hub objects
### -------------------------------------------------------------------------
###

.db_current <- function(path, hub, proxy)
{
    tryCatch({
        url <- paste0(hub, '/metadata/database_timestamp')
        onlineTime <- as.POSIXct(content(GET(url, proxy)))

        con <- .db_get_db(path, hub, proxy)
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


## Helpers to get a fresh metadata DB connection
.db_get_db <- function(path, hub, proxy) {
    ## download or cache
    tryCatch({
        need <- !file.exists(path)
        .hub_resource(.hub_metadata_path(hub), basename(path)[need], 
                      path[need], proxy)
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
            warning("empty 'resources' table; database may be corrupt")
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

.db_get <- function(path, hub, proxy) {
    update <- !file.exists(path)
    if (!update && !file.size(path)) {
        file.remove(path)
        update <- TRUE
    }
    if (!update && !.db_current(path, hub, proxy)) {
        file.remove(path)
        update <- TRUE
    }
    if (update)
        message("updating metadata: ", appendLF=FALSE)
    con <- .db_get_db(path, hub, proxy)
    .db_is_valid(con)
    .db_create_view(con)
    con
}

.db_index_file <- function(x)
    file.path(hubCache(x), "index.rds")

.db_index_load <- function(x)
    readRDS(.db_index_file(x))[names(x)]

.db_uid0 <- function(con, .date, path){
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

.db_create_index <- function(x) {
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

setMethod("dbconn", "Hub", 
    function(x) {
        conn <- slot(x, ".db_env")[["db_connection"]]
        if (!dbIsValid(conn)) {
            conn <- .db_open(dbfile(x))
            slot(x, ".db_env")[["db_connection"]] <- conn
        }
        conn
})

setMethod("dbfile", "Hub", 
    function(x) x@.db_env[["db_path"]]
)
