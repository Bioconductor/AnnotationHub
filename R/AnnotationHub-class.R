### =========================================================================
### AnnotationHub objects
### -------------------------------------------------------------------------
###

setClass("AnnotationHub", contains="Hub")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor 
###

## Add code to check : https://annotationhub.bioconductor.org/metadata/highest_id
## And compare to the highest ID locally (to see if we have the latest DB)
## And if not, delete the DB so it will be re-downloaded...
AnnotationHub <-
    function(..., hub=hubOption("URL"), cache=hubOption("CACHE"))
{
    .db_path <- .cache_create(cache)
    db_env <- new.env(parent=emptyenv())
    .db_connection <- .db_get(.db_path, hub)
    db_env[["db_connection"]] <- .db_connection
    db_env[["db_path"]] <- dbfile(db_env[["db_connection"]])
    .date <- max(.possibleDates(.db_connection))    
    .db_uid <- .db_uid0(.db_connection, .date, .db_path)
    hub <- new("AnnotationHub", cache=cache, hub=hub, date=.date, 
               .db_env=db_env, .db_uid=.db_uid)
    message("snapshotDate(): ", snapshotDate(hub))
    .db_index <- .db_index_create(hub)
    initialize(hub, .db_index=.db_index)
}
