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
    db_path <- .cache_create(cache)
    db_env <- new.env(parent=emptyenv())
    db_connection <- .db_get(db_path, hub)
    db_env[["db_connection"]] <- db_connection
    db_env[["db_path"]] <- dbfile(db_env[["db_connection"]])
    db_date <- max(.possibleDates(db_connection))    
    db_uid <- dbuid0(db_connection, db_date, db_path)
    hub <- new("AnnotationHub", cache=cache, hub=hub, date=db_date, 
               .db_env=db_env, .db_uid=db_uid)
    message("snapshotDate(): ", snapshotDate(hub))
    index <- dbcreateindex(hub)
    dbindex(hub) <- index 
    hub
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting 
###

.Hub_get1 <-
    function(x)
{
    if (length(x) != 1L)
        stop("'i' must be length 1")

    className <- sprintf("%sResource", .dataclass(x))
    if (is.null(getClassDef(className))) {
        msg <- sprintf("'%s' not available in this version of the
            package; use biocLite() to update?",
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
        .get1(class)
    }, error=function(err) {
        stop("failed to load resource",
             "\n  name: ", names(x),
             "\n  title: ", x$title,
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
}

setMethod("[[", c("AnnotationHub", "numeric", "missing"),
    function(x, i, j, ...)
{
    .Hub_get1(x[i])
})

setMethod("[[", c("AnnotationHub", "character", "missing"),
    function(x, i, j, ...)
{
    if (length(i) != 1L)
        stop("'i' must be length 1")
    idx <- match(i, names(dbuid(x)))
    if (is.na(idx))
        stop("unknown key ", sQuote(i))
    .Hub_get1(x[idx])
})
