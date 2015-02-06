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
    .date <- max(.possibleDates(conn = .db_connection))    
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
    ## Compare the highest online assigned unique ID to database id
    url <- paste0(hubOption('URL'), '/metadata/highest_id')
    tryCatch({
        latestOnlineID <- as.integer(content(GET(url)))
        if(is.na(latestOnlineID)){
            stop(wmsg("Back end is not currently displaying the highest ID"))
        }
        sql <- "SELECT max(id) FROM resources"
        latestOnlineID > dbGetQuery(con, sql)[[1]]
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

.db_connection <- function(x) slot(x, ".db_connection")

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
           snapshotDate <- snapshotDate(ah)
    }
    ## Changing the date is two step process.
    ## 1st we update the x@.db_uid
    conn <- .db_connection(x)
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


setMethod("snapshotDate", "AnnotationHub", function(x){.snapshotDate(x)} )

setReplaceMethod("snapshotDate", "AnnotationHub", 
                 function(x, value){.replaceSnapshotDate(x,value)})


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

## $, query / subset

setMethod("$", "AnnotationHub",
    function(x, name)
{
    switch(name,
     "tags"=unname(.collapse_as_string(x,.tags,'tag')),
     "rdataclass"=unname(.collapse_as_string(x,.rdataclass,'rdataclass')),
     "sourceurl"=unname(.collapse_as_string(x,.sourceurl,'sourceurl')),
     "recipe"=unname(.collapse_as_string(x,.recipe,'recipe')),      
     .resource_column(x, name))    ## try to get it from main resources table
})

.DollarNames.AnnotationHub <-
    function(x, pattern="")
{
    values <- c(.resource_columns(), "tags", "rdataclass",
                "sourceurl", "recipe")
    grep(pattern, values, value=TRUE)
}

setGeneric("query", function(x, pattern, ...) standardGeneric("query"),
    signature="x")

setMethod("query", "AnnotationHub",
    function(x, pattern, ...)
{
    tbl <- .compoundResourceTable(x)
    idx0 <- Reduce(`|`,
                   Map(grepl, x=tbl, MoreArgs=list(pattern=pattern, ...)))

    ## now check the many to one values:
    tags <- .tags(x)
    idx1 <- rownames(tbl) %in% unique(tags$id[grepl(pattern, tags$tag)])

    rdataclass <- .rdataclass(x)
    idx2 <- rownames(tbl) %in%
               unique(rdataclass$id[grepl(pattern, rdataclass$rdataclass)])

    sourceurl <- .sourceurl(x)
    idx3 <- rownames(tbl) %in%
               unique(sourceurl$id[grepl(pattern, sourceurl$sourceurl)])

    recipe <- .recipe(x)
    idx4 <- rownames(tbl) %in%
               unique(recipe$id[grepl(pattern, recipe$recipe)])
        
    x[idx0 | idx1 | idx2 | idx3 | idx4]
})

## Test: ahs <- query(ah, 'ChainFile')

setMethod("subset", "AnnotationHub",
    function(x, resource_table, tags, ...)
{
    i <- S4Vectors:::evalqForSubset(resource_table, .resource_table(x), ...)
    tbl <- .tags(x)
    j <- as.integer(.db_uid(x)) %in% 
            tbl$id[S4Vectors:::evalqForSubset(tags, tbl)]
    x[i & j]
})

## data

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

## show
.show <-
    function(x, ..., n=6)
{ 
    x0 <- x[head(seq_along(.db_uid(x)), n)]  ## This line is the problem?
    ## (it results in an empty uid slot)
    cat("display()ing ", length(x0), " of ", length(x), " records on 6 mcols()",
        "\n", sep="")
    tbl <- .compoundResourceTable(x0)
    ## tags <- .collapse_as_string(x0,FUN=.tags,fieldName='tag')
    ## df <- cbind(tbl, tags, stringsAsFactors=FALSE)
    df = tbl
    if (length(x0) != length(x)) {
        df <- rbind(df, "...")
        rownames(df) <- c(rownames(df)[-nrow(df)], "...")
    }
    print(df)
}

setMethod(show, "AnnotationHub", function(object) {
    cat("class:", class(object), "\n")
    cat("hub:", .hub(object), "\n")
    cat("cache:", .cache(object), "\n")
    .show(object, n=3)
})


## helpers used in the constructor as well as elsehwere

.possibleDates <- function(conn){
    query <- 'SELECT DISTINCT rdatadateadded FROM resources'
    dbGetQuery(conn, query)[[1]]
}



## methods for dbconn and dbfile
.dbconn <- function(x){
    con <- x@.db_connection
    ## CANNOT name a connection object. :(
    con
}
setMethod("dbconn", "AnnotationHub", function(x){.dbconn(x)})

.dbfile <- function(x){
    file <- x@.db_connection@dbname
    ## So (for now) I am not using names here either)
    file
}
setMethod("dbfile", "AnnotationHub", function(x){.dbfile(x)})
