.id_as_single_string <- function(x)
    paste(sprintf("'%s'", .db_uid(x)), collapse=", ")

.query_as_data.frame <- function(x, query)
{
    tbl <- .db_query(dbfile(x), query)
    ridx <- match(names(x), tbl$ah_id)
    cidx <- match("ah_id", names(tbl)) 
    rownames(tbl) <- tbl$ah_id
    tbl[ridx, -cidx, drop=FALSE]
}

## FIXME: This is redundant with the filter criteria in .uid0() and
## does not reuse ids already discovered (i.e., it's an independent
## query that may not match up and has no checks to confirm).
.names_for_ids <- function(conn, ids){
    query <- sprintf(
        'SELECT resources.id, resources.ah_id
         FROM resources, biocversions
         WHERE biocversion == "%s"
         AND biocversions.resource_id == resources.id',
        biocVersion())
    mtchData <- .db_query(conn, query)
    names(ids) <- mtchData[mtchData[[1]] %in% ids,][[2]]
    ids
}

## This function filters the local annotationhub.sqlite metadata db and
## dictates the subset exposed by AnnotationHub().
.uid0 <- function(path, date)
{
    conn <- .db_open(path)
    on.exit(.db_close(conn))

    ## Filter on the following:
    ## - biocversion field <= the current biocVersion
    ## - rdatadateremoved field is NULL
    query <- sprintf(
        'SELECT resources.id
         FROM resources, biocversions
         WHERE biocversions.biocversion <= "%s"
         AND resources.rdatadateremoved IS NULL
         AND biocversions.resource_id == resources.id',
         biocVersion())
    biocIds <- .db_query(conn, query)[[1]]

    ## make unique 
    allIds = unique(biocIds)
    ## match id to ah_id
    query <- paste0('SELECT ah_id FROM resources ',
                    'WHERE id IN (', paste0(allIds, collapse=","), ')')
    names(allIds) <- .db_query(conn, query)[[1]]
    allIds
}

## helper to retrieve tags
.tags <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT tag, resource_id AS id FROM tags
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .db_query(dbfile(x), query)
}

## helper for extracting rdataclass
.rdataclass <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT rdataclass, resource_id AS id FROM rdatapaths
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .db_query(dbfile(x), query)
}

## helper for extracting sourceUrls
.sourceurl <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT sourceurl, resource_id AS id FROM input_sources
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .db_query(dbfile(x), query)
}

##  helper for extracting sourcetype
.sourcetype <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT sourcetype, resource_id AS id FROM input_sources
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .db_query(dbfile(x), query)
}

.sourcesize <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT sourcesize, resource_id AS id FROM input_sources
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .db_query(dbfile(x), query)
}

.sourcelastmodifieddate <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT sourcelastmodifieddate, resource_id AS id FROM input_sources
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .db_query(dbfile(x), query)
}


## Helper to collapse many to one fields (like above) into one space
.collapse_as_string <- function(x, FUN)
{
    uid <- .db_uid(x)
    tbl <- FUN(x)
    lst <- vapply(split(tbl[[1]], tbl[["id"]]), paste0,
                  character(1), collapse=", ")
    lst <- lst[match(uid, names(lst))]
    setNames(lst, names(uid))           # allows for x with no tags 
}


## This gets many useful fields together for the end user and puts them into
## a nice square shaped container.
.resource_table <- function(x)
{
    query <- sprintf(
        'SELECT %s FROM resources
         WHERE resources.id IN (%s)',
        .DB_RESOURCE_FIELDS, .id_as_single_string(x))
    tbl <- .query_as_data.frame(x, query)
    tbl[["tags"]] <- .collapse_as_string(x, .tags)
    tbl[["rdataclass"]] <- .collapse_as_string(x, .rdataclass)
    tbl[["sourceurl"]] <- .collapse_as_string(x, .sourceurl)
    tbl[["sourcetype"]] <- .collapse_as_string(x, .sourcetype)
    tbl
}


.resource_columns <- function()
    strsplit(gsub("resources.", "", .DB_RESOURCE_FIELDS), ", ")[[1]]

.resource_column <- function(x, name)
{
    valid <- .resource_columns()
    if (!name %in% valid) {
        msg <- sprintf("%s is not a resource data column", sQuote(name))
        stop(msg)
    }
    query <- sprintf(
        'SELECT ah_id, %s FROM resources WHERE id IN (%s)',
        name, .id_as_single_string(x))
    .query_as_data.frame(x, query)[[1]]
}

## This is used by cache to get the rDataPath ID for a resource
## I think this should say to select 'id' as id to extract the rdatapathID 
## (instead of the resource_id)
.datapathIds <- function(x)
{
    query <- sprintf(
        'SELECT DISTINCT resources.ah_id, rdatapaths.id
         FROM resources, rdatapaths
         WHERE resources.id IN (%s)
         AND resources.id == rdatapaths.resource_id',
        .id_as_single_string(x))
    result <- .db_query(dbfile(x), query)
    setNames(result[[2]], result[[1]])
}

## 
.dataclass <- function(x)
{
    query <- sprintf(
        'SELECT DISTINCT r.ah_id AS ah_id, rdp.dispatchclass
         FROM rdatapaths AS rdp, resources AS r WHERE
         r.id = rdp.resource_id
         AND rdp.resource_id IN (%s)',
        .id_as_single_string(x))
    .query_as_data.frame(x, query)[[1]]
}

## 
.title_data.frame <-
    function(x)
{
    query <- sprintf(
        "SELECT ah_id, title FROM resources
         WHERE resources.id IN (%s)",
        .id_as_single_string(x))
    .query_as_data.frame(x, query)
}

.count_resources <-
    function(x, column, limit=10)
{
    query <- sprintf(
        "SELECT %s FROM resources
         WHERE resources.id IN (%s)
         GROUP BY %s ORDER BY COUNT(%s) DESC LIMIT %d", 
        column, .id_as_single_string(x), column, column, limit)
    .db_query(dbfile(x), query)[[column]]
}

.count_join_resources <-
    function(x, table, column, limit=10)
{
    query <- sprintf(
        "SELECT %s FROM resources, %s
         WHERE resources.id IN (%s) AND %s.resource_id == resources.id
         GROUP BY %s ORDER BY COUNT(%s) DESC LIMIT %d", 
        column, table,
        .id_as_single_string(x), table,
        column, column, limit)
    .db_query(dbfile(x), query)[[column]]
}

## make a function to create a view whenever the DB is updated..
## SQL will look kind of like the one used for go:
## CREATE VIEW go AS
## SELECT _id,go_id,evidence, 'BP' AS 'ontology' FROM go_bp
## UNION
## SELECT _id,go_id,evidence, 'CC' FROM go_cc
## UNION
## SELECT _id,go_id,evidence, 'MF' FROM go_mf;


## SO now we just need to decide on which views we want/need.
## So really we want to 1st refactor the show method (and make hard decisions there)
## And the view we create here should reflect those ideas.


## CREATE VIEW hub AS
## SELECT * FROM resources AS r, rdatapaths AS rdp, input_sources AS ins  WHERE r.id=rdp.resource_id AND r.id=ins.resource_id LIMIT 2;

























## Sonali better date query
## SELECT * FROM resources where rdatadateadded <= "2013-03-19" GROUP BY title ORDER BY rdatadateadded DESC limit 2;


## Example:
## SELECT COUNT(id) AS theCount, `Tag` from `images-tags`
## GROUP BY `Tag`
## ORDER BY theCount DESC
## LIMIT 20


## SELECT id FROM 
## (SELECT * FROM
## (SELECT * FROM resources where rdatadateadded <= "2013-03-19")
## AS res GROUP BY title ORDER BY rdatadateadded DESC limit 1);


## SELECT MAX(id) FROM (SELECT id FROM (select * from resources where ah_id in ('AH523','AH22249')) AS res GROUP BY title) AS res;
## same issue


## Here is an example that actually does get close:
## SELECT max(id) as mid, id, ah_id, title FROM (select * from resources where ah_id in ('AH523','AH22249','AH524','AH22250')) AS res GROUP BY maintainer;

## Basically I would just want it like this:
## SELECT max(id) as mid FROM (select * from resources where ah_id in ('AH523','AH22249','AH524','AH22250')) AS res GROUP BY maintainer;

## And then group by title instead so pretty much like this:
## SELECT max(id) as id FROM (SELECT * FROM resources where rdatadateadded <= "2013-03-19") AS res GROUP BY title;
