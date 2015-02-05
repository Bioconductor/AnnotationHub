.id_as_single_string <- function(x)
    paste(sprintf("'%s'", .db_uid(x)), collapse=", ")

.query_as_data.frame <- function(x, query)
{
    tbl <- dbGetQuery(.db_connection(x), query)
    ridx <- match(names(x), tbl$ah_id)
    cidx <- match("ah_id", names(tbl))
    rownames(tbl) <- tbl$ah_id
    tbl[ridx, -cidx, drop=FALSE]
}

.getAHNamesForId <- function(conn, ids){
    query <- sprintf(
        'SELECT resources.id, resources.ah_id
         FROM resources, biocversions
        WHERE biocversion == "%s"
        AND biocversions.resource_id == resources.id',
        biocVersion())
    mtchData <- dbGetQuery(conn, query)
    names(ids) <- mtchData[mtchData[[1]] %in% ids,][[2]]
    ids
}

.uid0 <- function(conn, date)
    ## AnnotationHub() helper
{
    ## TODO: this may be able to be faster by testing whether to do the sorting
    ## in R vs the DB (not terribly slow though)
    ## 1st get the ids that match our version of Bioc
    query <- sprintf(
        'SELECT resources.id
         FROM resources, biocversions
         WHERE biocversion == "%s"
         AND biocversions.resource_id == resources.id',
        biocVersion())
    biocIds <- dbGetQuery(conn, query)[[1]]
    
    ## Then get only ids that are appropriate for the date (date filter)
    query <- sprintf(
        'SELECT id FROM 
         (SELECT * FROM resources where rdatadateadded <= "%s")
         GROUP BY title',
        date)    
    dateFilterIds <- dbGetQuery(conn, query)[[1]]
    
    ## Then get the intersection
    allIds <- intersect(biocIds, dateFilterIds)
    
    ## Then match back to the AHIDs
    .getAHNamesForId(conn, allIds)
}
## test: tail(AnnotationHub:::.db_uid(mh))

.tags <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT tag, resource_id AS id FROM tags
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    dbGetQuery(.db_connection(x), query)
}

.tags_as_collapsed_string <- function(x)
{
    tbl <- .tags(x)
    tags <- sapply(split(tbl$tag, tbl$id), paste, collapse=", ")
    tags <- tags[match(.db_uid(x), names(tags))]
    setNames(tags, .db_uid(x))            # allows for x with no tags
}


## Then do this for rdataclass
.rdataclass <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT rdataclass, resource_id AS id FROM rdatapaths
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    dbGetQuery(.db_connection(x), query)
}
.rdataclass_as_collapsed_string <- function(x)
{
    tbl <- .rdataclass(x)
    rdataclass <- sapply(split(tbl$rdataclass, tbl$id), paste, collapse=", ")
    rdataclass <- rdataclass[match(.db_uid(x), names(rdataclass))]
    setNames(rdataclass, .db_uid(x))         # allows for x with no rdataclass
}

## And again for sourceUrls
.sourceurl <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT sourceurl, resource_id AS id FROM input_sources
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    dbGetQuery(.db_connection(x), query)
}
.sourceurl_as_collapsed_string <- function(x)
{
    tbl <- .sourceurl(x)
    sourceurl <- sapply(split(tbl$sourceurl, tbl$id), paste, collapse=", ")
    sourceurl <- sourceurl[match(.db_uid(x), names(sourceurl))]
    setNames(sourceurl, .db_uid(x))         # allows for x with no sourceurl
}


## And do it again for recipes? - shouldn't this one just come from the initial query? - I think it really should...



.resource_table <- function(x)
{
    query <- sprintf(
        'SELECT %s
         FROM resources, biocversions
         WHERE biocversions.biocversion == %s
         AND resources.id == biocversions.resource_id
         AND resources.id IN (%s)',
        .DB_RESOURCE_FIELDS, biocVersion(), .id_as_single_string(x))
    .query_as_data.frame(x, query)
}

###############################################################################
## need a new helper that will return MOAR fields (from some of the
## other tables) - but (when needed) as compound tables.
## The idea is that this should replace most (if not all) instances of
## .resource_table.  IOW I want this to be the new and more inclusive
## table that will already include things like tags, rdataclass and
## recipes.

## Helper for adding one more vector onto our table with a precise fieldName
.cbindAnother <- function(tbl, vec, headerName){
    if(length(vec) == dim(tbl)[1]){
        tbl <- cbind(tbl, vec, stringsAsFactors=FALSE)
    }
    colnames(tbl)[colnames(tbl) %in% 'vec'] <- headerName
    tbl
}

.compoundResourceTable <- function(x){
    tbl <- .resource_table(x)
    tags <- .tags_as_collapsed_string(x)    
    tbl <- .cbindAnother(tbl, tags, headerName='tags')
    rdataclass <- .rdataclass_as_collapsed_string(x)
    tbl <- .cbindAnother(tbl, rdataclass, headerName='rdataclass')
    sourceurl <- .sourceurl_as_collapsed_string(x)
    tbl <- .cbindAnother(tbl, sourceurl, headerName='sourceurl')
    ## TODO: add recipe to main query.
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

.datapath <- function(x)
{
    query <- sprintf(
        'SELECT resource_id AS id, rdatapath
         FROM rdatapaths WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .query_as_data.frame(x, query)[[1]]
}

## This is used by cache to get the rDataPath ID for a resource
## I think this should say to select 'id' as id to extract the rdatapathID 
## (instead of the resource_id)
.datapathIds <- function(x)
{
#     query <- sprintf(
#         'SELECT resource_id AS id, resource_id
#          FROM rdatapaths WHERE resource_id IN (%s)',
#         .id_as_single_string(x))
#     ## TODO: 'single': sounds incorrect...
#     .query_as_data.frame(x, query)[[1]]    
    query <- sprintf(
        'SELECT DISTINCT id
         FROM rdatapaths WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    ## TODO: 'single': sounds incorrect...
    dbGetQuery(.db_connection(x), query)[[1]]    
}

## 
.dataclass <- function(x)
{
    query <- sprintf(
        'SELECT DISTINCT r.ah_id AS ah_id, rdp.rdataclass
         FROM rdatapaths AS rdp, resources AS r WHERE
         r.id = rdp.resource_id
         AND rdp.resource_id IN (%s)',
        .id_as_single_string(x))
    .query_as_data.frame(x, query)[[1]]
}

## mcols
.mcols <- function(x){
    DataFrame(.compoundResourceTable(x))

    ## TODO: this is not enough, I need to move the addition of tags back to .resource_table (OR maybe make a new function resource_table) to consolidate attachment of thigns like tags etc.
}
## mcols method
setMethod("mcols", "AnnotationHub", function(x){ .mcols(x)} )


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

