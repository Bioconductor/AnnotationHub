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
        'SELECT tag, resource_id AS id FROM tags
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
    DataFrame(.resource_table(x))
}
## mcols method
setMethod("mcols", "AnnotationHub", function(x){ .mcols(x)} )
