## This function filters the local annotationhub.sqlite metadata db and
## defines the subset exposed by AnnotationHub().
.uid0 <- function(path, date, localHub)
{
    conn <- .db_open(path)
    on.exit(.db_close(conn))

    ## General filter:
    ## All AnnotationHub resources (except OrgDbs, see below) are
    ## available from the time they are added -> infinity unless
    ## they are removed from the web or by author request. The
    ## snapshot date can be changed by the user. We want to return records
    ## with no rdatadateremoved and with rdatadateadded <= snapshot.
    ## All OrgDbs are omitted in the first filter and selectively
    ## exposed in the second filter.
    ##   NOTE: biocversions filter distinguishes between release and devel;
    ##   this is not caught by rdatadate added filter because the timestamp
    ##   is updated with each modification and currently someone using
    ##   an old version of Bioconductor will still get the current db
    ##   which will have a timestamp > the date when the old version of
    ##   Bioconductor was valid.
    ##   NOTE: The 'date' variable is the snapshotDate().

    # Ran into an issue comparing BiocVersion once we hit 3.10
    # 3.10 got truncated to 3.1 and missed values
    bioc_value <- .db_query(conn,
                            "SELECT DISTINCT biocversion FROM biocversions")[[1]]
    indx <- package_version(bioc_value) <= BiocManager::version()
    if (sum(indx) != 0) bioc_value <- bioc_value[indx]
    bioc_value <- paste(paste0('"', bioc_value, '"'), collapse=",")

    query1 <- sprintf(
        'SELECT resources.id
         FROM resources, rdatapaths, biocversions
         WHERE resources.rdatadateadded <= "%s"
         AND biocversions.biocversion IN (%s)
         AND resources.rdatadateremoved IS NULL
         AND rdatapaths.rdataclass != "OrgDb"
         AND biocversions.resource_id == resources.id
         AND rdatapaths.resource_id == resources.id',
         date, bioc_value)
    biocIds1 <- .db_query(conn, query1)[[1]]

    ## Add a query to get resources that have been removed
    ## But were present during a given release
    ## There is a chance that if the data was removed
    ## completely from external location that these
    ## ids won't work
    query3 <- sprintf(
        'SELECT resources.id
         FROM resources, rdatapaths, biocversions
         WHERE resources.rdatadateadded <= "%s"
         AND biocversions.biocversion IN (%s)
         AND resources.rdatadateremoved > "%s"
         AND rdatapaths.rdataclass != "OrgDb"
         AND biocversions.resource_id == resources.id
         AND rdatapaths.resource_id == resources.id',
         date, bioc_value, date)
    biocIds3 <- .db_query(conn, query3)[[1]]
    ## OrgDb sqlite files:
    ##
    ## OrgDbs are the single resource designed to expire at the end of a
    ## release cycle. The sqlite files are built before a release, added to the
    ## devel branch then propagate to the new release branch. For the
    ## duration of a release cycle both release and devel share the same
    ## OrgDb packages. Before the next release, new files are built, added
    ## to devel, propagated to release and so on.
    ##
    ## When new sqlite files are added to the hub they are stamped
    ## with the devel version which immediately becomes the new release version.
    ## For this reason, the devel code loads OrgDbs with the release version
    ## e.g.,
    ##   ifelse(isDevel, biocversion - 0.1, biocversion)
    ##
    ## NOTE: Because OrgDbs are valid for a full devel cycle they are
    ##       not filtered by snapshotDate(); the OrgDbs are valid for all

    if(curl::has_internet() || !localHub){
        isDevel <- BiocManager:::isDevel()
        orgdb_release_version <-
            if (getAnnotationHubOption("TESTING") || !isDevel) {
                BiocManager::version()
            } else {
                BiocManager:::.version_bioc("release")
            }

        query2 <- sprintf(
            'SELECT resources.id
            FROM resources, biocversions, rdatapaths
            WHERE biocversions.biocversion == "%s"
            AND rdatapaths.rdataclass == "OrgDb"
            AND resources.rdatadateremoved IS NULL
            AND biocversions.resource_id == resources.id
            AND rdatapaths.resource_id == resources.id',
            orgdb_release_version)
        biocIds2 <- .db_query(conn, query2)[[1]]
    }else{
        query2 <- sprintf(
            'SELECT resources.id
            FROM resources, biocversions, rdatapaths
            WHERE biocversions.biocversion == "%s"
            AND rdatapaths.rdataclass == "OrgDb"
            AND resources.rdatadateremoved IS NULL
            AND biocversions.resource_id == resources.id
            AND rdatapaths.resource_id == resources.id',
            BiocManager::version())
        biocIds2 <- .db_query(conn, query2)[[1]]
    }


    ## make unique and sort
    allIds = sort(unique(c(biocIds1, biocIds2, biocIds3)))
    ## match id to ah_id
    query <- paste0('SELECT ah_id FROM resources ',
                    'WHERE id IN (', paste0(allIds, collapse=","), ')',
                    'ORDER BY id')
    names(allIds) <- .db_query(conn, query)[[1]]
    allIds
}


.resource_table <- function(x)
{
    query <- sprintf(
        'SELECT %s FROM resources
         WHERE resources.id IN (%s)',
        .DB_RESOURCE_FIELDS, .id_as_single_string(x))
    tbl <- .query_as_data.frame(x, query)
    tbl[["tags"]] <- I(.collapse_as_list(x, .tags))
    tbl[["rdataclass"]] <- .collapse_as_string(x, .rdataclass)
    tbl[["rdatapath"]] <- .collapse_as_string(x, .rdatapath)
    tbl[["version_id"]] <- .collapse_as_string(x, .version)
    tbl[["sourceurl"]] <- .collapse_as_string(x, .sourceurl)
    tbl[["sourcetype"]] <- .collapse_as_string(x, .sourcetype)
    tbl
}

## Used in mcols()
.DB_RESOURCE_FIELDS <- paste(sep=".", collapse=", ", "resources",
    c("ah_id", "title", "dataprovider", "species", "taxonomyid", "genome",
      "description", "coordinate_1_based", "maintainer",
      "rdatadateadded", "preparerclass"))

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

.collapse_as_list <- function(x, FUN)
{
    uid <- .db_uid(x)
    tbl <- FUN(x)
    lst <- split(tbl[[1]], tbl$id)
    lst <- lst[match(uid, names(lst))]
    setNames(lst, names(uid))           # allows for x with no tags
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

## helper for extracting rdatapath
.rdatapath <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT rdatapath, resource_id AS id FROM rdatapaths
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .db_query(dbfile(x), query)
}

## helper for extracting version_id
.version <- function(x) {
    query <- sprintf(
        'SELECT DISTINCT version_id, resource_id AS id FROM rdatapaths
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
        'SELECT DISTINCT sourcelastmodifieddate, resource_id AS id
         FROM input_sources
         WHERE resource_id IN (%s)',
        .id_as_single_string(x))
    .db_query(dbfile(x), query)
}

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

.title_data.frame <-
    function(x)
{
    query <- sprintf(
        "SELECT ah_id, title FROM resources
         WHERE resources.id IN (%s)",
        .id_as_single_string(x))
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

.IdsInfo <- function(x)
{
    query <-
        'SELECT DISTINCT resources.ah_id, rdatapaths.id, resources.title, rdatapaths.rdataclass, statuses.status, biocversions.biocversion, resources.rdatadateadded, resources.rdatadateremoved
         FROM resources, rdatapaths, statuses, biocversions
         WHERE resources.id == rdatapaths.resource_id
         AND resources.status_id == statuses.id
         AND biocversions.resource_id == resources.id'
    mat <- .db_query(dbfile(x), query)
    nms <- names(mat)
    nms[which(nms == "id")] = "fetch_id"
    names(mat) <- nms
    mat
}

.getversions <- function(hub, id)
{
    query <- paste0(
        "SELECT DISTINCT resources.ah_id, rdatapaths.version_id, resources.title, resources.description, rdatapaths.rdataclass, resources.species, resources.genome, resources.taxonomyid, input_sources.sourcesize, input_sources.sourceurl, input_sources.sourceversion, input_sources.sourcemd5, input_sources.sourcelastmodifieddate, input_sources.sourcetype, statuses.status, biocversions.biocversion, resources.rdatadateadded, resources.rdatadateremoved FROM resources, rdatapaths, statuses, biocversions, input_sources WHERE resources.id == rdatapaths.resource_id AND resources.status_id == statuses.id AND biocversions.resource_id == resources.id AND input_sources.resource_id == resources.id AND resources.ah_id LIKE '%",id,"%'")
    mat <- AnnotationHub:::.db_query(dbfile(hub), query)
    mat
}
