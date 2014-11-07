setClass("AnnotationHubResource", representation(hub="AnnotationHub"))

setMethod("show", "AnnotationHubResource",
    function(object)
{
    cat("class:", class(object), "\n")
})

setGeneric(".get1", function(x, ...) {
    stopifnot(is(x, "AnnotationHubResource") && length(x) == 1L)
    standardGeneric(".get1")
})

setMethod(".get1", "AnnotationHubResource",
    function(x, ...)
{
    msg <- sprintf("no '.get1' method defined for object
        of class %s, consider defining your own.",
        sQuote(class(x)))
    stop(paste(strwrap(msg), collapse="\n"))
})

##
## implementations
##

.require <-
    function(pkg)
{
    if (length(grep(sprintf("package:%s", pkg), search())) != 0L)
        return()
    message("require(", dQuote(pkg), ")")
    tryCatch({
        suppressPackageStartupMessages({
            require(pkg, quietly=TRUE, character.only=TRUE)
        })
    }, error=function(err) {
        msg <- sprintf("require(%s) failed: %s", dQuote(pkg),
                       conditionMessage(err))
        stop(msg)
    })
}

## FaFile

setClass("FaFileResource", contains="AnnotationHubResource")

setMethod(".get1", "FaFileResource",
    function(x, ...)
{
    fa <- cache(.hub(x))
    .require("Rsamtools")
    Rsamtools::FaFile(file=fa[1],index=fa[2])
})

## Rda

setClass("RdaResource", contains="AnnotationHubResource")

setMethod(".get1", "RdaResource",
    function(x, ...)
{
    get(load(cache(.hub(x))))
})

setClass("data.frameResource", contains="RdaResource")

setClass("GRangesResource", contains="RdaResource")

setMethod(".get1", "GRangesResource",
    function(x, ...)
{
    .require("GenomicRanges")
    callNextMethod(x, ...)
})

setClass("VCFResource", contains="RdaResource")

setMethod(".get1", "VCFResource",
    function(x, ...)
{
    .require("VariantAnnotation")
    callNextMethod(x, ...)
})

## UCSC chain file
setClass("ChainFileResource", contains="AnnotationHubResource")
## TODO: chain files won't work till you tap into the sourceurl in input_sources
## and the location_prefix from location_prefixes in the metadata.

## trace(".get1", browser, signature ="AnnotationHubResource")
setMethod(".get1", "ChainFileResource",
    function(x, ...)
{
#     conn <- .db_connection(x)
#     id <- .db_uid(x)[[1]]
# #    SQL <- "SELECT r.ah_id, i.sourceurl, l.location_prefix FROM resources AS r, 
# #input_sources AS i, location_prefixes AS l WHERE r.location_prefix_id=l.id 
# #AND r.id=i.resource_id  AND r.ah_id = 'AH15100' limit 3;"
#     query <- sprintf('SELECT r.id, i.sourceurl, l.location_prefix 
#                     FROM resources AS r, input_sources AS i, 
#                     location_prefixes AS l 
#                     WHERE r.location_prefix_id=l.id
#                     AND r.id=i.resource_id  
#                     AND r.id = "%s"', id)
#     dbpaths <- dbGetQuery(conn, query)
#     url <- paste0(dbpaths[3],dbpaths[2])
# ## TODO: now make this work better with cache()
# ## might want an alternative to cache for internal use?
# ## 
# #    chain <- cache(url)

        
    chain <- cache(.hub(x))
    .require("rtracklayer")
    rtracklayer::import.chain(chain)
})

setClass("TwoBitFileResource", contains="AnnotationHubResource")

setMethod(".get1", "TwoBitFileResource",
    function(x, ...)      
{
    bit <- cache(.hub(x))
    .require("rtracklayer")
    twobit <- rtracklayer::TwoBitFile(bit)
    rtracklayer::import(twobit)    
})

## SQLiteFile

setClass("SQLiteFileResource", contains="AnnotationHubResource")

setMethod(".get1", "SQLiteFileResource",
    function(x, ...)
{
    .require("AnnotationDbi")
    AnnotationDbi::loadDb(cache(.hub(x)))
})
