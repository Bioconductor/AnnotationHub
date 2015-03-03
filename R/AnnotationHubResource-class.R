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

## trace(AnnotationHub:::.get1, tracer=browser, signature ="ChainFileResource")
setMethod(".get1", "ChainFileResource",
    function(x, ...)
{
    chain <- cache(.hub(x))
    tf <- .gunzip(chain, tempfile())
    tf <- rtracklayer::import.chain(tf)
    tf[GenomeInfoDb::sortSeqlevels(names(tf))]
})

setClass("TwoBitFileResource", contains="AnnotationHubResource")

setMethod(".get1", "TwoBitFileResource",
    function(x, ...)      
{
    bit <- cache(.hub(x))
    rtracklayer::TwoBitFile(bit)
})

setClass("EpigenomeRoadmapFileResource", contains="AnnotationHubResource")

setMethod(".get1", "EpigenomeRoadmapFileResource",
    function(x, ...)      
{
    er <- cache(.hub(x))
    rtracklayer::import(er, format="bed", 
        extraCols=c(signalValue="numeric", pValue="numeric", qValue="numeric", 
        peak="numeric"))
})

setClass("dbSNPVCFFileResource", contains="AnnotationHubResource")

setMethod(".get1", "dbSNPVCFFileResource",
    function(x, ...)      
{
    .require("VariantAnnotation")
    er <- cache(.hub(x))
    VariantAnnotation::VcfFile(file=er[1],index=er[2])      
})
## SQLiteFile

setClass("SQLiteFileResource", contains="AnnotationHubResource") 

setMethod(".get1", "SQLiteFileResource",
    function(x, ...)
{
    AnnotationDbi::loadDb(cache(.hub(x)))
})

## GRASP2 SQLiteFile

setClass("GRASPResource", contains="SQLiteFileResource")

setMethod(".get1", "GRASPResource",
    function(x, ...)
{
    RSQLite::dbConnect(RSQLite::SQLite(), cache(.hub(x)),
        flags=RSQLite::SQLITE_RO)
})

setClass("ZipResource", contains="AnnotationHubResource")

setMethod(".get1", "ZipResource",
    function(x, filenames, ...)
{
    zip <- cache(.hub(x))
    for (fl in filenames)
        unzip(zip, fl, exdir=tempdir())
    file.path(tempdir(), filenames)
})

setClass("ChEAResource", contains="ZipResource")

setMethod(".get1", "ChEAResource",
    function(x, ...)
{
    fl <- callNextMethod(x, filenames="chea-background.csv")
    read.csv(fl, header=FALSE, stringsAsFactors=FALSE)
}) 

setClass("BioPaxResource", contains="AnnotationHubResource")

setMethod(".get1", "BioPaxResource",
    function(x, ...)
{
    .require("rBiopaxParser")
    er <- cache(.hub(x))
    rBiopaxParser::readBiopax(er)
})
 
setClass("PazarResource", contains="AnnotationHubResource")

setMethod(".get1", "PazarResource",
    function(x, ...)
{
    er <- cache(.hub(x))
    read.delim(er, header=FALSE, stringsAsFactors=FALSE)

})
 
 
