### =========================================================================
### AnnotationHubResource objects
### -------------------------------------------------------------------------
###

setClass("AnnotationHubResource", representation(hub="Hub"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor 
###

### Objects are created when ...


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors 
###

setMethod("hubCache", "AnnotationHubResource",
    function(x) hubCache(x@hub)
)

setMethod("hubUrl", "AnnotationHubResource",
    function(x) hubUrl(x@hub) 
)

setMethod("getHub", "AnnotationHubResource",
    function(x) x@hub
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###  Show 
###

setMethod("show", "AnnotationHubResource",
    function(object)
{
    cat("class:", class(object), "\n")
})

setGeneric(".get1", function(x, ...) {
    stopifnot(is(x, "AnnotationHubResource"), length(x) == 1L)
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

## FaFile

setClass("FaFileResource", contains="AnnotationHubResource")

setMethod(".get1", "FaFileResource",
    function(x, ...)
{
    .require("Rsamtools")
    fa <- cache(x@hub)
    Rsamtools::FaFile(file=fa[1],index=fa[2])
})

## Rda

setClass("RdaResource", contains="AnnotationHubResource")

setMethod(".get1", "RdaResource",
    function(x, ...)
{
    get(load(cache(x@hub)))
})

setClass("data.frameResource", contains="RdaResource")

setClass("GRangesResource", contains="RdaResource")

setMethod(".get1", "GRangesResource",
    function(x, ...)
{
    .require("GenomicRanges")
    gr <- callNextMethod(x, ...)
    .tidyGRanges(x, gr)
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
    .require("rtracklayer")
    .require("GenomeInfoDb")
    chain <- cache(x@hub)
    tf <- .gunzip(chain, tempfile())
    tf <- rtracklayer::import.chain(tf)
    tf[GenomeInfoDb::sortSeqlevels(names(tf))]
})

setClass("TwoBitFileResource", contains="AnnotationHubResource")

setMethod(".get1", "TwoBitFileResource",
    function(x, ...)      
{
    .require("rtracklayer")
    bit <- cache(x@hub)
    rtracklayer::TwoBitFile(bit)
})

setClass("GTFFileResource", contains="AnnotationHubResource")

setMethod(".get1", "GTFFileResource",
    function(x, ...)
{
    message("Importing File into R ..")
    .require("rtracklayer")
    .require("GenomeInfoDb")
    yy <- x@hub
    gtf <- rtracklayer::import(cache(yy), format="gtf", genome=yy$genome, ...)
    .tidyGRanges(x, gtf)
})

setClass("GFF3FileResource", contains="AnnotationHubResource")

setMethod(".get1", "GFF3FileResource",
    function(x, ...)
{
    .require("rtracklayer")
    yy <- x@hub
    rtracklayer::import(cache(yy), format="GFF", ...)
})

setClass("BigWigFileResource", contains="AnnotationHubResource")

setMethod(".get1", "BigWigFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    er <- cache(x@hub)
    rtracklayer::BigWigFile(er)  
})

setClass("dbSNPVCFFileResource", contains="AnnotationHubResource")

setMethod(".get1", "dbSNPVCFFileResource",
    function(x, ...)      
{
    .require("VariantAnnotation")
    er <- cache(x@hub)
    VariantAnnotation::VcfFile(file=er[1],index=er[2])      
})
## SQLiteFile

setClass("SQLiteFileResource", contains="AnnotationHubResource") 

setMethod(".get1", "SQLiteFileResource",
    function(x, ...)
{
    AnnotationDbi::loadDb(cache(x@hub))
})

## GRASP2 SQLiteFile

setClass("GRASPResource", contains="SQLiteFileResource")

setMethod(".get1", "GRASPResource",
    function(x, ...)
{
    RSQLite::dbConnect(RSQLite::SQLite(), cache(x@hub),
        flags=RSQLite::SQLITE_RO)
})

setClass("ZipResource", contains="AnnotationHubResource")

setMethod(".get1", "ZipResource",
    function(x, filenames, ...)
{
    zip <- cache(x@hub)
    for (fl in filenames)
        unzip(zip, fl, exdir=tempdir())
    file.path(tempdir(), filenames)
})

setClass("ChEAResource", contains="ZipResource")

setMethod(".get1", "ChEAResource",
    function(x, ...)
{
    fl <- callNextMethod(x, filenames="chea-background.csv")
    read.csv(fl, header=FALSE, stringsAsFactors=FALSE, 
        col.names=c("s.no","TranscriptionFactor", "TranscriptionFactor-PubmedID", 
        "TranscriptionFactorTarget", "PubmedID", "Experiment", "CellType",
        "Species","DateAdded"))
}) 

setClass("BioPaxResource", contains="RdaResource")

setMethod(".get1", "BioPaxResource",
    function(x, ...)
{
    .require("rBiopaxParser")
    callNextMethod(x, ...)
})
 
setClass("PazarResource", contains="AnnotationHubResource")

setMethod(".get1", "PazarResource",
    function(x, ...)
{
    .require("GenomicRanges")
    er <- cache(x@hub)
    colClasses <-
        setNames(c(rep("character", 6), rep("integer", 2),
                   rep("factor", 2), "character", "NULL"),
                 c("PazarTFID","EnsemblTFAccession", "TFName",
                   "PazarGeneID", "EnsemblGeneAccession", "Chr", "GeneStart",
                   "GeneEnd", "Species", "ProjectName","PMID",
                   "AnalysisMethod"))
    dat <- read.delim(er, header=FALSE, col.names=names(colClasses),
                      na.strings="-", colClasses=colClasses)
    if (!anyNA(dat[["GeneStart"]])) {
        dat <- GenomicRanges::makeGRangesFromDataFrame(dat,
                                                       keep.extra.columns=TRUE)
        dat <- .tidyGRanges(x, dat)
    }
    dat
})
 

setClass("CSVtoGrangesResource", contains="AnnotationHubResource")

setMethod(".get1", "CSVtoGrangesResource",
   function(x, ...)
{
    .require("GenomicRanges")
    yy <- x@hub
    dat <- read.csv(cache(yy), header=TRUE, stringsAsFactors=FALSE)
    dat <- dat[,!(names(dat) %in% "width")]
    gr <- GenomicRanges::makeGRangesFromDataFrame(dat, keep.extra.columns=TRUE)
    .tidyGRanges(x, gr)
})

setClass("ExpressionSetResource", contains="RdaResource")

setMethod(".get1", "ExpressionSetResource",
    function(x, ...)
{
    .require("Biobase")
    callNextMethod(x, ...)
})
