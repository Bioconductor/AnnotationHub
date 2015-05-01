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

.metadataForAH <- 
    function(x, ...)
{
    stopifnot(length(.hub(x)) == 1)
    list(AnnotationHubName=names(.hub(x)))
}

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

.seqlevelsIsCircular <- 
    function(seqlevs) 
{
    cirInd <- which(seqlevs %in% 
        c("MT", "MtDNA", "dmel_mitochondrion_genome","Mito", "chrM"))
    if(length(cirInd)!=0){
        cirVec <- rep(FALSE, length(seqlevs))
        cirVec[cirInd] <- TRUE
    } else {
        cirVec <- rep(FALSE, length(seqlevs))
    }
    cirVec
}

.tidyGRanges <- 
    function(x, gr, sort=TRUE, guess.circular=TRUE, addGenome=TRUE) 
{
    si <- seqinfo(sortSeqlevels(gr))
    yy <- .hub(x)
    if(sort)
        seqlevels(gr) <- seqlevels(si) 
    if(guess.circular) 
        isCircular(si)  <- .seqlevelsIsCircular(seqlevels(si))
    if(addGenome)
        genome(si) <-yy$genome      
    ## TODO: add seqlengths code
    seqinfo(gr) <- si
    metadata(gr)  <- .metadataForAH(x)
    gr
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

setClass("GTFFileResource", contains="AnnotationHubResource")

setMethod(".get1", "GTFFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    yy <- .hub(x)
    gtf <- rtracklayer::import(cache(yy), format="gtf", genome=yy$genome, ...)
    .tidyGRanges(x, gtf)
})

setClass("BigWigFileResource", contains="AnnotationHubResource")

setMethod(".get1", "BigWigFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    er <- cache(.hub(x))
    rtracklayer::BigWigFile(er)  
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
    er <- cache(.hub(x))
    dat <- read.delim(er, header=FALSE, stringsAsFactors=FALSE,
        col.names=c("PazarTFID","EnsemblTFAccession", "TFName", "PazarGeneID",
        "EnsemblGeneAccession", "Chr", "GeneStart", "GeneEnd", "Species", 
	"ProjectName","PMID", "AnalysisMethod"))
    dat <- dat[, -12]  # collumn contains only NA
    tryCatch({
        dat <- makeGRangesFromDataFrame(dat, keep.extra.columns=TRUE)
        dat <- .tidyGRanges(x, gr) 
    }, error=function(err){
    })
    dat
})
 

setClass("CSVtoGrangesResource", contains="AnnotationHubResource")

setMethod(".get1", "CSVtoGrangesResource",
   function(x, ...)
{
    .require("GenomicRanges")
    yy <- .hub(x)
    dat <- read.csv(cache(yy), header=TRUE, stringsAsFactors=FALSE)
    dat <- dat[,!(names(dat) %in% "width")]
    gr<- makeGRangesFromDataFrame(dat, keep.extra.columns=TRUE)
    .tidyGRanges(x, gr)
})

setClass("ExpressionSetResource", contains="RdaResource")

setMethod(".get1", "ExpressionSetResource",
    function(x, ...)
{
    .require("Biobase")
    callNextMethod(x, ...)
})
