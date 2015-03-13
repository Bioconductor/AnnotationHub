## THis file contains methods for all BED files

setClass("BEDFileResource", contains="AnnotationHubResource")

setMethod(".get1", "BEDFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    dat <- cache(.hub(x))
    dat <- rtracklayer::BEDFile(dat)
    rtracklayer::import(dat, format="bed", ...)
})


setClass("UCSCBroadPeakResource", contains="BEDFileResource")

setMethod(".get1", "UCSCBroadPeakResource",
    function(x, ...)
{
    broadPeaksmcols <- c(signalValue="numeric",
        pValue="numeric", qValue="numeric") 
    callNextMethod(x, extraCols=broadPeaksmcols)
})

setClass("UCSCNarrowPeakResource", contains="BEDFileResource")

setMethod(".get1", "UCSCNarrowPeakResource",
    function(x, ...)
{
    narrowPeaksmcols <- c(
        signalValue="numeric", pValue="numeric",
        qValue="numeric", peak="numeric") 
    callNextMethod(x, extraCols=narrowPeaksmcols)
})

setClass("UCSCBEDRnaElementsResource", contains="BEDFileResource")

setMethod(".get1", "UCSCBEDRnaElementsResource",
    function(x, ...)
{
    mcols <- c(name="character", score="integer",
        strand="character",
        level="numeric", signif="numeric", score2="numeric") 
    callNextMethod(x, extraCols=mcols)
})

setClass("UCSCGappedPeakResource", contains="BEDFileResource")

setMethod(".get1", "UCSCGappedPeakResource",
    function(x, ...)
{
    gappedPeakmcols <- c(signalValue="numeric",
        pValue="numeric", qValue="numeric")
    callNextMethod(x, extraCols=gappedPeakmcols)    
})

setClass("EpigenomeRoadmapFileResource", contains="AnnotationHubResource")

setMethod(".get1", "EpigenomeRoadmapFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    er <- cache(.hub(x))
    rtracklayer::import(er, format="bed",
        extraCols=c(signalValue="numeric", pValue="numeric", qValue="numeric",
        peak="numeric"))
})

