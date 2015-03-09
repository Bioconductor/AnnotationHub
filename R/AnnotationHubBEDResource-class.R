## THis file contains methods for all BED files

setClass("BEDFileResource", contains="AnnotationHubResource")

setMethod(".get1", "BEDFileResource",
    function(x, extraCols, ...)
{
    .require("rtracklayer")
    dat <- cache(.hub(x))
    if(length(extraCols)!=0)
        rtracklayer::import(dat, format="bed", extraCols=extraCols)
    else
        rtracklayer::import(dat, format="bed")
})


setClass("UCSCBroadPeakResource", contains="BEDFileResource")

setMethod(".get1", "UCSCBroadPeakResource",
    function(x, ...)
{
    broadPeaksmcols <- c(signalValue="numeric",
        pValue="numeric", qValue="numeric") 
    fl <- callNextMethod(x, extraCols="broadPeaksmcols")
})

setClass("UCSCNarrowPeakResource", contains="BEDFileResource")

setMethod(".get1", "UCSCNarrowPeakResource",
    function(x, ...)
{
    narrowPeaksmcols <- c(
        signalValue="numeric", pValue="numeric",
        qValue="numeric", peak="numeric") 
    fl <- callNextMethod(x, extraCols=narrowPeaksmcols)
})

setClass("UCSCBedRnaElementsResource", contains="BEDFileResource")

setMethod(".get1", "UCSCBedRnaElementsResource",
    function(x, ...)
{
    narrowPeaksmcols <- c(name="character", score="integer",
        strand="character",
        level="numeric", signif="numeric", score2="numeric") 
    fl <- callNextMethod(x, extraCols=narrowPeaksmcols)
})

setClass("SimpleBedResource", contains="BEDFileResource")

setMethod(".get1", "HaemcodeBedResource",
    function(x, ...)
{
    fl <- callNextMethod(x, extraCols=character(0))
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

