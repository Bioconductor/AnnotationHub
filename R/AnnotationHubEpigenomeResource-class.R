## all code for Epigenome RoadMap files

setClass("EpiMetadataResource", contains="AnnotationHubResource")

setMethod(".get1", "EpiMetadataResource",
   function(x, ...)
{
    read.delim(cache(.hub(x)))
})

setClass("EpigenomeRoadmapFileResource", contains="AnnotationHubResource")

setMethod(".get1", "EpigenomeRoadmapFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    yy <- .hub(x)
    gr <- rtracklayer::import(cache(yy), format="bed", genome=yy$genome,
        extraCols=c(signalValue="numeric", pValue="numeric", qValue="numeric",
        peak="numeric"))
    .tidyGRanges(x, gr)
})
