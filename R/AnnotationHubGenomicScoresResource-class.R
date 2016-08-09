setClass("GenomicScoresResource", contains="AnnotationHubResource")

setMethod(".get1", "GenomicScoresResource",
   function(x, ...)
{
    .require("GenomicScores")
    readRDS(cache(getHub(x)))
})
