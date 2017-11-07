### =========================================================================
### EnsDb objects
### -------------------------------------------------------------------------
###

setClass("EnsDbResource", contains="AnnotationHubResource")

setMethod(".get1", "EnsDbResource",
    function(x, ...)
{
    .require("ensembldb")
    ensembldb::EnsDb(cache(getHub(x)))
})
