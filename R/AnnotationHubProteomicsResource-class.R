setClass("mzRpwizResource", contains="AnnotationHubResource")
setMethod(".get1", "mzRpwizResource",
    function(x, ...) 
{
    .require("mzR")
    yy <- cache(.hub(x))
    mzR::openMSfile(yy, backend = "pwiz")
})

setClass("mzTabResource", contains="RdaResource")
setMethod(".get1", "MSnSetResource",
    function(x, ...) 
{
    .require("MSnbase")
    yy <- cache(.hub(x))
    load(yy) 
})

setClass("mzRidentResource", contains="AnnotationHubResource")
setMethod(".get1", "mzRidentResource",
    function(x, ...) 
{
    .require("mzR")
    yy <- cache(.hub(x))
    mzR::openIDfile(yy)
})


setClass("MSnSetResource", contains="AnnotationHubResource")
setMethod(".get1", "MSnSetResource",
    function(x, ...) 
{
    yy <- cache(.hub(x))
    .require("MSnbase")
    load(yy) 
})

setClass("AAStringSetResource", contains="AnnotationHubResource")
setMethod(".get1", "AAStringSetResource",
     function(x, ...) 
{
    .require("Biostrings")
    yy <- cache(.hub(x))
    Biostrings::readAAStringSet(yy)
})

