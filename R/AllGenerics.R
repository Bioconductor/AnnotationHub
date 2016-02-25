### =========================================================================
### All Generics
### -------------------------------------------------------------------------
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Hub objects 
###

setGeneric("hubCache", signature="x",
    function(x) standardGeneric("hubCache")
)
setGeneric("hubUrl", signature="x",
    function(x) standardGeneric("hubUrl")
)
setGeneric("hubDate", signature="x",
    function(x) standardGeneric("hubDate")
)
setGeneric("snapshotDate", 
    function(x, ...) standardGeneric("snapshotDate")
)
setGeneric("snapshotDate<-", signature="x",
    function(x, value) standardGeneric("snapshotDate<-")
)
setGeneric("package", signature="x",
    function(x, value) standardGeneric("package")
)
## cache returns either the path to the URL or the local path (in a cache)
## along the way it downloads the resource that it locates 
## Already expecting multiple ids from .dataPathIds(), (potentially)
## This is the path based on the RdataPath? (currenntly its based on resouce_id)
setGeneric("cache", signature="x",
    function(x, ...) standardGeneric("cache")
)
setGeneric("cache<-", signature="x",
    function(x, ..., value) standardGeneric("cache<-")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### AnnotationHubResource objects
###

setGeneric("getHub", signature="x",
    function(x) standardGeneric("getHub")
)
