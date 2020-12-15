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
setGeneric("isLocalHub", signature="x",
    function(x) standardGeneric("isLocalHub")
)
setGeneric("isLocalHub<-", signature="x",
    function(x, value) standardGeneric("isLocalHub<-")
)
setGeneric("allVersions", signature="x",
    function(x) standardGeneric("allVersions")
)
setGeneric("allVersions<-", signature="x",
    function(x, value) standardGeneric("allVersions<-")
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
setGeneric("recordStatus", signature="hub",
    function(hub, record) standardGeneric("recordStatus")
)
setGeneric("listResources", signature="hub",
    function(hub, package, filterBy=character()) 
        standardGeneric("listResources")
)
setGeneric("loadResources", signature="hub",
    function(hub, package, filterBy=character()) 
        standardGeneric("loadResources")
)
setGeneric("getInfoOnIds", signature="ids",
    function(hub, ids) standardGeneric("getInfoOnIds")
)
setGeneric("removeResources", signature="ids",
    function(hub, ids) standardGeneric("removeResources")
)
setGeneric("getVersionsOfId", signature="id",
    function(hub, id) standardGeneric("getVersionsOfId")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### AnnotationHubResource objects
###

setGeneric("getHub", signature="x",
    function(x) standardGeneric("getHub")
)
