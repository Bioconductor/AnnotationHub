## Accessors
setGeneric("hubUrl", function(x, ...) standardGeneric("hubUrl"))

setGeneric("hubCache", function(x, ...) standardGeneric("hubCache"))

setGeneric("hubCache<-",
           function(x, ..., value) standardGeneric("hubCache<-"))

setGeneric("snapshotVersion",
           function(x, ...) standardGeneric("snapshotVersion"))

setGeneric("snapshotDate", function(x, ...) standardGeneric("snapshotDate"))

setGeneric("snapshotUrl",
           function(x, ...) standardGeneric("snapshotUrl"))

setGeneric("snapshotPaths",
           function(x, ...) standardGeneric("snapshotPaths"))

setGeneric("snapshotUrls",
           function(x, ...) standardGeneric("snapshotUrls"))

setGeneric("snapshotDate<-", signature=c("x", "value"),
           function(x, ..., value) standardGeneric("snapshotDate<-"))

setGeneric("possibleDates", function(x, ...) standardGeneric("possibleDates"))

setGeneric("hubResource", signature="x",
           function(x, path=character(), ...) standardGeneric("hubResource"))

setGeneric("filters", function(x, ...) standardGeneric("filters"))

setGeneric("filters<-", signature=c("x", "value"),
           function(x, ..., value) standardGeneric("filters<-"))
