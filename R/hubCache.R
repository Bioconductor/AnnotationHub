.hubCache <- function() {
    getOption("AnnotationHub.Cache", NA_character_)
}

.hubCacheExists <- function(filePath) {
    !is.na(filePath) && file.exists(filePath) && is.dir(filePath)
}

setMethod("hubCache", "missing", function(x, ...) .hubCache())

## example URL for online
## http://annotationhub.bioconductor.org/ah/resources/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints_0.0.1.RData

## example URL for cache:
## ~/R/x86_64-unknown-linux-gnu-library/3.0/ah/2.12/2013-01-02/resources/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints_0.0.1.RData
