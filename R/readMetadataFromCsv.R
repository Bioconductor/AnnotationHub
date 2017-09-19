### =========================================================================
### readMetadataFromCsv()
### -------------------------------------------------------------------------
###

## High level helper used to check metadata in 'Hub' packages.
readMetadataFromCsv <- function(pathToPackage, fileName=character())
{
    if (!length(fileName))
        fileName <- "metadata.csv"
    path <- file.path(pathToPackage, "inst", "extdata")
    meta <- read.csv(file.path(path, fileName), colClasses="character",
                     stringsAsFactors=FALSE)
    mat <- rbind(c("Title", "character"),
                 c("Description", "character"),
                 c("BiocVersion", "character"),
                 c("Genome", "character"),
                 c("SourceType", "character"),
                 c("SourceUrl", "character"),
                 c("SourceVersion", "character"),
                 c("Species", "character"),
                 c("TaxonomyId", "integer"),
                 c("Coordinate_1_based", "logical"),
                 c("DataProvider", "character"),
                 c("Maintainer", "character"),
                 c("RDataClass", "character"),
                 c("DispatchClass", "character"),
                 c("RDataPath", "character"))

    expected <- mat[,1]
    missing <- !expected %in% names(meta)
    if (any(missing))
        stop(paste0("missing fields in metadata file ", fileName, ": ",
                    paste(expected[missing], collapse=", ")))
    extra<- !names(meta) %in% expected

    ## All fields length 1
    apply(meta, 1,
        function(xx) {
            valid <- sapply(xx, function(field) length(field) == 1L)
            if (any(!valid))
                stop(paste0("all fields in ", fileName, " must be a character ",
                     "string of length 1"))
        }

    )
    ## Populate required fields
    missing <- is.na(nchar(meta$DataProvider))
    if (any(missing)) {
        meta$DataProvider[missing] <- "NA"
        message("missing values for 'DataProvider set to 'NA''")
    }
    if (any(is.na(meta$Coordinate_1_based))) {
        meta$Coordinate_1_based <- TRUE
        message("missing values for 'Coordinate_1_based set to TRUE'")
    } else {
        meta$Coordinate_1_based <- as.logical(meta$Coordinate_1_based)
    }
    ## Enforce data type
    meta$TaxonomyId <- as.integer(meta$TaxonomyId)
    meta$BiocVersion <- package_version(meta$BiocVersion)

    ## Location_Prefix not specified -> data in S3
    if (all(is.null(Location_Prefix <- meta$Location_Prefix))) {
        meta$Location_Prefix <- 'http://s3.amazonaws.com/annotationhub/'
    ## Location_Prefix specified -> data at other location
    }

    if(all(
        (meta$Location_Prefix == 'http://s3.amazonaws.com/annotationhub/') ||
        (meta$Location_Prefix == 'http://s3.amazonaws.com/experimenthub/'))
       ){
        package <- basename(pathToPackage)
        if (!all(sapply(meta$RDataPath,
                        pattern=paste0("^",package), FUN=grepl))){
            stop(paste0("RDataPath must start with package name: ", package))
        }
    }

    ## Real time assignments
    meta$RDataDateAdded <- rep(Sys.time(), nrow(meta))
    meta
}
