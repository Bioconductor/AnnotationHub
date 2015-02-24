## ------------------------------------------------------------------------------
setOldClass(c("POSIXct", "POSIXt"))
setOldClass("numeric_version")
setOldClass(c("package_version", "numeric_version"))

.NA_version_ <- numeric_version("0.0")  # proxy for unknown version

## .as.numeric_version <-
##     function(x, ...)
## {
##     if (is(x, "character"))
##         x[x == "unknown"] <- as.character(.NA_version_)
##     base::as.numeric_version(x)
## }

## Class defintion
## 
## The prototype needs to be fully specified, using 'NA' to indicate
## unknown, otherwise to / from JSON is confused

setClass("AnnotationHubMetadata",
    representation(
        AnnotationHubRoot="character",
        BiocVersion="package_version",
        Coordinate_1_based="logical",
        DataProvider="character",
        DerivedMd5="character",
        Description='character',
        Genome="character",                    ## needed for record_id
        Maintainer="character",
        Notes='character',
        RDataClass="character",                ## needed for record_id
        RDataDateAdded="POSIXct",
        RDataPath="character",
        Recipe="character",                 ## no longer needed for record_id
        RecipeArgs="list",
        SourceLastModifiedDate="POSIXct",
        SourceMd5="character",
        SourceSize="numeric",
        SourceUrl="character",                 ## needed for record_id
        SourceVersion="character",
        SourceType="character",
        Species="character",
        Tags='character',
        TaxonomyId="integer",                  ## needed for record_id
        Title="character",
        Location_Prefix="character",
        DispatchClass="character",
        PreparerClass="character"              ## needed for record_id
    ),
    prototype = prototype(
        AnnotationHubRoot=NA_character_,
        BiocVersion=biocVersion(),
        Coordinate_1_based=NA,
        DataProvider=NA_character_,
        DerivedMd5=NA_character_,
        Description=NA_character_,
        Genome=NA_character_,
        Maintainer=
            "Bioconductor Package Maintainer <maintainer@bioconductor.org>",
        Notes=NA_character_,
        RDataClass=NA_character_,
        RDataDateAdded=as.POSIXct(NA_character_),
        RDataPath=NA_character_,
        Recipe=NA_character_,
        RecipeArgs=list(),
        SourceLastModifiedDate=as.POSIXct(NA_character_),
        SourceMd5=NA_character_,
        SourceSize=NA_real_,
        SourceVersion=NA_character_,
        SourceType=NA_character_,
        Species=NA_character_,
        Tags=NA_character_,
        TaxonomyId=NA_integer_,
        Title=NA_character_,
        Location_Prefix=NA_character_,
        DispatchClass=NA_character_,
        PreparerClass=NA_character_
    )
)


## ------------------------------------------------------------------------------
## constructor, validity
## 

.derivedFileName <-
    function(originalFile, suffix)
{
    ret <- sub(".gz$", "", basename(originalFile))
    ret <- paste(ret, collapse="-")
    sprintf("%s_%s.%s", ret, suffix)
}


## alternative prefix (so far)
## http://hgdownload.cse.ucsc.edu/

AnnotationHubMetadata <-
    function(AnnotationHubRoot,  SourceUrl, SourceType, SourceVersion,
        SourceMd5, SourceSize, DataProvider, Title, Description,
        Species, TaxonomyId, Genome, Tags, Recipe, RecipeArgs =
        list(), RDataClass, RDataDateAdded, RDataPath,
        Maintainer, ..., BiocVersion=biocVersion(), Coordinate_1_based = TRUE,
        Notes=NA_character_, DispatchClass,
        Location_Prefix='http://s3.amazonaws.com/annotationhub/')
{
    ## TODO: work out a way to derive these (Sonali has some methods for some files that we can apply using httr etc.)
    ## And we *may just want to continue requiring that the recipe give this value - and then not store it.
    ## if (missing(SourceMd5))
    ##     SourceMd5 <- unname(tools::md5sum(SourceFile))
    ## if (missing(SourceSize))
    ##     SourceSize <- file.info(SourceFile)$size
    if (missing(TaxonomyId))
    {
        if (!is.na(Species) &&
            requireNamespace("AnnotationHubData", quietly=TRUE))
            TaxonomyId <- AnnotationHubData:::.taxonomyId(Species)
        else
            TaxonomyId <- NA_character_
    }
    ## This was probably too aggressive since some resources do not have the sourceFile field
    if (missing(RDataPath)) {
        ## resourceDir <- dirname(SourceFile[1])
        ## resourceFiles <- .derivedFileName(SourceFile,  RDataVersion, "RData")
        ## RDataPath <- file.path(resourceDir, resourceFiles)
        Stop("You must supply an Rdatapath")
        ## TODO: try to derive this from the sourceUrl and the location prefix (if present)
    }
    if (missing(AnnotationHubRoot)){
        AnnotationHubRoot <- "/var/FastRWeb/web" ## Dans preferred default
    }
    
    RDataDateAdded <-
        as.POSIXct(strsplit(
            as.character(RDataDateAdded), " ")[[1]][1], tz="GMT")

    new("AnnotationHubMetadata",
        AnnotationHubRoot=AnnotationHubRoot,
        BiocVersion=BiocVersion,
        Coordinate_1_based=Coordinate_1_based,
        DataProvider=DataProvider,
        Description=Description,
        Genome=Genome,
        Maintainer=Maintainer,
        Notes=Notes,
        RDataClass=RDataClass,
        RDataDateAdded=as.POSIXct(RDataDateAdded),
        RDataPath=RDataPath,
        Recipe=Recipe,
        RecipeArgs=RecipeArgs,
#        SourceMd5=SourceMd5,                          ## temporarily required
#        SourceSize=SourceSize,                        ## temporarily required?
        SourceUrl=SourceUrl,
        SourceVersion=SourceVersion,
        SourceType=SourceType,
        Species=Species,
        Tags=Tags,
        TaxonomyId=TaxonomyId,
        Title=Title,
        Location_Prefix=Location_Prefix,
        DispatchClass=DispatchClass, 
        ...
    )
}



## ------------------------------------------------------------------------------
## show
## 

setMethod(show, "AnnotationHubMetadata",
    function(object)
{
    cat("class: ", class(object), '\n', sep='')
    for (slt in sort(slotNames(object))) {
        value <- slot(object, slt)
        txt <- paste0(slt, ": ", paste0(as.character(value), collapse=" "))
        cat(strwrap(txt), sep="\n  ")
    }
})


## check function that verifies that all of a vector of values have a valid prefixes
.checkSourceurlPrefixesAreValid <- function(url){
    safePrefixes <- c('http://','https://','ftp://','rtracklayer://')
    lst <- lapply(safePrefixes, grepl, x=url)
    if(!all(Reduce('|', lst))){
        stop(wmsg(paste0("sourceurl provided has an invalid prefix (missing ",
                        "protocol). Source urls should be full uris that point ",
                        "to the original resources used in a recipe.")))
    }
}  ## it turns out the above is a bit overkill (did not need to be vectorised). :P

## check function to ensure that we don't have double slashes in url
.checkSourceurlsFreeOfDoubleSlashes <- function(url){
    if(grepl("\\w//", url, perl=TRUE)){
        stop(wmsg(paste0("sourceurl provided has a double slash outside of the ",
                         "protocol). Source urls should be working uris that ",
                         "point to the original resources used in a recipe.")))        
    }
}

## try to make sure genomes do not contain weird suffixes.. (should be short)
.checkThatGenomeLooksReasonable <- function(genome){
    if(nchar(genome)>12){
        warning(wmsg(paste0("genome provided is suspiciously long. ",
                         "Check to make sure that the genome is legitimate and ",
                         "does not contain unecessary extensions etc.")))        
    }
}

## check that the rdataclass specified is a real class.
.checkRdataclassIsReal <- function(class){
    tryCatch(isClass(class), error = function(err){
        stop("The rdataclass must be a valid R data type. \n",
             conditionMessage(err))})
}


.checkThatSourceTypeSoundsReasonable <- function(sourcetype){
expectedSourceTypes <- c("BED file",                                            
                         "UCSC track",
                         "VCF file",
                         "GTF file",
                         "GFF file",
                         "CSV file",
                         "TSV file",
                         "BigWig file",
                         "TwoBit file",
                         "Chain file",
                         "FASTA file",
                         "Inparanoid sqltable files",
                         "NCBI gene files with supplemental data from blast2GO",
                         "NHLBI GRASP 2.0 GWAS database")
  if(!(sourcetype %in% expectedSourceTypes)){
      stop(wmsg(paste0("The source type you have provided (",sourcetype,")",
                       " looks unusual.  We were expecting one of these",
                       " values: ",paste(expectedSourceTypes, collapse=", "),
                       ". Please check to make sure that yoour source type",
                       " is really what you want and if so, then please tell",
                       " us about it so that we can add your source type to",
                       " our list of expected values.."))) 
  }
}


.checkForAValidTaxonomyId <- function(taxId){
## TODO: precompute the list of valid tax Ids
if (!exists("speciesMap")) 
         data(speciesMap, package = "AnnotationHubData")
validTaxIds <- unique(speciesMap$taxon)
if(!(taxId %in% validTaxIds)){
      stop(wmsg(paste0("The taxonomy Id you have provided (",taxId,")",
                       " is not in our list of valid Tax Ids.",
                       ". Please check to make sure that your tax ID",
                       " is really legitimate and if so, then please tell",
                       " us about it so that we can update our list."))) 
  }
}
    



setValidity("AnnotationHubMetadata",function(object) {
    msg = NULL
    ## no spaces are allowed int he RDataPath field
    if(grepl(" ", object@RDataPath)){
        msg <- c(msg, "the string for RDataPath cannot contain spaces.")
    }
    ## if the location prefix is "non-standard" (IOW not stored in S3) and 
    ## if the source URL is not the same as rdatapath 
    ## then we need to add a message and fail out
    standardLocationPrefix <- 'http://s3.amazonaws.com/annotationhub/'
    if(object@Location_Prefix != standardLocationPrefix){
        if(object@RDataPath != object@SourceUrl){
            msg <- c(msg, "the string for RDataPath must match the SourceUrl.")
        }
    }
    if (is.null(msg)) TRUE else msg 
    
    ## more checks
    .checkSourceurlPrefixesAreValid(object@SourceUrl)
    .checkSourceurlsFreeOfDoubleSlashes(object@SourceUrl)
    .checkThatGenomeLooksReasonable(object@Genome)
    .checkRdataclassIsReal(object@RDataClass)
    .checkThatSourceTypeSoundsReasonable(object@SourceType)
 ##   .checkForAValidTaxonomyId(object@TaxonomyId)
})
