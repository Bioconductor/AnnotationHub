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
        Genome="character",
        Maintainer="character",
        Notes='character',
        RDataClass="character",
        RDataDateAdded="POSIXct",
        RDataLastModifiedDate="POSIXct",
        RDataPath="character",
        RDataSize="numeric",
        RDataVersion="numeric_version",
        Recipe="character",
        RecipeArgs="list",
        SourceFile="character",
        SourceLastModifiedDate="POSIXct",
        SourceMd5="character",
        SourceSize="numeric",
        SourceUrl="character",
        SourceVersion="character",
        Species="character",
        Tags='character',
        TaxonomyId="integer", 
        Title="character",
        Location_Prefix="character"
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
        RDataLastModifiedDate=as.POSIXct(NA_character_),
        RDataPath=NA_character_,
        RDataSize=NA_real_,
        RDataVersion=.NA_version_,
        Recipe=NA_character_,
        RecipeArgs=list(),
        SourceFile=NA_character_,
        SourceLastModifiedDate=as.POSIXct(NA_character_),
        SourceMd5=NA_character_,
        SourceSize=NA_real_,
        SourceVersion=NA_character_,
        Species=NA_character_,
        Tags=NA_character_,
        TaxonomyId=NA_integer_,
        Title=NA_character_,
        Location_Prefix=NA_character_
    )        
)

## ------------------------------------------------------------------------------
## constructor, validity
## 

.derivedFileName <-
    function(originalFile, RDataVersion, suffix)
{
    ret <- sub(".gz$", "", basename(originalFile))
    ret <- paste(ret, collapse="-")
    sprintf("%s_%s.%s", ret, RDataVersion, suffix)
}


## alternative prefix (so far)
## http://hgdownload.cse.ucsc.edu/

AnnotationHubMetadata <-
    function(AnnotationHubRoot, SourceFile, SourceUrl, SourceVersion,
        SourceMd5, SourceSize, DataProvider, Title, Description,
        Species, TaxonomyId, Genome, Tags, Recipe, RecipeArgs =
        list(), RDataClass, RDataVersion, RDataDateAdded, RDataPath, Maintainer,
        ..., BiocVersion=biocVersion(), Coordinate_1_based = TRUE,
        Notes=NA_character_,
        Location_Prefix='http://s3.amazonaws.com/annotationhub/')
{
    if (missing(SourceMd5))
        SourceMd5 <- unname(tools::md5sum(SourceFile))
    if (missing(SourceSize))
        SourceSize <- file.info(SourceFile)$size
    if (missing(TaxonomyId))
    {
        if (!is.na(Species) && require(AnnotationHubData))
            TaxonomyId <- AnnotationHubData:::.taxonomyId(Species)
        else
            TaxonomyId <- NA_character_
    }
    ## This is probably too aggressive since some resources will not have this?
    if (missing(RDataPath)) {
        resourceDir <- dirname(SourceFile[1])
        resourceFiles <- .derivedFileName(SourceFile,  RDataVersion, "RData")
        RDataPath <- file.path(resourceDir, resourceFiles)
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
        RDataVersion=numeric_version(RDataVersion),
        Recipe=Recipe,
        RecipeArgs=RecipeArgs,
        SourceFile=SourceFile,
        SourceMd5=SourceMd5,
        SourceSize=SourceSize,
        SourceUrl=SourceUrl,
        SourceVersion=SourceVersion,
        Species=Species,
        Tags=Tags,
        TaxonomyId=TaxonomyId,
        Title=Title,
        Location_Prefix=Location_Prefix,
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

