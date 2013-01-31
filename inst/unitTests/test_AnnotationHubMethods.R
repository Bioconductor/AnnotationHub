## Unit tests for methods and helpers that are used to set and use
## filters on an AnnotationHub object.

x <- AnnotationHub()

## What is the shortest example to DL? A: stamH3K4me3ProfilePromoters.RData


## can we get and set filters() for an object?
test_filters <- function(){
    ## by default: no filters (this may change)
    checkTrue(length(filters(x))==0)
    ## then add a filter
    filters(x) <- list(TaxonomyId="9606",
        SourceFile=
        "pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints.gz")
    checkTrue(length(filters(x))==2)
    ## can we set them back to nothing?
    filters(x) <- NULL
    checkTrue(length(filters(x))==0)
}


## when the user hits enter, they should either get a DL OR a list of paths.
## Does that happen?
test_getResource <- function(){
    ## try a specific name
#    name <- "pub.databases.ensembl.encode.supplementary.integration_data_jan2011.byDataType.openchrom.jan2011.promoter_predictions.master_known.bed_0.0.1.RData"
    altName <- "fakedata.data.bed_0.0.3.RData" 
    res <- AnnotationHub:::.getResource(x, altName)
    checkTrue(class(res) == "GRanges")
    
    ## try a less specific name    
    name <- "goldenpath.hg19.encodeDCC.wgEncodeRikenCage.wgEncodeRikenCage"
    suppressWarnings(res2 <- AnnotationHub:::.getResource(x, name))
    checkTrue(class(res2) == "character")
}


## test our filter Validation
## Who watches the watchmen?
test_validFilterValues <- function(){
    badFilter <- list(Foo="9606") ## bad name
    checkException(AnnotationHub:::.validFilterValues(x,badFilter))
    badFilter2 <- list(Tags="9606") ## bad value
    checkException(AnnotationHub:::.validFilterValues(x,badFilter2))
}



## .getMetadata needs to get metadata that is filtered OR NOT, depending.
## Both methods of access should work.
test_getMetadata <- function(){
    filters <- NULL ## IOW: no filters
    res <- AnnotationHub:::.metadata(x,filters)
    checkTrue(dim(res)[1] > 1) ## should give multiple records 
    ## check that we can get filtered metadata
    filters <- list(TaxonomyId="9606",
        SourceFile=
        "pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints.gz")
    res <- AnnotationHub:::.metadata(x,filters)
    checkTrue(dim(res)[1] == 1)  ## should only match one record
}



## I think that I just need to simplify this function.
## Now that metadata is smarter it doesn't need to be so smart.
## test_getNewPathsBasedOnFilters <- function(){
## }



## test that this thing respects merging of values.  If values are
## repeated, they should not end up repeated in the result etc.
test_replaceFilter <- function(){
    filters(x) <- NULL ## null out filters
    checkTrue(length(filters(x))==0)
    x <- AnnotationHub:::.replaceFilter(x,list(TaxonomyId="9606"))
    checkTrue(length(filters(x))==1)
    ## now place a bigger filter on there
    filters <- list(TaxonomyId="9606",
        SourceFile=
        "pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints.gz")
    x <- AnnotationHub:::.replaceFilter(x,filters)
    checkTrue(length(filters(x))==2)  ## TaxonomyId should not repeat.
}



## does this work as expected?  (it should respect the filter values)
test_metadata <- function(){
    filters(x) <- NULL ## null out filters
    resFull <- metadata(x)
    ## Now apply filters
    filters(x) <- list(TaxonomyId="9606",
        SourceFile=
        "pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints.gz")
    resPartial <- metadata(x)
    checkTrue(dim(resFull)[2] == dim(resPartial)[2])
    checkTrue(dim(resFull)[1] != dim(resPartial)[1])
}


## TODO: simplify getNewPathsBasedOnFilters()
## TODO: solve bug that prevents the using of multi-valued keys in the
## filter lists.





## TODO: add tests for:
## versionDate etc.  (make sure they are returning correct things).

## Add tests for caching (test to make sure caching works, and that my helpers to test if cache is present etc. are working)


## some debugging code for caching:


##  library(AnnotationHub); x = AnnotationHub(); filters(x) <- list(TaxonomyId="9606",SourceFile= "pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints.gz");

##  system.time(a <- x$pub.databases.ensembl.encode.supplementary.integration_data_jan2011.byDataType.footprints.jan2011.all.footprints_0.0.1.RData)  



##  FOR testing the caching. the test code should look at the dir that
##  is generated (we have helper for this), and see if the appropriate
##  dirs and files are being generated when code is run.






## more testing

## library(AnnotationHub); x = AnnotationHub(); filters(x) <- list(TaxonomyId="9606")

