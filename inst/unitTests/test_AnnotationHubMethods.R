## Unit tests for methods and helpers that are used to set and use
## filters on an AnnotationHub object.

x <- AnnotationHub()


## can we get and set filters() for an object?
test_filters <- function(){
    ## by default: no filters (this may change)
    checkTrue(length(filters(x))==0)
    ## then add a filter
    filters(x) <- list(Organism="9606", File="all.footprints.gz")
    checkTrue(length(filters(x))==2)
    ## can we set them back to nothing?
    filters(x) <- NULL
    checkTrue(length(filters(x))==0)
}


## when the user hits enter, they should either get a DL OR a list of paths.
## Does that happen?
test_getResource <- function(){
    ## try a specific name
    ## TODO: what is the shortest example? A - stamH3K4
    name <- "pub.databases.ensembl.encode.supplementary.integration_data_jan2011.byDataType.openchrom.jan2011.promoter_predictions.stamH3K4me3ProfilePromoters.RData"
    res <- AnnotationHub:::.getResource(x, name)
    
    ## try a less specific name    
    name <- "goldenpath.hg19.encodeDCC.wgEncodeR"
    res <- AnnotationHub:::.getResource(x, name)
    
}


## test are valid filters being used?
## a bunch of checkException calls...
test_validFilterValues <- function(){

}



## .getMetadata needs to get metadata that is filtered OR NOT, depending.
## both methods of access should work.
## Lets get an example of both and then compare
test_getMetadata <- function(){

}



## I think that I just need to simplify this function.
## Now that metadata is smarter it doesn't need to be so smart.
## test_getNewPathsBasedOnFilters <- function(){
## }



## test that this thing respects merging of values.  If values are
## repeated, they should not end up repeated in the result etc.
test_replaceFilter <- function(){

}



## does this work as expected?  (it should respect the filter values)
test_metadata <- function(){

}


