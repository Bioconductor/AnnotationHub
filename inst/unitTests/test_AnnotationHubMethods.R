## Unit tests for methods and helpers that are used to set and use
## filters on an AnnotationHub object.

x <- AnnotationHub()


## can we get and set filters() for an object?
test_filters <- function(){
    
}


## when the user hits enter, they should either get a DL OR a list of paths.
## Does that happen?
test_getResource <- function(){

}


## test are valid filters being used?
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


