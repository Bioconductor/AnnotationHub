## Things I will need for caching.


## Function for getting the base directory to use for caching.
.baseUserDir <- function(){
    if("AnnotationHubCache" %in% names(options())){ ## User has an option set. 
        userDir <- getOption("AnnotationHubCache")
    }else{    
        userDir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"),
                                   .Platform$path.sep))[1L]
        userDir <- dirname(dirname(userDir))
    }
    file.path(userDir, "ah")
}



## A method to just extact the current caching value
setMethod("caching", "AnnotationHub", function(x) x@cachingEnabled )

.tryCacheCreate <- function(x, userDir){
    if(!dir.create(userDir, recursive=TRUE)){
        warning(gettextf("unable to create %s", sQuote(userDir)),
                domain = NA)
    }else{
        message("switching on the caching and creating a local cache.")
        x@cachingEnabled <- value
    }
    x
}

## replacement for caching value also checks for local cache presence/absence
.caching <- function(x, value){
    userDir <- .baseUserDir()
    .tryCacheCreate(x, userDir)
}
setReplaceMethod("caching", c("AnnotationHub", "logical"),
                 function(x, ..., value){.caching(x, value)} )


## replacement for caching when an alternative is offered.
.charCaching <- function(x, value){
    options("AnnotationHubCache"=value) ## set the option
    .tryCacheCreate(x, getOption("AnnotationHubCache")) ## And try to use it
}

setReplaceMethod("caching", c("AnnotationHub", "character"),
                 function(x, ..., value){.charCaching(x, value)} )



## So it should be: caching(x) <- TRUE


## This helper tries to hook users up with a cache.
## It returns TRUE if one exists or if it can be set up, and FALSE otherwise.
.checkCaching <- function(){
    userDir <- .baseUserDir()
    file.exists(userDir)
}




## example URL for online
## http://annotationhub.bioconductor.org/ah/resources/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints_0.0.1.RData

## example URL for cache:
## ~/R/x86_64-unknown-linux-gnu-library/3.0/ah/2.12/2013-01-02/resources/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints_0.0.1.RData
