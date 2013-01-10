## Things I will need for caching.

## look at code in utils package for packages2.R (lib argument of
## install.packages.  Specifically we want to steal the part where
## they create a personal library to install into.  Except ours needs
## to NOT end in something like "library/base", and to instead end in
## something specific like "2.12/01_08_2013"
## Other useful methods (potentially)
##.libPaths(), system.file(), file.path(), ask.yes.no(),


## AND I am pretty sure we want to just do this to get our dir
## unlist(strsplit(Sys.getenv("R_LIBS_USER"),.Platform$path.sep))[1L]

## And I don't think we want to put this into libPaths.  Just to be
## able to get at it.  But we can create an option

## Then when the user loads the object, it should (every time) check
## if there is this alternate dir set, and if not suggest one
## (ask.yes.no).  Then it can set a flag in the object and write to
## that dir from then on.  The flag will tell the internal methods if
## they need to be writing to and/or checking the cache for data.



## TODO:

## 1) add flag for caching to object (default = FALSE) - DONE
## 2) constructor will call check/create dir (that code can live here)
## and ask users if they want to create the directory etc.

     ## a. check if dir exists.  If so, set flag and move along.
     ## b. if no dir exists, offer to make one (ask.yes.no) and set flag.
## - DONE

.baseUserDir <- function(){
    userDir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"),
                               .Platform$path.sep))[1L]
    userDir <- dirname(dirname(userDir))
    userDir <- file.path(userDir, "ah")
    userDir
}


## ## borrowed from utils package (where it's not exported)
## ask.yes.no <- function(msg) {
##     userdir <- .baseUserDir()
##     ##' returns "no" for "no",  otherwise 'ans', a string
##     msg <- gettext(msg)
##     if(.Platform$OS.type == "windows") {
##         ans <- winDialog("yesno", sprintf(msg, sQuote(userdir)))
##         if(ans != "YES") "no" else ans
##     } else {
##         ans <- readline(paste(sprintf(msg, userdir), " (y/n) "))
##         if(substr(ans, 1L, 1L) == "n") "no" else ans
##     }
## }

## A method to just extact the current caching value
setMethod("caching", "AnnotationHub", function(x) x@cachingEnabled )

## TODO: switch enableCaching to be a replaceMethod semantic
## So it should be: caching(x) <- TRUE

.caching <- function(x, value){
    userDir <- .baseUserDir()
    if(!dir.create(userDir, recursive=TRUE)){
        warning(gettextf("unable to create %s", sQuote(userDir)),
                domain = NA)
    }else{
        message("switching on the caching and creating a local cache.")
        x@cachingEnabled <- value
    }
    x
}

setReplaceMethod("caching", "AnnotationHub",
                 function(x, value){.caching(x, value)} )


## This helper tries to hook users up with a cache.
## It returns TRUE if one exists or if it can be set up, and FALSE otherwise.
.checkCaching <- function(){
    userDir <- .baseUserDir()
    if(file.exists(userDir)){
        return(TRUE)
    }else{
        return(FALSE)
    }
}
    
##         ans <- ask.yes.no("Would you like to create a cache\n%s\nto save downloaded web resources into?")
##         if(identical(ans, "no")){
##             message("switching the caching off for this session.")
##             return(FALSE)
##         }else if(!dir.create(userDir, recursive = TRUE)){
##             warning(gettextf("unable to create %s", sQuote(userDir)),
##                  domain = NA)
##             return(FALSE)
##         }else{
##             ## set the flag in the object.
##             message("switching on the caching and creating a local cache.")
##             return(TRUE)
##         }
##     }else{## file exists
##         ## set the flag
##         message("Using the available cache directory.")
##         return(TRUE)
##     }
## }



## 3) add to code that gets the files so that it checks 1st if the
## file is in the local cache before going out to get it from the web
## (providing that the flag is set).  If the file is not stored
## locally, it should be stored there after the 1st time.

## - In progress 
## So I ONLY have to change the basePath (when its both available AND saved)

## More TODO:
## 1) include the date and version number - DONE
## 2) there are problems with the path/filename being passed in as
## a string. - DONE
## 3) test and debug
## 4) I will have to make this all work with not only the files, but
## also with the other data that can be obtained (so that things can
## work fully offline).



## example URL for online
## http://annotationhub.bioconductor.org/ah/resources/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints_0.0.1.RData

## example URL for cache:
## ~/R/x86_64-unknown-linux-gnu-library/3.0/ah/2.12/2013-01-02/resources/pub/databases/ensembl/encode/supplementary/integration_data_jan2011/byDataType/footprints/jan2011/all.footprints_0.0.1.RData
