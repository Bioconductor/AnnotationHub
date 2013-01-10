## onload will check if you have used caching before and if not print
## a helpful message


.onLoad <- function(libname, pkgname)
{
    if(.checkCaching()==FALSE){   
        message("Caching is not currently enabled.  To enable local caching mechanism, please call the enableCaching() method on your AnnotationHub object.")
    }
}
