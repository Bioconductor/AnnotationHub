## onload will check if you have used caching before and if not print
## a helpful message

.onLoad <- function(libname, pkgname)
{
    if(.checkCaching()==FALSE){   
        message("Caching is not currently enabled.  To enable local caching mechanism, please use the caching() replacement method with your AnnotationHub object.")
    }
}

## Load cpp Trie module
loadModule("TrieModule", TRUE)
