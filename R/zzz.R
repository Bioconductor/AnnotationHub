## onload will check if you have used caching before and if not print
## a helpful message

.onLoad <- function(libname, pkgname)
{
    if (!.checkCaching())
        message("See ?caching to enable an AnnotationHub cache")
}

## Load cpp Trie module
loadModule("TrieModule", TRUE)
