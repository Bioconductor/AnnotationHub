## onload will check if you have used caching before and if not print
## a helpful message

.onLoad <- function(libname, pkgname)
{
    if (is.na(hubCache())) {
        cache <- tempfile()
        dir.create(tempfile(), recursive=TRUE)
        options(AnnotationHub.Cache=cache)
        message("See ?hubCache to enable a permanent AnnotationHub cache")
    }
}

## Load cpp Trie module
loadModule("TrieModule", TRUE)
