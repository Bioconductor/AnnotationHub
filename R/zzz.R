## onload will set your caching to your home dir by default.

.onLoad <- function(libname, pkgname)
{
    if (is.na(hubCache())) {
        os <- .Platform$OS.type
        basePath <- switch(os,
                           "unix" = path.expand("~"),
                           "windows"= file.path(Sys.getenv("HOME")),
                             "AppData")
        cache <- file.path(basePath, ".AnnotationHub")
        suppressWarnings(dir.create(cache, recursive=TRUE))
        options(AnnotationHub.Cache=cache)
    }
}
