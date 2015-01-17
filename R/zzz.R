.CACHE_ROOT <- ".AnnotationHub"

.DB_SQLITE <- "annotationhub.sqlite3"  ## "ahtest.sqlite3"

.DB_RESOURCE_FIELDS <- paste(sep=".", collapse=", ", "resources",
    c("ah_id", "title", "dataprovider", "species", "taxonomyid", "genome",
      "description"))

.onLoad <- function(libname, pkgname, ...) {
    ## options from getOption or Sys.env or default, in that order
    if (is.null(hubOption("MAX_DOWNLOADS"))) {
        opt <- getOption("ANNOTATION_HUB_MAX_DOWNLOADS", 10L)
        opt <- Sys.getenv("ANNOTATION_HUB_MAX_DOWNLOADS", opt)
        opt <- as.integer(opt)
        setHubOption("MAX_DOWNLOADS", opt)
    }
    if (is.null(hubOption("URL"))) {
        opt <- getOption("ANNOTATION_HUB_URL",
                         "https://annotationhub.bioconductor.org")
        opt <- Sys.getenv("ANNOTATION_HUB_URL", opt)
        setHubOption("URL", opt)
    }
    if (is.null(hubOption("CACHE"))) {
        path <- switch(.Platform$OS.type, unix = path.expand("~"),
                       windows= file.path(gsub("\\\\", "/",
                       Sys.getenv("HOME")), "AppData"))
        opt <- getOption("ANNOTATION_HUB_CACHE", file.path(path, .CACHE_ROOT))
        opt <- Sys.getenv("ANNOTATION_HUB_CACHE", opt)
        setHubOption("CACHE", opt)
    }
}
