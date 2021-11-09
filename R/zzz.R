.CACHE_ROOT <- "AnnotationHub"

.onLoad <- function(libname, pkgname, ...) {
    ## options from getOption or Sys.env or default, in that order
    if (is.null(getAnnotationHubOption("MAX_DOWNLOADS"))) {
        opt <- getOption("ANNOTATION_HUB_MAX_DOWNLOADS", 10L)
        opt <- Sys.getenv("ANNOTATION_HUB_MAX_DOWNLOADS", opt)
        opt <- as.integer(opt)
        setAnnotationHubOption("MAX_DOWNLOADS", opt)
    }
    if (is.null(getAnnotationHubOption("URL"))) {
        opt <- getOption("ANNOTATION_HUB_URL",
                         "https://annotationhub.bioconductor.org")
        opt <- Sys.getenv("ANNOTATION_HUB_URL", opt)
        setAnnotationHubOption("URL", opt)
    }
    if (is.null(getAnnotationHubOption("CACHE"))) {
        path <- tools::R_user_dir(.CACHE_ROOT, which="cache")
        opt <- getOption("ANNOTATION_HUB_CACHE", path)
        opt <- Sys.getenv("ANNOTATION_HUB_CACHE", opt)
        setAnnotationHubOption("CACHE", opt)
    }
    if (is.null(getAnnotationHubOption("PROXY"))) {
        opt <- getOption("ANNOTATION_HUB_PROXY", "")
        opt <- Sys.getenv("ANNOTATION_HUB_PROXY", opt)
        if (nzchar(opt))
            setAnnotationHubOption("PROXY", opt)
    }
    if (is.null(getAnnotationHubOption("LOCAL"))) {
        opt <- getOption("ANNOTATION_HUB_LOCAL", FALSE)
        opt <- Sys.getenv("ANNOTATION_HUB_LOCAL", opt)
        opt <- as.logical(opt)
        setAnnotationHubOption("LOCAL", opt)
    }
    if (is.null(getAnnotationHubOption("ASK"))) {
        opt <- getOption("ANNOTATION_HUB_ASK", interactive())
        opt <- Sys.getenv("ANNOTATION_HUB_ASK", opt)
        opt <- as.logical(opt)
        setAnnotationHubOption("ASK", opt)
    }
}
