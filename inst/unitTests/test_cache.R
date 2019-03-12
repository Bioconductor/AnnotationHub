test_cache_download_ok<- function() {
    FUN <- AnnotationHub:::.cache_download_ok
    hub <- AnnotationHub()

    checkIdentical(rep(TRUE, 3), unname(FUN(hub, rep(tempfile(), 3), 3,force=FALSE, verbose=FALSE)))

}
