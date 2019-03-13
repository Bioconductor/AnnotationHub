########################################################
# couldn't implement
#   do to ERROR in BiocManager::version()
#   when repeating calls to different cache location
#########################################################
test_cache <- function(){
    cache <- tempfile()
    dir.create(cache)
    ah <- AnnotationHub(cache=cache)
    checkTrue(dir.exists(cache))
    locfiles <- dir(cache)
    checkTrue(file.exists(file.path(cache, "BiocFileCache.sqlite")))
    checkTrue(any(endsWith(locfiles, "annotationhub.sqlite3")))
    checkTrue(any(endsWith(locfiles, "hub_index.rds")))
    temp <- AnnotationHub:::.create_cache(.class="AnnotationHub",
                                          url=getAnnotationHubOpion("URL"),
                                          cache=cache,
                                          proxy=getAnnotationHubOption("PROXY"),
                                          localHub=FALSE)
    checkIdentical(hubCache(ah), dirname(temp))
    bfc <- BiocFileCache(cache)
    checkIdentical(length(bfc), 2L)

    bfc2 <- AnnotationHub:::.get_cache(ah)
    checkIdentical(bfc, bfc2)

    removeCache(ah, ask=FALSE)
}

test_cache_download_ok_maxdownloads<- function() {
    FUN <- AnnotationHub:::.cache_download_ok
    hub <- AnnotationHub()

    checkIdentical(rep(TRUE, 3), unname(FUN(hub, rep(tempfile(), 3), 3,force=FALSE, verbose=FALSE)))
    cachepath <- AnnotationHub:::.named_cache_path(hub[1:3])
    paths <- cache(hub[1:3])
    checkIdentical(rep(FALSE,3), unname(FUN(hub, cachepath, 3, force=FALSE, verbose=FALSE)))
    checkIdentical(rep(TRUE, 3), unname(FUN(hub, cachepath, 3, force=TRUE,
                                            verbose=FALSE)))
    if (!interactive()){
        checkException(FUN(hub, cachepath, max.downloads=2, force=TRUE, verbose=FALSE))
    }
}
