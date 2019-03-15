test_updatehub <- function(){

    bfctemp <- BiocFileCache(cache=tempdir())
    checkException(
        AnnotationHub:::.updateHubDB(bfctemp, "annotationhub",
                                     url=getAnnotationHubOption("URL"),
                                     proxy=getAnnotationHubOption("PROXY"),
                                     localHub=TRUE))
    ah <-  AnnotationHub()
    bfc <- AnnotationHub:::.get_cache(ah)
    res <- bfcquery(bfc, "annotationhub.sqlite3",
                    field="rname", exact=TRUE)
    checkTrue(bfccount(res) == 1L)
    path <- AnnotationHub:::.updateHubDB(bfc, "annotationhub",
                                         url=getAnnotationHubOption("URL"),
                                         proxy=getAnnotationHubOption("PROXY"),
                                         localHub=TRUE)
    checkIdentical(unname(bfcpath(bfc, rids=bfcrid(res))), path)

}

test_hub_cache_resource <- function(){

    ah = AnnotationHub()
    cachepath <- AnnotationHub:::.named_cache_path(ah[1])
    fetchpath <- AnnotationHub:::.hub_data_path(hubUrl(ah))
    hubpath <- file.path(fetchpath, cachepath)
    namescachepath <- names(cachepath)
    bfc <- AnnotationHub:::.get_cache(ah)
    proxy <- getAnnotationHubOption("PROXY")
    # valid    
    checkTrue(AnnotationHub:::.hub_cache_resource(hubpath,
                                                  namescachepath,
                                                  cachepath,
                                                  bfc,
                                                  proxy))
    #invalid
    checkIdentical(AnnotationHub:::.hub_cache_resource("bogus/url",
                                                       "AHInvalid",
                                                       cachepath,
                                                       bfc,
                                                       proxy), FALSE)
                                                 
}
