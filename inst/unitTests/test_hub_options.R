test_option_key <- function(){

    checkIdentical(.hub_option_key("TESTING"), "TESTING")
    checkException(.hub_option_key("NOTFOUND"))
}

test_get_option <- function(){

    checkException(getAnnotationHubOption("NOTFOUND"))
    checkIdentical(getAnnotationHubOption("URL"),
                   "https://annotationhub.bioconductor.org")

}

test_set_option <- function(){

    checkException(setAnnotationHubOption("NOTFOUND"))
    orig_max <- getAnnotationHubOption("MAX_DOWNLOADS")
    orig_test <- getAnnotationHubOption("TESTING")
    setAnnotationHubOption("MAX_DOWNLOADS",100)
    checkIdentical(getAnnotationHubOption("MAX_DOWNLOADS"), 100L)
    setAnnotationHubOption("MAX_DOWNLOADS",orig_max)
    setAnnotationHubOption("TESTING", TRUE)
    checkIdentical(getAnnotationHubOption("TESTING"), TRUE)
    setAnnotationHubOption("TESTING", FALSE)
    checkIdentical(getAnnotationHubOption("TESTING"), FALSE)
    setAnnotationHubOption("TESTING", orig_test)
}
