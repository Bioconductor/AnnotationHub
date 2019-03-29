test_cache_datapathIds <- function() {
    ## map hub identifiers AH123 to cached identifier(s)
    hub <- AnnotationHub()

    ## 1:1 mapping
    result <- AnnotationHub:::.datapathIds(hub["AH28854"])
    checkIdentical(result, structure(34294L, .Names = "AH28854"))

    ## 1:several mapping
    #result <- AnnotationHub:::.datapathIds(hub["AH169"])
    #checkIdentical(result,
    #               structure(c(169L, 14130L), .Names = c("AH169", "AH169")))
    # Removed old ra zip files that needed index
    #   Currently no id associated with two files in 3.9


    ## unknown identifier
    result <- AnnotationHub:::.datapathIds(hub["AH0"])
    checkIdentical(result, setNames(integer(), character()))
}
