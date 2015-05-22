test_query <- function() {
    ah = AnnotationHub()
    q1 <- query(ah, c("GTF", "Ensembl", "Homo sapiens"))
    checkTrue("AH7558" %in% names(q1))
    nm <- c("title", "dataprovider", "species", "taxonomyid", "genome", 
        "description", "tags", "rdataclass", "sourceurl", "sourcetype")
    checkEquals(nm, names(mcols(q1)))
}

test_as.list_and_c <- function() {
    ah <- AnnotationHub()
    cc <- selectMethod("c", "AnnotationHub")
    checkIdentical(ah[1:5], do.call("cc", as.list(ah[1:5])))
    checkIdentical(ah[1:5], c(ah[1:2], ah[3:5]))
    checkIdentical(ah[FALSE], c(ah[FALSE], ah[FALSE]))
    checkIdentical(ah[1:5], c(ah[FALSE], ah[1:5]))
    checkIdentical(ah[1:5], c(ah[1:4], ah[3:5])) # unique() ids
}
