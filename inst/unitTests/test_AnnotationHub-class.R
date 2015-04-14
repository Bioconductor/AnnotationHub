test_query <- function() {
    ah = AnnotationHub()
    q1 <- query(ah, c("GTF", "Ensembl", "Homo sapiens"))
    checkEquals(11, nrow(mcols(q1)))
    nm <- c("title", "dataprovider", "species", "taxonomyid", "genome", 
        "description", "tags", "rdataclass", "sourceurl", "sourcetype")
    checkEquals(nm, names(mcols(q1)))
}
