test_query <- function() {
    ah = AnnotationHub()
    q1 <- query(ah, c("GTF", "Ensembl", "Homo sapiens"))
    checkEquals(11, nrow(mcols(q1)))
    nm <- c("title", "dataprovider", "species", "taxonomyid", "genome", 
        "description", "tags", "rdataclass", "sourceurl", "sourcetype")
    checkEquals(nm, names(mcols(q1)))
}

test_metadata <- function() {
    ah = AnnotationHub()
    gtf <- query(ah , c("gtf", "77", "Ensembl", "Homo sapiens"))
    gr <- gtf[[1]]
    checkEquals("AH28812", as.character(metadata(gr)))
    checkEquals("GRCh38", unique(genome(gr)))
} 

