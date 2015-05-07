test_tidyGRanges <- function() {
    gr <- GenomicRanges::GRanges(paste0("chr", c(1, 10, "M", 2, 3)),
                                 IRanges::IRanges(1, 1))
    gr1 <- AnnotationHub:::.tidyGRanges(gr=gr, metadata=FALSE, genome="hg19")

    chr <- paste0("chr", c(1, 2, 3, 10, "M"))
    checkIdentical(chr, GenomeInfoDb::seqlevels(gr1))
    checkIdentical(setNames(rep("hg19", 5), chr), GenomeInfoDb::genome(gr1))
    checkIdentical(setNames(rep(c(FALSE, TRUE), c(4, 1)), chr),
                   GenomeInfoDb::isCircular(gr1))
} 
