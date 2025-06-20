test_tidyGRanges <- function() {
    # Case 1 - GRanges does not have any seqinfo 
    gr <- GenomicRanges::GRanges(paste0("chr", c(1, 10, "M", 2, 3)),
                                 IRanges::IRanges(1, 1))
    gr1 <- AnnotationHub:::.tidyGRanges(gr=gr, metadata=FALSE, genome="hg19")

    chr <- paste0("chr", c(1, 2, 3, 10, "M"))
    checkIdentical(chr, Seqinfo::seqlevels(gr1))
    checkIdentical(setNames(rep("hg19", 5), chr), Seqinfo::genome(gr1))
    checkIdentical(setNames(rep(c(FALSE, TRUE), c(4, 1)), chr),
                   Seqinfo::isCircular(gr1))

    # case -2 genome not supported by Seqinfo::Seqinfo
    gr2 <- AnnotationHub:::.tidyGRanges(gr=gr, metadata=FALSE, genome="NotFound")
    checkIdentical(setNames(rep("NotFound", 5), chr), Seqinfo::genome(gr2))
    checkIdentical(setNames(rep(c(FALSE, TRUE), c(4, 1)), chr),
               Seqinfo::isCircular(gr2))


    # case -3 GRanges has incorrect/missing seqinfo
    Seqinfo::seqlengths(gr) <- c(1,2,3,4,5)
    Seqinfo::isCircular(gr) <- rep(FALSE,5)
    Seqinfo::genome(gr) <- "hg19"
    gr1 <- AnnotationHub:::.tidyGRanges(gr=gr, metadata=FALSE, genome="hg19")

    chr <- paste0("chr", c(1, 2, 3, 10, "M"))
    checkIdentical(chr, Seqinfo::seqlevels(gr1))
    checkIdentical(setNames(rep("hg19", 5), chr), Seqinfo::genome(gr1))
    checkIdentical(setNames(rep(c(FALSE, TRUE), c(4, 1)), chr),
               Seqinfo::isCircular(gr1))
 
} 
