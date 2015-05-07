.require <-
    function(pkg)
{
    if (length(grep(sprintf("package:%s", pkg), search())) != 0L)
        return()
    message("require(", dQuote(pkg), ")")
    tryCatch({
        suppressPackageStartupMessages({
            require(pkg, quietly=TRUE, character.only=TRUE)
        })
    }, error=function(err) {
        msg <- sprintf("require(%s) failed: %s", dQuote(pkg),
                       conditionMessage(err))
        stop(msg)
    })
}

.ask <- function(txt, values) {
    txt <- sprintf("%s [%s] ", txt, paste(values, collapse="/"))
    repeat {
        ans <- tolower(substr(readline(txt), 1, 1))
        if (ans %in% values)
            break
    }
    ans
}

.gunzip <- function(file, destination)
{
    bufferSize <- 1e7
    fin <- gzfile(file, "rb")
    fout <- file(destination, "wb")
    on.exit({
        close(fin)
        close(fout)
    })

    repeat {
        x <- readBin(fin, raw(0L), bufferSize, 1L)
        if (length(x) == 0L)
            break
        writeBin(x, fout, size=1L)
    }

    invisible(destination)
}

## tidyGRanges

.metadataForAH <- 
    function(x, ...)
{
    stopifnot(length(.hub(x)) == 1)
    list(AnnotationHubName=names(.hub(x)))
}

.guessIsCircular <-
    function(x)
{
    ans <- GenomeInfoDb::isCircular(x)
    idx <- is.na(ans)
    test <- names(ans) %in% c("MT", "MtDNA", "dmel_mitochondrion_genome",
                              "Mito", "chrM")
    ans[idx] <- test[idx]
    ans
}

.tidyGRanges <- 
    function(x, gr, sort=TRUE, guess.circular=TRUE, addGenome=TRUE,
             metadata=TRUE, genome=.hub(x)$genome)
{
    if (metadata)
        metadata(gr)  <- .metadataForAH(x)

    tryCatch({
        loadNamespace("GenomeInfoDb")
    }, error=function(err) {
        ## quietly return un-tidied GRanges (?)
        return(gr)
    })

    si <- GenomeInfoDb::seqinfo(gr)
    if (guess.circular)
        GenomeInfoDb::isCircular(si)  <- .guessIsCircular(si)
    if (addGenome)
        GenomeInfoDb::genome(si) <- genome
    if (sort)
        si <- GenomeInfoDb::sortSeqlevels(si) 
    ## TODO: add seqlengths code
    if (sort || guess.circular || addGenome) {
        new2old <- match(GenomeInfoDb::seqlevels(si),
                         GenomeInfoDb::seqlevels(gr))
        GenomeInfoDb::seqinfo(gr, new2old=new2old) <- si
    }
    gr
}
