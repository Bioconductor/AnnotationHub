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
    meta <- .hub(x)
    list(AnnotationHubName=names(meta), 
         `File Name`=basename(meta$sourceurl),
         `Data Source`=meta$sourceurl,
         `Provider`=meta$dataprovider,
         `Organism`=meta$species,
         `Taxonomy ID`=meta$taxonomyid )
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

# tasks we want .tidyGRanges() to do:
# 1. add metdata() to GRanges containing the names() of hub object
# 2. sortSeqlevels()
# 3. fill the seqinfo with correct information
# for step 3 - comparison is done with existingSeqinfo and 
# GenomeInfoDb::Seqinfo() - currently if its not the same, seqinfo is replaced.

.tidyGRanges <- 
    function(x, gr, sort=TRUE, guess.circular=TRUE, addGenome=TRUE,
             metadata=TRUE, genome=.hub(x)$genome)
{
    if (metadata)
        metadata(gr)  <- .metadataForAH(x)

    ## BEWARE: 
    ## 1) GenomeInfoDb::Seqinfo doesnt sortSeqlevels - so we need to 
    ## sortSeqlevels before comparison else identical wont work.
    ## 2) case - the input GRanges might have a subset of seqlevels whereas
    ## the GenomeInfoDb::Seqinfo returns all seqlevels with scaffolds
    ## from an assembly.  
    ## 3)only 10-15 genomes supported by GenomeInfoDb::Seqinfo

    tryCatch({
        loadNamespace("GenomeInfoDb")
    }, error=function(err) {
        ## quietly return un-tidied GRanges (?)
        return(gr)
    })      
    
    
    if (sort)
        gr <- GenomeInfoDb::sortSeqlevels(gr) 
    existingSeqinfo <- GenomeInfoDb::seqinfo(gr)    

    ## Not all Genome's are supported by GenomeInfoDb::Seqinfo
    newSeqinfo <- tryCatch({
        GenomeInfoDb::Seqinfo(genome=genome)
    }, error= function(err){
         message( "Using guess work to populate seqinfo ", conditionMessage(err))
         
    })
    
    if(class(newSeqinfo)!="Seqinfo"){
	# use guess work to populate
        if (guess.circular)
            GenomeInfoDb::isCircular(existingSeqinfo)  <- 
                .guessIsCircular(existingSeqinfo)
        if (addGenome)
            GenomeInfoDb::genome(existingSeqinfo) <- genome
        if (sort || guess.circular || addGenome) {
            new2old <- match(GenomeInfoDb::seqlevels(existingSeqinfo),
                        GenomeInfoDb::seqlevels(gr))
            GenomeInfoDb::seqinfo(gr, new2old=new2old) <- existingSeqinfo
        }
        return(gr)
    }
   

    
    newSeqinfo <- newSeqinfo[GenomeInfoDb::seqlevels(gr)]
    # comapre the current and new seqinfo
    diffSeqlengths <- setdiff(GenomeInfoDb::seqlengths(newSeqinfo), 
                          GenomeInfoDb::seqlengths(existingSeqinfo))  
    diffSeqnames <- setdiff(GenomeInfoDb::seqnames(newSeqinfo), 
                        GenomeInfoDb::seqnames(existingSeqinfo)) 
    diffGenome <- identical(unique(GenomeInfoDb::genome(newSeqinfo)), 
                      unique(GenomeInfoDb::genome(existingSeqinfo))) 
    diffIscircular <- identical(table(GenomeInfoDb::isCircular(newSeqinfo)), 
                          table(GenomeInfoDb::isCircular(existingSeqinfo)))
    len <- c(length(diffSeqlengths), length(diffSeqnames))
    
    # if its the same dont replace 
    if(all(unique(len)==0 & diffGenome & diffIscircular))
        return(gr)   

    ## Replace incorrect seqinfo 
    if (sort || guess.circular || addGenome) {
        new2old <- match(GenomeInfoDb::seqlevels(gr), 
                         GenomeInfoDb::seqlevels(newSeqinfo))
        GenomeInfoDb::seqinfo(gr, new2old=new2old) <- newSeqinfo
    }
    gr
}
