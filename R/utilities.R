.require <-
    function(pkg)
{
    result <- TRUE
    if (length(grep(sprintf("package:%s", pkg), search())) != 0L)
        return(invisible(result))
    message("require(", dQuote(pkg), ")")

    handler <- function(err) {
        msg <- sprintf("require(%s) failed: %s", dQuote(pkg),
                       conditionMessage(err))
        stop(msg)
    }

    result <- tryCatch(suppressPackageStartupMessages(
        require(pkg, quietly=TRUE, character.only=TRUE)
    ), error=handler)
    if (!result)
        handler(simpleError("use BiocManager::install() to install package?"))
    invisible(result)
}

.ask <- function(txt, values) {
    txt <- sprintf("%s [%s] ", txt, paste(values, collapse="/"))
    repeat {
        ans <- tolower(trimws(readline(txt)))
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
    stopifnot(length(getHub(x)) == 1)
    meta <- getHub(x)
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
             metadata=TRUE, genome=getHub(x)$genome)
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
    }, error= function(err) {
         NULL
    })
    
    if (is.null(newSeqinfo) || !all(GenomeInfoDb::seqlevels(gr) %in% GenomeInfoDb::seqlevels(newSeqinfo))) {
        ## use guess work to populate
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


DispatchClassList <- function(){

    matrix(
    c("FaFile", "Rsamtools::FaFile(); requires rtracklayer",
      "BamFile", "Rsamtools::BamFile(); requires rtracklayer",
      "Rds", "readRDS()",
      "RDS", "readRDS()",
      "Rda", "get(load())",
      "data.frame", "get(load())",
      "dcf", "read.dcf()",
      "GRanges", "get(load()); requires GenomicRanges",
      "VCF", "get(load()); requires VariantAnnotation",
      "ChainFile",
      "rtracklayer::import.chain(); requires rtracklayer and GenomeInfoDb; before using import.chain internally uses gzfile and writeBin to extract data from file; files saved as chain.gz",
      "TwoBitFile", "rtracklayer::TwoBitFile(); requires rtracklayer",
      "GFFFile",
      "rtracklayer::import(); require rtracklayer and GenomeInfoDB; after import converts to GRanges object",
      "GFF3File", "rtracklayer::import(); require rtracklayer", 
      "BigWig", "rtracklayer::BigWigFile(); require rtracklayer",
      "dbSNPVCFFile",
      "VariantAnnotation::VcfFile(); require VariantAnnotation; files saved as vcf.gz and vcf.gz.tbi",
      "SQLiteFile", "AnnotationDbi::loadDb(); files saved as sqlite",
      "GRASP", "dbFileConnect()",
      "Zip", "unzip(); returns file path to files",
      "ChEA", "unzip(); returns data.frame from reading chea-background.csv",
      "BioPax", "get(load()); require rBiopaxParser",
      "Pazar",
      "read.delim(); require GenomicRanges; reads specific columns from file and coverts to GRanges object",
      "CSVtoGranges",
      "read.csv(); require GenomicRanges; coverts data.frame to GRanges object",
      "ExpressionSet", "get(load()); require Biobase",
      "GDS", "gdsfmt::openfn.gds(); require gdsfmt",
      "H5File", "require rhdf5; resource downloaded but not loaded; returns file path",
      "FilePath", "resource downloaded but not loaded; returns file path",
      "BEDFile",
      "rtracklayer::import(rtracklayer::BEDFile()); require rtracklayer; converts to GRanges object",
      "UCSCBroadPeak", "rtracklayer::import(rtracklayer::BEDFile()); require rtracklayer; converts to GRanges object",
      "UCSCNarrowPeak", "rtracklayer::import(rtracklayer::BEDFile()); require rtracklayer; converts to GRanges object",
      "UCSCBEDRnaElements", "rtracklayer::import(rtracklayer::BEDFile()); require rtracklayer; converts to GRanges object",
      "UCSCGappedPeak", "rtracklayer::import(rtracklayer::BEDFile()); require rtracklayer; converts to GRanges object",
      "EpiMetadata", "read.delim()",
      "EpiExpressionText", "read.table(); converts to SummarizedExperiment object",
      "EpichmmModels",
      "rtracklayer::import(); calls additional helper AnnotationHub:::.mapAbbr2FullName and then converts to GRange object; file assumed to be bed file format",
      "EpigenomeRoadmapFile", "rtracklayer::import(); converts to GRange object; file assumed to be bed file format",
      "EpigenomeRoadmapNarrowAllPeaks", "rtracklayer::import(rtracklayer::BEDFile()); require rtracklayer; converts to GRanges object",
      "EpigenomeRoadmapNarrowFDR", "rtracklayer::import(rtracklayer::BEDFile()); require rtracklayer; converts to GRanges object",
      "EnsDb", "ensembldb::EnsDb(); require ensembldb",
      "mzRpwiz", "mzR::openMSfile(); require mzR",
      "mzRident", "mzR::openIDfile(); require mzR",
      "MSnSet", "get(load()); require MSnbase",
      "AAStringSet", "Biostrings::readAAStringSet(); require Biostrings",
      "CompDb", "CompoundDb::Compdb(); requires CompoundDb",
      "kerasHDF5Model", "keras::load_model_hdf5(); requires keras",
      "kerasHDF5ModelWeights", "keras::load_model_weights_hdf5(); requires keras"
      ),  ncol=2, byrow=TRUE)
}
