## This file contains code to generate man pages based on the objects
## dynamically from the metadata.

## Eventually, we would like to be able to do somethign more like this:
## hub?path ## returns help

## little object to come back from ahinfo
setClass("ahinfoList", contains="SimpleList")
## constructor:
ahinfoList <- function(list){
    if(missing(list)) stop("No list to create an ahinfoList from")
    new("ahinfoList", list)
}
## show method
setMethod(show, "ahinfoList", function(object) {
    txt <- paste(
                 " $DataProvider ",object$DataProvider,"\n",
                 "$SourceVersion ",object$SourceVersion,"\n",
                 "$Description ",object$Description,"\n",
                 "$Species ",object$Species,"\n",
                 "$Genome ",object$Genome,"\n",
                 "$BiocVersion ",
                 paste(unlist(object$BiocVersion), collapse=", "),"\n",
                 "$Tags ",paste(unlist(object$Tags), collapse=", "),"\n",
                 "$SourceUrl ",object$SourceUrl,"\n",
                 "$RDataPath ",object$RDataPath,"\n",
                 "$RDataName ",object$RDataName,"\n"
                 )
    cat(txt)
})

.getOnePathObject <- function(x, path){
    if(length(path) >1) stop("Only one resource at a time please.")
    ## 1st translate path to RDataPath style
    srcUrls <- snapshotUrls(x)
    path <- srcUrls[names(srcUrls) %in% path]
    path <- sub(paste(hubUrl(x),"/",sep=""), "", path)
    
    ## get metadata based on the hub and path
    cols <- c('BiocVersion','DataProvider','Description','Genome',
             'Tags','SourceUrl','SourceVersion','Species','RDataPath')
    m <- .metadata(snapshotUrl(),
                   filters=list(RDataPath=path),
                   cols=cols)
    
    m$RDataPath <- paste0(hubUrl(x),"/",m$RDataPath)
    m$RDataName <- names(snapshotUrls(x))[snapshotUrls(x) %in% m$RDataPath]
    ahinfoList(as(m,"SimpleList"))
}

## This function is not vectorized, because .metadata is not vectorized.
ahinfo <- function(x, path){
    sapply(path, FUN=.getOnePathObject, x=x)
}




## path = "goldenpath.hg19.encodeDCC.wgEncodeUwTfbs.wgEncodeUwTfbsMcf7CtcfStdPkRep1.narrowPeak_0.0.1.RData"; library(AnnotationHub); hub = AnnotationHub(); ahinfo(hub, path)

## debug(AnnotationHub:::ahinfo); ahinfo(hub, path)

## OTHER issue???
## debug(AnnotationHub:::.metadata)
## AnnotationHub:::.metadata(snapshotUrl(hub),filters=list(RDataPath=path), cols=c('BiocVersion','DataProvider','Description','Genome','Tags','SourceUrl','SourceVersion','Species'))

## ahinfo(hub,"ensembl.release.69.fasta.equus_caballus.dna.Equus_caballus.EquCab2.69.dna.toplevel.fa.gz")



## So currently it's now using the string that follows the $...

## I will have to translate it using sourceUrls()  UGH.





## TODO: Tags are coming back as an object that we can't parse?




