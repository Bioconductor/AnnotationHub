## This file contains code to generate man pages based on the objects
## dynamically from the metadata.

## Eventually, we want to be able to do it like this:
## hub?path ## returns help


## But for now we will live with this:
## AHHelp(hub, path)


## the .metadata function is pretty great for finding stuff.  For this
## example, we are just going to restrict the output to when
## filter=list(RDataPath="PATH")


## TODO: We could also easily make a helper that exposes more of the
## power of .metadata() So we could give the user access to more
## arguments. (and name it something intuitive)


ahinfo <- function(x, path, returnMeta=FALSE){
    ## 1st translate path to RDataPath style
    srcUrls <- snapshotUrls(x)
    path <- srcUrls[names(srcUrls) %in% path]
    path <- sub(paste(hubUrl(x),"/",sep=""), "", path)
    
    ## get metadata based on the hub and path
    cols <- c('BiocVersion','DataProvider','Description','Genome',
             'Tags','SourceUrl','SourceVersion','Species')
    m <- .metadata(snapshotUrl(x),
                   filters=list(RDataPath=path),
                   cols=cols)
    
    ## PROBLEM: what to do when values are missing???
    ## So for my example, I ask for 8 things and I only get 7...
    ## This is a case where the base service needs to return NAs (it
    ## currently returns no indication when something is not there)
    
    ## then output that into a dynamic help page.
    txt <- paste(
                 " From:",m$DataProvider,"\n",
                 "Version:",m$SourceVersion,"\n",
                 "Description:",m$Description,"\n",
                 "Genus and Species:",m$Species,"\n",
                 "Genome:",m$Genome,"\n",
                 "BiocVersion:",
                 paste(unlist(m$BiocVersion), collapse=", "),"\n",
                 "Tags:",paste(unlist(m$Tags), collapse=", "),"\n"
                 )
    ## returnMeta is really just for internal use...
    if(returnMeta==TRUE){
        return(m)
    }else{
        message(txt)
    }    
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




