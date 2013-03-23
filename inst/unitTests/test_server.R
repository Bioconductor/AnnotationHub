## a set of methods to make sure that our server is live and the that
## current URLs are all valid.

## For current examples of how URLs should look. see README.md from the
## AnnotationHubServer package

## testObject
x <- AnnotationHub()
BiocVersion <- BiocInstaller::biocVersion()
RDataDateAdded <- as.character(possibleDates(x)[1])


## Helper for getting headers into easily grep-able format
.getHeader <- function(url){
    myOpts = RCurl:::curlOptions(header = TRUE)
    res <- RCurl:::getURI(url,.opts =myOpts)
    unlist(strsplit(res, split="\n"))
}



#########################
##       Tests:
#########################

## is base URL legit?
test_serverAvailability <- function(){
    ## Now I need to basically ping that string (plus other things) to
    ## check various things...
    res <- .getHeader(url=AnnotationHub:::.hostUrl())
    checkTrue(length(grep("200 OK", res[1])) > 0)
}


## is curpath pointing to somewhere? (a change from last time)
test_basePath <- function(){
    ## Now test that the base path has a header.
    res <- .getHeader(url=snapshotUrl(x))
    checkTrue(length(grep("200", res[1])) > 0)
}


## ## I think I no longer need to test this (things have drifted)
## ## does the base serve path work?
## test_servePath <- function(){
##     ## http://wilson2.fhcrc.org/cgi-bin/R/serve?path=
##     baseServe <- AnnotationHub:::.getBaseServe()
##     res <- .getHeader(url=baseServe)
##     checkTrue(length(grep("200 OK", res[1])) > 0)    
## }


## does the path serve mechanism work?
## This one should be able to DL...
test_servePathFuntionality <- function(){
    baseServe <- hubUrl(x)
    ## AND THEN we have to DL something specific:
    url <- paste(baseServe, snapshotPaths(x)[1], sep="/")
    res <- RCurl:::getBinaryURL(url)
    ## res is a bin, but we ONLY want to check that we can get it so:
    checkTrue(length(res) > 0) ## there should be something here.
}


## does keytypes link work?
test_keytypesFunctionality <- function(){
    res <- AnnotationHub:::.keytypes(snapshotUrl(x))
    checkTrue(length(res) > 0) ## contents may vary.  Are there some?
}


## does keys link work?
test_keysFunctionality <- function(){
    res <- AnnotationHub:::.keys(snapshotUrl(x),keytype="Tags")
    checkTrue(length(res) > 0) ## at least one key exists?
}





## does query engine work?
## ALSO test the .validFilterValues() separately from this as it's complex.
## but this test is JUST for whether the server is serving up answers
test_queryPathResults <- function(){ 
##     url <- "http://wilson2.fhcrc.org/cgi-bin/R/query?Organism=9606"
    url <- 
        sprintf("http://annotationhub.bioconductor.org/ah/%s/%s/query/TaxonomyId/9606",
        BiocVersion, RDataDateAdded)
    res <- RCurl:::getURLContent(url)
    emptyVal <- "[]\n"
    checkTrue(res != emptyVal) ## should not just be the empty braces.
    ## and should actually be more than just 1000 characters for Human.
    checkTrue(nchar(res) > 1000)  
}

## Does the URL used by metadata return values?
test_metadataResults <- function(){
    url <- 
        sprintf('http://annotationhub.bioconductor.org/ah/%s/%s/query/Species/Homo sapiens/cols/RDataPath',
        BiocVersion, RDataDateAdded)
    ## just attempts to DL this file and then call fromJSON()
    res <- AnnotationHub:::.parseJSON(url) 
    checkTrue(length(res[[1]]) > 1)
}
