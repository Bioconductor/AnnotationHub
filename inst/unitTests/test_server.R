## a set of methods to make sure that our server is live and the that
## current URLs are all valid.

## 1st get the string for the base server
server <- AnnotationHub:::.getServer() ## Can I test this???
## string to get the curPath
curPath <- paste0(AnnotationHub:::.getServer() ,"/cgi-bin/R")
x <- AnnotationHub()


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
    res <- .getHeader(url=server)
    checkTrue(length(grep("200 OK", res[1])) > 0)
}


## is curpath pointing to nowhere? (this is expected for this one)
test_basePath <- function(){
    ## Now test that the base path has a header.
    res <- .getHeader(url=curPath)
    checkTrue(length(grep("400", res[1])) > 0)
}


## does the base serve path work?
test_servePath <- function(){
    ## http://wilson2.fhcrc.org/cgi-bin/R/serve?path=
    baseServe <- AnnotationHub:::.getBaseServe(x)
    res <- .getHeader(url=baseServe)
    checkTrue(length(grep("200 OK", res[1])) > 0)    
}


## does the path serve mechanism work?
## This one should be able to DL...
test_servePathFuntionality <- function(){
    baseServe <- AnnotationHub:::.getBaseServe(x)
    ## AND THEN we have to DL something specific:
    url <- paste(baseServe, x@paths[1], sep="/")
    res <- RCurl:::getBinaryURL(url)
    ## res is a bin, but we ONLY want to check that we can get it so:
    checkTrue(length(res) > 0) ## there should be something here.
}


## does keytypes link work?
test_keytypesFunctionality <- function(){
    res <- AnnotationHub:::.keytypes()
    checkTrue(length(res) > 0) ## contents may vary.  Are there some?
}


## does keys link work?
test_keysFunctionality <- function(){
    res <- AnnotationHub:::.keys(x,keytype="Type")
    checkTrue(length(res) > 0) ## at least one key exists?
}


## does a query point somewhere meaningful?
test_queryPath <- function(){
    paste0(x@curPath,"/query?")
}

## does query engine work?
## ALSO test the .validFilterValues() separately from this as it's complex.
## but this test is JUST for whether the server is serving up answers

test_queryPathResults <- function(){ 
    url <- "http://wilson2.fhcrc.org/cgi-bin/R/query?Organism=9606"
    res <- RCurl:::getURLContent(url)
    emptyVal <- "[]\n"
    checkTrue(res != emptyVal) ## should not just be the empty braces.
    ## and should actually be more than just 1000 characters for Human.
    checkTrue(nchar(res) > 1000)  
}


