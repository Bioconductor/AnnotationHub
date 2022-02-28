ah = AnnotationHub()

test_open <- function() {
    ## valid connection, including on second try
    checkTrue(RSQLite::dbIsValid(dbconn(AnnotationHub())))
    checkTrue(RSQLite::dbIsValid(dbconn(AnnotationHub())))
}

test_constructor <- function(){
    checkIdentical(slotNames(ah), slotNames("Hub"))
}

test_query <- function() {
    q1 <- query(ah, c("GTF", "Ensembl", "Homo sapiens"))
    checkTrue("AH7558" %in% names(q1))
    nm <- c("title", "dataprovider", "species", "taxonomyid", "genome",
        "description", "tags", "rdataclass", "sourceurl", "sourcetype")
    checkTrue(all(nm %in% names(mcols(q1))))
}

test_NA_subscript <- function() {
    checkIdentical(0L, length(ah[NA]))
    checkIdentical(0L, length(ah[NA_character_]))
    checkIdentical(0L, length(ah[NA_integer_]))
}

test_as.list_and_c <- function() {
    cc <- selectMethod("c", "AnnotationHub")
    checkIdentical(ah[1:5], do.call("cc", as.list(ah[1:5])))
    checkIdentical(ah[1:5], c(ah[1:2], ah[3:5]))
    checkIdentical(ah[FALSE], c(ah[FALSE], ah[FALSE]))
    checkIdentical(ah[1:5], c(ah[FALSE], ah[1:5]))
    checkIdentical(ah[1:5], c(ah[1:4], ah[3:5])) # unique() ids
}

test_subsethub <- function(){

    ah2 <- ah
    ah2 <- AnnotationHub:::.subsethub(ah2)
    checkTrue(length(ah) > length(ah2))
    rids <- names(ah2@.db_uid)
    rnames <- bfcinfo(AnnotationHub:::.get_cache(ah2))$rname
    vls <- unlist(lapply(rids,
                         function(x, rnames){
                             rnames[which(startsWith(rnames, x))]},
                         rnames=rnames))
    cachepath <- vapply(vls,
                        function(x){
                            temp <- strsplit(x, split=" : ");
                            temp[[1]][length(temp[[1]])] },
                        character(1),USE.NAMES=FALSE)
    locFiles <- vapply(dir(hubCache(ah2)),
                       function(x){
                           temp <- strsplit(x, split="_");
                           temp[[1]][length(temp[[1]])] },
                       character(1), USE.NAMES=FALSE)


    checkTrue(all(cachepath %in% locFiles))
}


test_accessors_setters<- function(){
    checkIdentical(getAnnotationHubOption("URL"),
                   hubUrl(ah))

    checkIdentical(getAnnotationHubOption("CACHE"),
                   hubCache(ah))

    dates <- possibleDates(ah)
    checkIdentical(dates[length(dates)],
                   hubDate(ah))
    checkIdentical(hubDate(ah), snapshotDate(ah))
    orig.date <- snapshotDate(ah)
    len <- length(ah)
    snapshotDate(ah) <- dates[1]
    checkTrue(length(ah) < len)

    checkIdentical(isLocalHub(ah), FALSE)
    len <- length(ah)
    isLocalHub(ah) <- FALSE
    checkIdentical(len, length(ah))
    isLocalHub(ah) <- TRUE
    checkTrue(isLocalHub(ah))
    checkTrue(length(ah) < len)
    isLocalHub(ah) <- FALSE
}

test_fileNames <- function(){

    tempfile <- fileName(ah)
    ah2 = AnnotationHub(localHub=TRUE)
    checkTrue(all(sort(names(AnnotationHub:::.db_uid(ah2))) %in% 
                  sort(names(which(!is.na(tempfile))))))
}

test_subset <- function(){

    checkIdentical(names(ah)[1:3], names(ah[1:3]@.db_uid))

}

# cache, cache <- NULL,  [[
