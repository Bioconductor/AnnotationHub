.create_cache <-
    function(.class, url, cache, proxy, localHub=FALSE)
{
    hub_bfc <- BiocFileCache(cache=cache)
    db_path <- .updateHubDB(hub_bfc, .class, url, proxy, localHub)
    .db_is_valid(db_path)
    db_path
}

removeCache <- function(x){
    bfc <- .get_cache(x)
    removebfc(bfc)

}

.get_cache <-
    function(hub)
{
    BiocFileCache(cache=hubCache(hub))
}


.named_cache_path <- function(x)
{
    stopifnot(is(x, "Hub"))
    path <- .datapathIds(x)
    path
}


.cache_internal <- function(x, proxy, max.downloads,
                            force, verbose)
{
    cachepath <- .named_cache_path(x)
    need <- .cache_download_ok(x, cachepath, max.downloads, force, verbose)

    ok <- .hub_resource(x, as.character(cachepath)[need],
        cachepath[need], proxy=proxy, verbose=verbose
    )

    if (!all(ok))
        stop(sum(!ok), " resources failed to download", call. = FALSE)

    rnames <- paste(names(cachepath), cachepath, sep=" : ")
    bfc <- .get_cache(x)
    tryCatch({
        localFiles <- unname(bfcrpath(bfc, rnames=rnames))
    }, error=function(err){
        stop("Corrupt Cache: resource id",
             "\n  See vignette section on corrupt cache",
             "\n  cache: ", bfccache(bfc),
             "\n  reason: ", conditionMessage(err),
             call.=FALSE)
    })
    names(localFiles) <- rnames

    if (verbose){
        message(paste0(
            c(
                "loading from cache ", sQuote(head(rnames)),
                if (length(rnames) > 6) "..."
                ),
            collapse="\n    "
            ))
    }
    localFiles
}

.cache_download_ok <- function(x, cachepath, max.downloads, force, verbose)
{
    if (force){
        need <- rep(TRUE, length(cachepath))
    } else {
        bfc <- .get_cache(x)
        need <- .updateEntry(bfc, cachepath)
    }
    n <- sum(need)

    if (n > max.downloads) {
        ans <- "n"
        if (interactive())
            ans <- .ask(sprintf("download %d resources?", n), c("y", "n"))
        if (ans == "n") {
            txt <- sprintf(
                "resources needed (%d) exceeds max.downloads (%d)",
                n, max.downloads
            )
            stop(txt, call. = FALSE)
        }
    } else {
        if (verbose) message("downloading ", n, " resources")
    }

    need
}

.updateEntry <- function(bfc, cachepath)
{

    locFiles <- dir(bfccache(bfc))
    locFiles <- locFiles[!(endsWith(locFiles, ".sqlite") |
                           endsWith(locFiles, ".sqlite3")|
                           endsWith(locFiles, "_index.rds"))]

    baseFileName <-   suppressWarnings(as.numeric(vapply(locFiles,
                          FUN=function(x){
                              vl <- strsplit(x,split="_")
                              vl[[1]][length(vl[[1]])]
                          }, FUN.VALUE=character(1), USE.NAMES=FALSE)))
    if(any(is.na(baseFileName))){
        dx <- is.na(baseFileName)
        baseFileName <- baseFileName[!dx]
        locFiles <- locFiles[!dx]
    }

    locFiles = setNames(locFiles, baseFileName)
    if (any(duplicated(baseFileName))){
        files <- locFiles[names(locFiles) %in% baseFileName[duplicated(baseFileName)]]
        stop("Corrupt Cache: resource path",
             "\n  See vignette section on corrupt cache",
             "\n  cache: ", bfccache(bfc),
             "\n  potential duplicate files: ",
             "\n    ", paste0(files[order(names(files))], collapse="\n    "),
             call.=FALSE)
    }


    allUpdate <- rep(TRUE, length(cachepath))
    names(allUpdate) <- as.character(cachepath)
    fndFiles <-  which(cachepath %in% baseFileName)

    Update <- function(rpath, bfc){
        res <- bfcquery(bfc, rpath, fields="rpath", exact=TRUE)
        cnt <- bfccount(res)
        rid <- res %>% collect(Inf) %>% `[[`("rid")
        if (cnt > 1){
            stop("Corrupt Cache: resource path",
                 "\n  See vignette section on corrupt cache",
                 "\n  cache: ", bfccache(bfc),
                 rpath, call.=FALSE
                 )
        } else if (cnt == 0){
            TRUE
        } else {
            bfcneedsupdate(bfc, rids=rid)
        }
    }
    if (length(fndFiles) > 0){

        cachepath[fndFiles]

        update <- vapply(locFiles[match(cachepath[fndFiles], names(locFiles))],
                         FUN=Update, FUN.VALUE=logical(1), USE.NAMES=TRUE,
                         bfc=bfc)
        if (anyNA(update))
            # if no caching information use local file
            update[is.na(update)] = FALSE

        allUpdate[match(names(update), names(allUpdate))] <- update
    }
    allUpdate
}
