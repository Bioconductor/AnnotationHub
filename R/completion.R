.longestCommonPrefix <-
    function(x)
{
    ## find longest common prefix shared by elements of character
    ## vector x
    if (length(x) > 1L) {
        for (i in seq_len(min(nchar(x))))
            if (length(unique(substring(x, 1, i))) != 1L)
                break
        x <- substring(x, 1L, i - 1L)
    }
    table(x)
}

.complete <-
    function(hubNames)
{
    ## shared prefix
    prefix0 <- names(.longestCommonPrefix(hubNames))
    ## unique extension
    compl0 <- substring(hubNames, 1L, nchar(prefix0) + 1L)
    ## shared suffix of each unique extension
    result <- lapply(split(hubNames, compl0), .longestCommonPrefix)
    ## format output w/ count info
    prefix <- unname(sapply(result, names))
    counts <- sapply(result, unname)
    idx <- counts != 1L
    prefix[idx] <- sprintf("%s ... [%d]", prefix[idx], counts[idx])
    prefix
}
