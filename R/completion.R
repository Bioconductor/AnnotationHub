.strsplit0 <- function(x)
    strsplit(x, "", fixed=TRUE)

.longestCommonPrefix <-
    function(x)
{
    n <- length(x)
    if (length(x) > 1L) {
        x <- sort(x)
        x1 <- .strsplit0(x[[1]])[[1L]]
        xN <- .strsplit0(x[[length(x)]])[[1L]]
        idx <- seq_len(min(length(x1), length(xN)))
        i <- which.min(c(x1[idx] == xN[idx], FALSE)) # 'FALSE' protects x1 == xN
        x <- substring(x[[1L]], 1L, i - 1L)
    }
    setNames(n, x)
}

.completion <-
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
