## curlPerform(URL=url, writefunction=.curl_writer, writedata=<externalptr>)
.curl_writer <- NULL

.curl_writer_open <-
    function(filename)
{
    if (!is.character(filename) || 1L != length(filename))
        stop("'filename' must be character(1)")
    dir <- dirname(filename)
    if (!file.exists(dir) || !file.info(dir)$isdir)
        stop("'dirname(filename)' does not exist or is not a directory")
    filename <- file.path(normalizePath(dir), basename(filename))
    if (file.exists(filename))
        stop("'filename' must not already exist")
    
    .Call(".writer_open", filename)
}

.curl_writer_close <- function(ext)
{
    .Call(".writer_close", ext)
}

## usage .curl_writer_download(url, filename)
.curl_writer_download <-
    function(url, filename=tempfile())
{
    ext <- .curl_writer_open(filename)
    on.exit(.curl_writer_close(ext))
    status <- curlPerform(URL=url, .opts=list(ssl.verifypeer=FALSE),
                          writefunction=.curl_writer,
                          writedata=ext, sslversion=3, followlocation=TRUE,
                          failonerror=TRUE)
    if (0L != status)
        stop("'curlPerform' failed; status: ", status)
    filename
}
