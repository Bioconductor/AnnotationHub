.ask <- function(txt, values) {
    txt <- sprintf("%s [%s] ", txt, paste(values, collapse="/"))
    repeat {
        ans <- tolower(substr(readline(txt), 1, 1))
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
