.ask <- function(txt, values) {
    txt <- sprintf("%s [%s] ", txt, paste(values, collapse="/"))
    repeat {
        ans <- tolower(substr(readline(txt), 1, 1))
        if (ans %in% values)
            break
    }
    ans
}
