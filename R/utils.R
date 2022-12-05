.msg <- function(
    fmt, ..., width = getOption("width"), indent = 0, exdent = 2, wrap. = TRUE
) {
    txt <- sprintf(fmt, ...)
    if (wrap.) {
        txt <- strwrap(sprintf(fmt, ...), width = width, indent = indent,
                       exdent = exdent)
        paste(txt, collapse = "\n")
    }
    else {
        txt
    }
}

.stop <- function(..., call. = FALSE) {
    stop(.msg(...), call. = call.)
}
