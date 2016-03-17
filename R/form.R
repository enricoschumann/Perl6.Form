form <- function(..., cat = TRUE, perl = "perl") {

    s <- list(...)
    vecs <- lengths(s) > 1L
    if (any(vecs)) {
        for (i in which(vecs)) {
            s[[i]] <- paste(as.character(s[[i]]), collapse = "\r")
        }
    }
    s <- unlist(s)
    cmd <- paste(dQuote(s), collapse = ",", sep = "")
    cmd <- paste0("use Perl6::Form; print form ", cmd, ";")
    res <- system2(perl, args = paste("-e ", shQuote(cmd)), stdout = TRUE)
    if (cat) {
        cat(paste(res, collapse ="\n"), "\n")
        invisible(res)
    } else
        res
}
