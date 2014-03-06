#' Append observations to bayeslr
#' 
bayeslr_append_evidence <- function(blr, X, y) {
    if (bayeslr_has_evidence(blr)) {
        blr$X <- X
        blr$y <- y
    } else {
        blr$X <- rbind(blr$X, X)
        blr$y <- c(blr$y, y)
    }
    blr
}

#' Check if bayeslr has had evidence appended
#' 
bayeslr_has_evidence <- function(blr) {
    (!is.null(blr$X)) & (!is.null(blr$y))
}
