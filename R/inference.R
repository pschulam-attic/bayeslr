#' Observe data and update posteriors
#'
#' Update beliefs about model parameters after observing data.
#'
#' @export
#' 
observe <- function(...) UesMethod("observe")

observe.bayeslr <- function(blr, X, y) {
    stopifnot(is.matrix(X))
    stopifnot(is.numeric(y))
    
    p <- ncol(X)

    if (p != dim(blr)) {
        stop("Dimension of X and bayeslr object do not match.")
    }
    
    n <- nrow(X)

    if (n != length(y)) {
        stop("Number inputs nrow(X) and observations length(y) do not match.")
    }

    if (!bayeslr_has_evidence(blr)) {
        blr$prior <- blr
    }

    blr <- bayeslr_append_evidence(blr, X, y)

    ## Update the expected value of 'w'.

    ## Update the covariance of 'w'.

    ## Update inverse gamma parameters for 'sigsq'.
    
}
