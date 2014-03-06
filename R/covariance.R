#' Convert argument to a covariance matrix.
#'
to_covmat <- function(...) UseMethod("to_covmat")

#' Convert numeric vector to covariance matrix.
#'
to_covmat.numeric <- function(x, d) {
    if (length(x) != d) {
        stop("length(x) of diagonal vector not equal to dimension d.")
    }

    if (d == 1) {
        covmat <- as.matrix(x)
    } else {
        covmat <- diag(x)
    }

    covmat
}

#' Convert matrix to covariance matrix.
#'
to_covmat.matrix <- function(X, d) {
    n <- nrow(X)
    m <- ncol(X)

    if (n != m | n != d) {
        stop("Dimension of X is different from requested dimension d.")
    }

    X
}
