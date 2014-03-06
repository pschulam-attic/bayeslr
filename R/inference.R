#' Observe data and update posteriors
#'
#' Update beliefs about model parameters after observing data.
#'
#' @export
#' 
observe <- function(...) UseMethod("observe")

#' Sample models
#'
#' Use current beliefs to sample possible models.
#'
#' @export
#'
samples <- function(...) UseMethod("samples")

#' @export
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

    ## Standard Bayesian LM posterior updates (see page 232 of Murphy; equations 7.56-7.58).

    s2 <- blr$sigsq

    w_cov <- s2 * solve(s2 * blr$w_cov + crossprod(X))  # Murphy eq. 7.58

    w_mean <- w_cov %*% solve(blr$w_cov, blr$w_mean) + (1 / s2) * w_cov %*% crossprod(X, y)  # Murpy eq. 7.56

    ## Update bayeslr object.

    blr$w_mean <- w_mean
    blr$w_cov <- w_cov
    blr
}

#' Predict response for new x
#'
#' @export
#' 
predict.bayeslr <- function(blr, X) {
    if (is.vector(X)) {
        X <- matrix(X, nrow = 1)
    }

    p <- ncol(X)

    if (p != dim(blr)) {
        stop("Number of columns (features) in X do not match bayeslr dimension.")
    }

    y <- as.double(X %*% blr$w_mean)
    sigma <- blr$sigsq + X %*% tcrossprod(blr$w_cov, X)

    list(y = y, sd = sqrt(diag(sigma)))
}

#' @export
samples.bayeslr <- function(blr, n) {
    rmvnorm(n, mean = blr$w_mean, sigma = blr$w_cov)
}
