#' Observe data and update posteriors
#'
#' Update beliefs about model parameters after observing data.
#'
#' @export
#' 
observe <- function(...) UseMethod("observe")

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

    ## Normal normal-inverse-gamma posterior (see page 235 of Murphy; equations 7.69-7.73)

    w_cov <- solve(solve(blr$w_cov) + crossprod(X))  # Murphy eq. 7.71

    w_mean <- w_cov %*% (solve(blr$w_cov, blr$w_mean) +  t(X) %*% y)  # Murphy eq. 7.70

    sigsq_a <- blr$sigsq_a + n / 2  # Murphy eq. 7.72

    q1 <- crossprod(w_mean, solve(w_cov, w_mean))
    q1 <- as.double(q1)
    q2 <- crossprod(blr$w_mean, solve(blr$w_cov, blr$w_mean))
    q2 <- as.double(q2)
    sigsq_b <- blr$sigsq_b + 0.5 * (q1 + as.double(crossprod(y)) + q2)  # Murphy eq. 7.73

    ## Update bayeslr object.

    blr$w_mean <- w_mean
    blr$w_cov <- w_cov
    blr$sigsq_a <- sigsq_a
    blr$sigsq_b <- sigsq_b
    blr
}
