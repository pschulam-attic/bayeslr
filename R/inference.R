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

observe.ibayeslr <- function(iblr, X, y) {
    stopifnot(is.matrix(X))
    stopifnot(is.numeric(y))
    
    p <- ncol(X)

    if (p != dim(blr)) {
        stop("Dimension of X and ibayeslr object do not match.")
    }
    
    n <- nrow(X)

    if (n != length(y)) {
        stop("Number inputs nrow(X) and observations length(y) do not match.")
    }

    iblr <- bayeslr_append_evidence(iblr, X, y)

    ##  Update uninformative prior (see page 236 of Murphy; equations 7.78-7.82)

    w_cov <- solve(crossprod(X))  # Murphy eq. 7.79

    w_mean <- w_cov %*% crossprod(X, y)  # Murphy eq. 7.78

    sigsq_a <- (n - p) / 2  # Murphy eq. 7.80

    s2 <- crossprod(y - X %*% w_mean)  # Murphy eq. 7.82

    sigsq_b <- as.double(s2) / 2  # Murphy 7.81

    ## Update ibayeslr object.

    iblr$w_mean <- w_mean
    iblr$w_cov <- w_cov
    iblr$sigsq_a <- sigsq_a
    iblr$sigsq_b <- sigsq_b
    iblr    
}

#' Predict responses for new x
#'
#' @export
#' 
predict.bayeslr <- function(blr, X) {
    if (is.vector(X)) {
        X <- matrix(X, nrow = 1)
    }

    if (ncol(X) != dim(blr)) {
        stop("Dimension of input X does not match dimension of model.")
    }

    n <- nrow(X)

    noncentrality_params <- X %*% blr$w_mean

    scale_mat <- diag(n) + X %*% tcrossprod(blr$w_cov, X)
    scale_mat <- (blr$sigsq_b / blr$sigsq_a) * scale_mat

    degr_of_freedom <- 2 * blr$sigsq_a

    y <- as.double(noncentrality_params)

    covmat <- degr_of_freedom / (degr_of_freedom - 2) * scale_mat
    y_sd <- diag(covmat)

    list(y = y, sd = y_sd)
}

#' Predict responses for new x
#'
#' @export
#' 
predict.ibayeslr <- function(blr, X) {
    if (is.vector(X)) {
        X <- matrix(X, nrow = 1)
    }

    if (ncol(X) != dim(blr)) {
        stop("Dimension of input X does not match dimension of model.")
    }

    n <- nrow(X)

    noncentrality_params <- X %*% blr$w_mean

    scale_mat <- diag(n) + X %*% tcrossprod(blr$w_cov, X)
    scale_mat <- (blr$sigsq_b / blr$sigsq_a) * scale_mat

    degr_of_freedom <- 2 * blr$sigsq_a

    y <- as.double(noncentrality_params)

    covmat <- degr_of_freedom / (degr_of_freedom - 2) * scale_mat
    y_sd <- diag(covmat)

    list(y = y, sd = y_sd)
}
