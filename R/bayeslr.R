#' Create a new 'bayeslr' object
#'
#' 'bayeslr' wraps up the model hyperparameters into a single object
#' that can later be used for inference.
#'
#' @export
#' 
bayeslr <- function(w_mean, w_cov, sigsq_a, sigsq_b) {
    w_mean <- as.double(w_mean)
    d <- length(w_mean)
    
    w_cov <- to_covmat(w_cov, d)
    
    sigsq_a <- as.double(sigsq_a)
    if (length(sigsq_a) != 1) stop("Length of hyperparameter sigsq_a greater than 1.")
    
    sigsq_b <- as.double(sigsq_b)
    if (length(sigsq_b) != 1) stop("Length of hyperparameter sigsq_b greater than 1.")
    
    obj <- structure(list(), class = "bayeslr")
    obj$w_mean <- w_mean
    obj$w_cov <- w_cov
    obj$sigsq_a <- sigsq_a
    obj$sigsq_b <- sigsq_b
    obj
}

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

#' Print bayeslr object
#'
#' @export
#'
print.bayeslr <- function(blr) {
    s <- sprintf("Bayesian Linear Regression Model (dim = %d)\n", dim(blr))
    cat(s)
    invisible(blr)
}

#' Number of coefficients in bayeslr model.
#'
#' @export
#'
dim.bayeslr <- function(blr) {
    length(blr$w_mean)
}