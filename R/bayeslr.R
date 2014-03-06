#' Functions for fitting and using Bayesian linear regression models.
#'
#' @import ggplot2
#' @importFrom mvtnorm rmvnorm
#' @docType package
#' @name bayeslr
NULL

#' Create a new 'bayeslr' object
#'
#' 'bayeslr' wraps up the model hyperparameters into a single object
#' that can later be used for inference.
#'
#' @export
#' 
bayeslr <- function(w_mean, w_cov, sigsq) {
    w_mean <- as.double(w_mean)
    d <- length(w_mean)
    
    w_cov <- to_covmat(w_cov, d)
    
    obj <- structure(list(), class = "bayeslr")
    obj$w_mean <- w_mean
    obj$w_cov <- w_cov
    obj$sigsq <- sigsq
    obj
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

#' Plot bayeslr object
#'
#' @export
#' 
plot.bayeslr <- function(blr, type = c("coef", "func"), ...) {
    switch(
        type[1],
        coef = bayeslr_plot_coef(blr),
        func = bayeslr_plot_func(blr, ...)
        )
}
