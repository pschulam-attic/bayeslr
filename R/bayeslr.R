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
plot.bayeslr <- function(blr) {
    w_mean <- blr$w_mean
    w_sd <- sqrt(diag(blr$w_cov))
    
    density_evals <- vector("list", dim(blr))

    for (ix in seq_along(w_mean)) {
        wm <- w_mean[ix]
        ws <- w_sd[ix]

        dfunc <- function(x) dnorm(x, wm, ws)
        lo <- wm - 5 * ws
        hi <- wm + 5 * ws

        density_evals[[ix]] <- dense_data_frame(dfunc, lo, hi, 100, param_num = ix)
    }

    p <- ggplot(do.call(rbind, density_evals))
    p <- p + geom_line(aes(x, y))
    p <- p + facet_wrap(~ param_num)
    p <- p + labs(title = "Coefficient Distributions", x = "w", y = "p(w)")
    p <- p + theme_bw()

    print(p)
    invisible(p)
}
