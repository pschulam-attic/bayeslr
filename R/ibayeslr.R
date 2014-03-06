#' Create a new 'ibayeslr' object
#'
#' 'ibayeslr' is a Bayesian linear regression model with an improper
#' prior. This means that 'w_mean = 0', 'w_cov = Inf * diag(p)',
#' 'sigsq_a = - D / 2', and 'sigsq_b = 0'.
#'
#' The only piece of information required is the dimension 'p' (the
#' number of features).
#'
#' @export
#' 
ibayeslr <- function(p) {
    p <- as.integer(p)
    
    if (length(p) != 1) {
        stop("Dimension p should be a single integer.")
    }

    if (p < 1) {
        stop("Dimension p must be an integer greater than 0.")
    }
    
    obj <- structure(list(), class = "ibayeslr")
    obj$p <- p
    obj
}

#' @export
print.ibayeslr <- function(iblr) {
    s <- sprintf("Improper Bayesian Linear Regression Model (dim = %d)\n", dim(iblr))
    cat(s)
    invisible(iblr)    
}

#' @export
dim.ibayeslr <- function(iblr) {
    iblr$p
}
