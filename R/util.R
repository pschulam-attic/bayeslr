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

#' Evaluate function on dense data frame
#'
#' Create data frames of dense function evaluations. Works well with
#' ggplot2. 'func' argument should be vectorized.
#'
#' 
dense_data_frame <- function(func, from, to, length.out, ...) {
    x <- seq(from, to, length.out = length.out)
    y <- func(x)
    dense_df <- data.frame(x = x, y = y)

    extra_cols <- list(...)

    for (ix in seq_along(extra_cols)) {
        label <- names(extra_cols)[ix]
        value <- extra_cols[[ix]]
        dense_df[[label]] <- value
    }

    dense_df
}
