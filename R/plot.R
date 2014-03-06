bayeslr_plot_coef <- function(blr) {
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

bayeslr_plot_func <- function(blr, from, to, feature_func) {
    n <- 100
    W <- samples(blr, n)

    x <- seq(from, to, length.out = 100)
    X <- feature_func(x)
    Y <- X %*% t(W)

    p <- ggplot() + theme_bw()

    for (ix in seq(ncol(Y))) {
        dat <- data.frame(x = x, y = Y[, ix])
        p <- p + geom_line(aes(x, y), data = dat, alpha = 0.5)
    }

    print(p)
    invisible(p)
}
