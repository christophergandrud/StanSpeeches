#' Create data lists for Stan from a base list and model covariates
#'
#' @param base a list with the shared elements.
#' @param df a data frame where the \code{vars} can be found.
#' @param vars a character vector of variable names to include in the model.
#'
#' @export

stan_lister <- function(base, df, vars)
{
    base$K <- length(covars_all)
    base$X <- df[, covars_all] %>% as.matrix
    return(base)
}

#' Helper function for waic
#' @noRd

colVars <- function(a)
{
    n <- dim(a)[[1]]
    c <- dim(a)[[2]]

    return(.colMeans(((a - matrix(.colMeans(a, n, c),
            nrow = n, ncol = c, byrow = TRUE)) ^ 2), n, c) * n / (n - 1))
}

#' Internal for combining vectors of different lengths.
#' @noRd

cbind.fill<-function(...){
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}
