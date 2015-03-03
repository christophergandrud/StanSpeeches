#' A simple wrapper function for running Stan in parallel on Mac or Linux
#' @param fit a \code{stanfit} object, can be without simulations.
#' @param data a list with the data for Stan.
#' @param iter integer number of iterations to run per chain.
#' @param pars character vector of parameters to keep.
#' @param cores integer number of cores to run on.
#'
#' @importFrom rstan stan sflist2stanfit
#' @importFrom parallel mclapply
#' @export

parallel_4 <- function(fit, data, iter = 2000,
                        pars = c('alpha', 'beta', 'a', 'log_lik'), cores = 4)
{
    sflist <-
        mclapply(1:cores, mc.cores = cores,
                 function(i) stan(fit = fit, data = data,
                                  seed = i, chains = 1,
                                  iter = iter, chain_id = i,
                                  pars = pars
                 )
        )

    # Collect in to Stan fit object
    fit <- sflist2stanfit(sflist)
    return(fit)
}
