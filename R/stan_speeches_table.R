#' Create data frame of results from stanfit speeches for tables using xtable
#'
#' @param stanfit list of stanfit objects. Names will become column labels
#' unless \code{col_labels} is specified.
#' @param model_pars character vector of parameters from the \code{stanfit}
#' object to include in the table.
#' @param pars_labels optional vector of parameter labels to include in the
#' table. Must be as long as the parameter list from the longest \code{stanfit}
#' object and in the same order.
#' @param col_labels optional vector of column labels.
#' @param obs intiger number of observations in the model. Note: currently crude
#' implementation.
#'
#' @importFrom rstan extract
#' @importFrom dplyr %>%
#' @export

stan_speeches_param_est <- function(stanfit, model_pars = c('beta', 'alpha'),
                                    pars_labels, col_labels, obs)
{
    combined <- data.frame()
    for (i in 1:length(stanfit)) {
        sims <- rstan::extract(stanfit[[i]], pars = model_pars) %>%
                    as.data.frame

        temp <- est_1(sims = sims) %>% as.data.frame

        combined <- StanSpeeches:::cbind.fill(combined, temp)

        combined <- sapply(1:ncol(combined), function(x)
                    c(combined[, x], '', obs))

        combined <- sapply(1:ncol(combined), function(x)
                        c(combined[, x],
                          as.vector(round(waic(stanfit[[i]])$waic[1],
                          digits = 2))))
    }

    if (missing(pars_labels)) pars_labels <- names(sims)
    names <- rbind(pars_labels, rep('', length(pars_labels))) %>% c
    labels <- c(names, '', 'Obs.', 'WAIC')
    combined <- cbind(labels, combined)

    combined <- combined %>% as.data.frame

    if (missing(col_labels)) col_labels <- c('', 
                                             names(stanfit)[1:length(stanfit)])
    names(combined) <- col_labels

    return(combined)
}

#' Internal function for finding individual model runs
#' @noRd

est_1 <- function(sims) {
    medians <- sapply(1:ncol(sims), function(x) median(sims[, x]) %>%
                          round(digits = 2))
    lower_95 <- sapply(1:ncol(sims), function(x)
                    quantile(sims[, x], probs = 0.025) %>%
                    round(digits = 2))
    upper_95 <- sapply(1:ncol(sims), function(x)
                    quantile(sims[, x], probs = 0.975) %>%
                    round(digits = 2))

    cred_interval <- sprintf('(%s, %s)', lower_95, upper_95)

    comb <- rbind(medians, cred_interval) %>% c
    return(comb)
}
