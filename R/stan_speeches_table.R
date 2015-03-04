#' Create data frame of results from stanfit speeches for tables using xtable
#'
#' @param stanfit list of stanfit objects. Names will become column labels
#' unless \code{col_labels} is specified.
#' @param model_pars character vector of parameters from the \code{stanfit}
#' object to include in the table.
#' @param pars_labels required vector of parameter labels to include in the
#' table. Must be a list with parameter labels for each element in
#' \code{stanfit} and in the same order. Identical variables must have matching
#' labels.
#' @param col_labels optional vector of column labels. If not specified then
#' names of fitted stan models in \code{stanfit} are used.
#' @param obs intiger number of observations in the model. Note: currently crude
#' implementation.
#'
#' @importFrom rstan extract
#' @importFrom dplyr %>% full_join
#' @export

stan_speeches_param_est <- function(stanfit, model_pars = c('beta', 'alpha'),
                                    pars_labels, col_labels, obs)
{
    for (i in 1:length(stanfit)) {
        sims <- rstan::extract(stanfit[[i]], pars = model_pars) %>%
                    as.data.frame

        unnamed <- est_1(sims = sims) %>% as.data.frame(stringsAsFactors = F)
        unnamed <- rbind(unnamed, obs)

        unnamed <- sapply(1:ncol(unnamed), function(x)
            c(unnamed[, x],
              as.vector(round(waic(stanfit[[i]])$waic[1],
                              digits = 2))))
        pars_labels_temp <- pars_labels[[i]]
        names <- rbind(pars_labels_temp, sprintf('%s_ci', pars_labels_temp)) %>%
                        c
        labels <- c(names, 'Obs.', 'WAIC')
        unnamed <- cbind(labels, unnamed)

        if (i == 1) {
            combined <- unnamed %>% data.frame
            names(combined) <- c('labels', 'm1')
        }
        else if (i > 1) {
            temp <- unnamed %>% data.frame
            names(temp) <- c('labels', sprintf('m%s', i))
            combined <- suppressWarnings(
                            full_join(combined, temp, by = 'labels', copy = T))
        }
    }
    combined <- combined %>% as.data.frame

    if (missing(col_labels)) col_labels <- c('Parameters',
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
