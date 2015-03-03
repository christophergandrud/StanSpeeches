#' Predict probabilities for multiple scenarios
#'
#' @param stanfit a \code{stanfit} object.
#' @param data the data used to create the \code{stanfit} object.
#' @param fitted_values a matrix of fitted values for the \code{betas}.
#' Each row should be a different scenario.
#' @param a_num integer of the \code{a} speaker level to fit for.
#' @param model_pars character vector of model parameters to use in in the
#' predicted probabilities. Note: should include parameters beginning with
#' "\code{beta}" that correspond to the fitted values,
#'
#'
#' @export

predict_prob <- function(stanfit, data, fitted_values, a_num, betas,
                              model_pars = c('beta', 'alpha', 'a'))
{
    pred_prob_out <- data.frame()
    for (i in 1:nrow(fitted_values)) {
        temp <- fitted_values[i, ]
        temp_predict <- predict_1(stanfit = fit_housing,
                                  data = speeches_data_housing,
                                  fitted_values = temp, a_num = 3,
                                  betas = betas)

        pred_prob_out <- rbind(pred_prob_out, temp_predict)
    }
    return(pred_prob_out)
}

#' Predict probability for one set of fitted values
#'
#' @importFrom rstan extract
#' @importFrom dplyr %>%
#' @importFrom boot inv.logit
#' @importFrom SPIn SPIn
#' @noRd

predict_1 <- function(stanfit, data, fitted_values, a_num,
                      model_pars = c('beta', 'alpha', 'a'))
{
    sims <- rstan::extract(stanfit, pars = model_pars) %>% as.data.frame

    betas <- grep('^beta.*', names(sims))

    betas_coef <- sims[, betas]
    alpha <- sims[, 'alpha']
    a <- sims[, sprintf('a.%s', a_num)]

    fitted_full <- t(replicate(nrow(sims), fitted_values))

    betas_x <- sapply(1:ncol(betas_coef),
                      function(x) betas_coef[, x] * fitted_full[, x])

    raw <- sapply(1:nrow(betas_x),
                  function(x) sum(betas_x[x, ]) + alpha[x] + a[x])

    pred_prob <- boot::inv.logit(raw)

    spin_95 <- SPIn(pred_prob, conf = 0.95)
    spin_50 <- SPIn(pred_prob, conf = 0.5)
    spin_lower95 <-

    pred_prob_summary <- data.frame(
        lower_95 = spin_95$spin[1],
        lower_50 = spin_50$spin[1],
        medians = median(pred_prob),
        upper_50 = spin_50$spin[2],
        upper_95 = spin_95$spin[2]
    )
    return(pred_prob_summary)
}
