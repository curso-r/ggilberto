#' Envelope plots of generalized linear model
#'
#' Draws the envelope plot for the residual distributions of a generalized linear model.
#'
#' @param fit a model object - currently working for glm
#' @param B number of bootstrap iterations
#'
#' @import ggplot2
#' @export
envel <- function(fit, B = 100) {
  residual_sample <- simulate(fit, nsim = B) %>%
    purrr::map_df(~refit(fit, .x) %>%
                    resid(type = 'deviance') %>%
                    sort()) %>%
    dplyr::mutate(posto = 1:n()) %>%
    tidyr::gather(sim, value, -posto) %>%
    dplyr::group_by(posto) %>%
    dplyr::summarise(residuo_teorico = mean(value),
                     q025 = quantile(value, prob = 0.025),
                     q975 = quantile(value, prob = 0.975)) %>%
    dplyr::mutate(residuo_empirico = fit %>% resid(type = 'deviance') %>% sort())

  residual_sample %>%
    dplyr::mutate(banda = (residuo_empirico > q975) | (residuo_empirico < q025)) %>%
    ggplot(aes(x = sort(residuo_teorico), y = sort(residuo_teorico),
               ymin = sort(q025), ymax = sort(q975))) +
    geom_point(aes(x = sort(residuo_teorico), y = sort(residuo_empirico),
                   color = banda)) +
    geom_ribbon(alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    theme_bw(15) +
    labs(y = 'Deviance residuals', x = 'Theoretic quantiles') +
    theme(legend.position = 'none') +
    scale_color_manual(values = c("black","red"))
}
