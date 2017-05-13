refit_beta <- function(model, sample, rt){
  refit(model, sample) %>%
    resid(type = rt) %>%
    sort()
}

#' Envelope plots of generalized linear model
#'
#' Draws the envelope plot for the residual distributions of a generalized linear model.
#' @param fit a model object - currently working for glm
#' @param B number of bootstrap iterations
#' @param resid_type type of the residual to be used
#'
#' @export
envel_glm <- function(fit, B = 100, resid_type = 'response'){

  residual_sample <- simulate(fit, nsim = B) %>%
    map_df(~refit_beta(model = fit, sample = .x, rt = 'response')) %>%
    mutate(posto = 1:n()) %>%
    gather(sim, value, -posto) %>%
    group_by(posto) %>%
    summarise(residuo_teorico = mean(value),
              q025 = quantile(value, prob = 0.025),
              q975 = quantile(value, prob = 0.975)) %>%
    mutate(residuo_empirico = fit %>% resid(type = resid_type) %>% sort)

  residual_sample %>%
    mutate(o_sonho_acabou = (residuo_empirico > q975)|(residuo_empirico < q025)) %>%
    ggplot(aes(x = sort(residuo_teorico), y = sort(residuo_teorico),
               ymin = sort(q025), ymax = sort(q975))) +
    geom_point(aes(x = sort(residuo_teorico), y = sort(residuo_empirico),
                   color = o_sonho_acabou)) +
    geom_ribbon(alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    theme_bw(15) +
    labs(y = 'Resíduos Empíricos', x = 'Resíduos Teóricos') +
    theme(legend.position = 'none') +
    scale_color_manual(values = c("black","red"))
}
