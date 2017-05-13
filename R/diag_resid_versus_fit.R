#' Fitted values against residuals
#'
#' Plots residuals versus fitted values of a generalized linear model.
#' @param fit a model object - currently working for glm
#'
#' @export
diag_resid_versus_fit <- function(fit){
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)

  if(fit.model$family$family == "Gamma"){
    fi <- MASS::gamma.shape(fit.model)$alpha
    coeficiente <- sqrt(fi/(1-h))
  } else {
    coeficiente <- sqrt(1-h)
  }

  td <- resid(fit.model,type="deviance")*coeficiente
  a <- max(td)
  b <- min(td)

  data_frame(fitted_values = fitted(fit.model), deviance_component = td) %>%
    ggplot(aes(x = fitted_values, y = deviance_component)) +
    geom_point() +
    geom_hline(yintercept = 2, linetype = 2) +
    geom_hline(yintercept = -2, linetype = 2) +
    theme_bw(15) +
    scale_y_continuous(limits = c(b-1,a+1)) +
    labs(x = 'Valor Ajustado', y = 'Resíduo Componente do Desvio')
}
