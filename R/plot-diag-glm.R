gg_leverage <- function(df) {
  UseMethod('gg_leverage')
}

gg_link <- function(df) {
  UseMethod('gg_link')
}

gg_resid_vs_fit <- function(df) {
  df %>%
    ggplot(aes(x = .fitted, y = .std.resid)) +
    geom_point() +
    geom_hline(yintercept = 2, linetype = 2) +
    geom_hline(yintercept = -2, linetype = 2) +
    theme_bw(15) +
    scale_y_continuous(limits = range(df$.std.resid) + c(-1, 1)) +
    labs(x = 'Fitted values', y = 'Deviance residuals')
}

gg_leverage.diag_lm <- function(df) {
  p <- stringr::str_which(names(df), '\\.fitted') - 3L
  df %>%
    ggplot(aes(x = seq_along(.rownames), y = .hat)) +
    geom_point() +
    theme_bw(15) +
    geom_hline(2 * p / nrow(df), colour = 'blue') +
    labs(x = 'Index', y = 'Leverage')
}

gg_leverage.diag_glm <- function(df) {
  df %>%
    ggplot(aes(x = .fitted, y = .hat)) +
    geom_point() +
    theme_bw(15) +
    labs(x = 'Fitted values', y = 'Leverage')
}

gg_cook <- function(df) {
  df %>%
    ggplot(aes(x = seq_along(.rownames), y = .cooksd)) +
    geom_point() +
    theme_bw(15) +
    labs(x = 'Index', y = "Cook's distance")
}

gg_link.diag_glm <- function(df) {
  df %>%
    ggplot(aes(x = .eta, y = .z)) +
    geom_point() +
    theme_bw(15) +
    labs(x = 'Linear predictor', y = "Z variable") +
    geom_smooth(se = TRUE, alpha = .2)
}

#' Plot diag
#'
#' @param x object
#' @param graphic -
#' @param ... other
#'
#' @family glm diagnostic functions
#'
#' @import ggplot2
#'
#' @export
plot.diag_glm <- function(x, graphic = 'residuals', ...) {
  # graphic <- list(...)$graphic
  if (length(graphic) > 1) stop ("I won't make more than one graph for you.")
  if (graphic == 'link' && 'diag_lm' %in% x) stop('There are no link functions in linear models.')
  if (graphic == 'residuals') p <- gg_resid_vs_fit(x)
  if (graphic == 'leverage') p <- gg_leverage(x)
  if (graphic == 'cook') p <- gg_cook(x)
  if (graphic == 'link') p <- gg_link(x)
  print(p)
}

