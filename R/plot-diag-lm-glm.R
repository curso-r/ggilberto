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
    geom_hline(yintercept = 2 * p / nrow(df), colour = 'blue') +
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
    geom_smooth(se = TRUE, alpha = .2, method = "loess")
}

#' Create a specific diagnostic graphic for glm fit
#'
#' \code{ggplot} is used to plot a specific diagnostic graphic. The choices are:
#' residuals plot, leverage mesuare plot, Cook's distance plot and z variable
#' plot (link function plot). See \emph{Details} section for more information.
#'
#' The graph created will depend on the class of the \code{model} object and
#' choice of the \code{graphic} argument. For \code{diag_glm} class, the option
#' \code{"residuals"} creates a residuals vs fitted values plot, the option
#' \code{"cook"} creates a Cook's distance vs index plot, the option
#' \code{"leverage"} creats a leverage vs fitted values plot, and the option
#' \code{"link"} returns a z variable vs linear predictor plot.
#'
#' For more information about the measures used to create the plots, see the
#' \code{\link{diag_data}} function documentation.
#'
#' @param x a tibble of class \code{diag_glm}.
#' @param graphic a string specifying the graphic to be plotted. The available
#'   choices are 'residuals' (default), 'leverage', 'cook' and 'link'.
#' @param ... futher arguments.
#'
#' @examples
#'
#' fit_glm <- glm(mpg ~ qsec, family = Gamma(link = "log"), data = mtcars)
#' diag_glm <- diag_data(fit_glm)
#'
#' plot(diag_glm)
#' plot(diag_glm, graphic = "leverage")
#' plot(diag_glm, graphic = "cook")
#' plot(diag_glm, graphic = "link")
#'
#' @family glm diagnostic functions
#' @method plot diag_glm
#' @import ggplot2
#' @export
plot.diag_glm <- function(x, graphic = 'residuals', ...) {
  # graphic <- list(...)$graphic

  if (length(graphic) > 1) stop ("I won't make more than one graph for you.")

  if (graphic == 'residuals') p <- gg_resid_vs_fit(x)
  if (graphic == 'leverage') p <- gg_leverage(x)
  if (graphic == 'cook') p <- gg_cook(x)
  if (graphic == 'link') p <- gg_link(x)
  print(p)
}

#'Create a specific diagnostic graphic for lm fits
#'
#'\code{ggplot} is used to plot a specific diagnostic graphic. The choices are:
#'residuals plot, leverage mesuare plot, Cook's distance plot and z variable
#'plot (link function plot). See \emph{Details} for more information.
#'
#'The graph created will depend on the class of the \code{model} object and
#'choice of the \code{graphic} argument. For \code{diag_lm} class, the option
#'\code{"residuals"} creates a residuals vs fitted values plot, the option
#'\code{"cook"} creates a Cook's distance vs index plot, and the option
#'\code{"leverage"} plots a leverage vs index plot.
#'
#'For more information about the measures used to create the plots, see the
#'\code{\link{diag_data}} function documentation.
#'
#'@param x a tibble of class \code{diag_lm}.
#'@param graphic a string specifying the graphic to be plotted. The available
#'  choices are 'residuals' (default), 'leverage', 'cook' and 'link'.
#'@param ... futher arguments.
#'
#' @examples
#'
#' #' fit_glm <- glm(mpg ~ qsec, family = Gamma(link = "log"), data = mtcars)
#' diag_glm <- diag_data(fit_glm)
#'
#' plot(diag_lm, graphic = "residuals")
#' plot(diag_lm, graphic = "leverage")
#' plot(diag_lm, graphic = "cook")
#'
#'@family glm diagnostic functions
#'@method plot diag_lm
#'@import ggplot2
#'@export
plot.diag_lm <- function(x, graphic = 'residuals', ...) {
  # graphic <- list(...)$graphic

  if (length(graphic) > 1) stop ("I won't make more than one graph for you.")

  if (graphic == "link") {
    stop('There are no link functions in linear models.')
  }

  if (graphic == 'residuals') p <- gg_resid_vs_fit(x)
  if (graphic == 'leverage') p <- gg_leverage(x)
  if (graphic == 'cook') p <- gg_cook(x)
  print(p)
}
