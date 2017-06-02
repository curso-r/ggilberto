#' Create a tibble with useful diagnostic measures
#'
#' Uses broom::augment and some other useful measures
#'
#' @param model a previously fitted model of class \code{lm} or \code{glm}
#'
#' @return A tibble of class \code{diag_lm} or \code{diag_glm} (depending on the
#'   class of the \code{model} object) with the following columns:
#'
#'   \itemize{ \item \strong{.rownames}: the row labels taken from the dataset
#'   \item the variables (response and predictors) used to fit the model \item
#'   \strong{.fitted}: predicted value \item \strong{.se.fit}: standard-error
#'   \item \strong{.resid}: residuals \item \strong{.hat}: h-measure \item
#'   \strong{.sigma}: \item \strong{.cooksd}: Cook's distance \item
#'   \strong{.std.resid}: standardized residuals \item \strong{.w}: weights
#'   \item \strong{.eta}: \item \strong{.pearson}: person residuals \item
#'   \strong{.z}: z-measure }
#'
#'
#' @family glm diagnostic functions
#'
#' @examples
#' fit_lm <- lm(mpg ~ cyl, data = mtcars)
#' diag_data(fit_lm)
#'
#' fit_glm <- glm(mpg ~ cyl, family = Gamma(link = "log"), data = mtcars)
#' diag_data(fit_glm)
#'
#' @export
diag_data <- function(model) {
  w <- model$weights
  if (is.null(w)) w <- 1
  d_diag <- model %>%
    broom::augment() %>%
    dplyr::mutate(.w = 1, .eta = predict(model),
                  .pearson = resid(model, type = 'pearson'),
                  .z = .eta + .pearson / .w) %>%
    tibble::as_tibble()
  class(d_diag) <- c(ifelse('glm' %in% class(model),  'diag_glm', 'diag_lm'),
                     class(d_diag))
  d_diag
}
