#' Create a tibble with useful diagnostic measures
#'
#' Uses broom::augment and some other useful measures
#'
#' @param model a previously fitted model of class \code{lm} or \code{glm}
#'
#' @return A tibble with the following columns:
#'
#'   - x1
#'
#'   - x2
#'
#'   - x3
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
