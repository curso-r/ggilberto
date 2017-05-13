#' Refit a model
#'
#' Refits a model changing the response variable
#' @param model a model object - currently working for lm and glm
#' @param y new response variable
#'
refit <- function(model, y){
  x <- as.data.frame(model.matrix(model))
  x$`.y` <- y
  update(model, .y ~ ., data = x)
}


