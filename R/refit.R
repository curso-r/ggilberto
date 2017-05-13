#' Refit a model
#'
#' Refits a model changing the response variable
#' @param model a model object - currently working for lm and glm
#' @param y new response variable
#'
refit <- function(model, y){
  x <- model.matrix(model) %>% as.data.frame()
  x$`.y` <- y
  update(model, .y ~ ., data = x)
}


