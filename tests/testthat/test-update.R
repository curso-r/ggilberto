context("Refit for lm")

test_that("refit for lm", {

  x <- 1:100
  y1 <- 2*x
  y2 <- 3*x

  model <- lm(y1 ~ x)
  model_new <- lm(y2 ~ x)
  model_upd <- refit(model, y2)

  expect_equal(model_new$coefficients, model_upd$coefficients)

})

test_that("refit for lm with no intercept", {

  x <- 1:100
  y1 <- 2*x
  y2 <- 3*x

  model <- lm(y1 ~ 0 + x)
  model_new <- lm(y2 ~ 0 + x)
  model_upd <- refit(model, y2)

  expect_equal(model_new$coefficients, model_upd$coefficients)

})

test_that("refit for lm with expressions inside formula", {

  x <- 1:100
  y1 <- 2*x
  y2 <- 3*x

  model <- lm(log(y1) ~ 0 + x)
  model_new <- lm(log(y2) ~ 0 + x)
  model_upd <- refit(model, log(y2))

  expect_equal(model_new$coefficients, model_upd$coefficients)

  model <- lm(log(y1) ~ 0 + log(x))
  model_new <- lm(log(y2) ~ 0 + log(x))
  model_upd <- refit(model, log(y2))

  expect_equal(model_new$coefficients, model_upd$coefficients)
})

context("Refit for glm")

test_that("refit for glm", {

  x <- 1:100
  y1 <- 2*x
  y2 <- 3*x

  model <- glm(y1 ~ x)
  model_new <- glm(y2 ~ x)
  model_upd <- refit(model, y2)

  expect_equal(model_new$coefficients, model_upd$coefficients)

})

test_that("refit for glm with no intercept", {

  x <- 1:100
  y1 <- 2*x
  y2 <- 3*x

  model <- glm(y1 ~ 0 + x)
  model_new <- glm(y2 ~ 0 + x)
  model_upd <- refit(model, y2)

  expect_equal(model_new$coefficients, model_upd$coefficients)

})

test_that("refit for glm with expressions inside formula", {

  x <- 1:100
  y1 <- 2*x
  y2 <- 3*x

  model <- glm(log(y1) ~ 0 + x)
  model_new <- glm(log(y2) ~ 0 + x)
  model_upd <- refit(model, log(y2))

  expect_equal(model_new$coefficients, model_upd$coefficients)

  model <- glm(log(y1) ~ 0 + log(x))
  model_new <- glm(log(y2) ~ 0 + log(x))
  model_upd <- refit(model, log(y2))

  expect_equal(model_new$coefficients, model_upd$coefficients)
})
