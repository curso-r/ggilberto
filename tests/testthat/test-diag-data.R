context("diag-data for lm")

test_that("works with lm", {
  m <- lm(mpg ~ disp + cyl, data = mtcars)
  expect_s3_class(diag_data(m), c('diag_lm', 'data.frame'))
})

context("diag-data for glm")

test_that("works with glm", {
  m <- glm(mpg ~ disp + cyl, data = mtcars, family = Gamma)
  expect_s3_class(diag_data(m), c('diag_glm', 'data.frame'))
})
