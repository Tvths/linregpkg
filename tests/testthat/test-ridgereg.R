data("iris")

test_that("ridgereg rejects erroneous input", {
  expect_error(ridgereg(Petal.Length ~ Sepdsal.Width, data = iris, lambda = 1))
  expect_error(ridgereg("Petal.Length ~ Sepal.Width", data = iris, lambda = 1))
  expect_error(ridgereg(Petal.Length ~ Sepal.Width, data = iris, lambda = -1))
})

test_that("ridgereg returns an object of class ridgereg", {
  mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  expect_s3_class(mod, "ridgereg")
})

test_that("coefficients are similar to MASS::lm.ridge", {
  skip_if_not_installed("MASS")
  for (l in c(0, 0.5, 10, 100)) {
    mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length + Petal.Width,
                    data = iris, lambda = l)
    mass <- MASS::lm.ridge(Petal.Length ~ Sepal.Width + Sepal.Length + Petal.Width,
                           data = iris, lambda = l)
    expect_equal(unname(coef(mod)[-1]), unname(mass$coef), tolerance = 1e-6)
    expect_equal(unname(coef(mod)[1]), mass$ym)
  }
})

test_that("QR and least squares give the same result", {
  qr_mod <- ridgereg(Petal.Length ~ ., data = iris, lambda = 3, method = "qr")
  ls_mod <- ridgereg(Petal.Length ~ ., data = iris, lambda = 3, method = "ls")
  expect_equal(coef(qr_mod), coef(ls_mod))
})

test_that("lambda = 0 gives the same fitted values as lm", {
  mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 0)
  lm_mod <- lm(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_equal(unname(predict(mod)), unname(fitted(lm_mod)))
})

test_that("predict() works on new data", {
  mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 2)
  expect_equal(predict(mod, newdata = iris), predict(mod), ignore_attr = TRUE)
  expect_length(predict(mod, newdata = iris[1:5, c("Sepal.Width", "Sepal.Length")]), 5)
})

test_that("print() method works", {
  mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  expect_output(print(mod), "ridgereg\\(formula = Petal.Length ~ Sepal.Width \\+ Sepal.Length")
  expect_output(print(mod), "\\(Intercept\\)( )*Sepal.Width( )*Sepal.Length")
})

test_that("visualize_airport_delays returns a ggplot", {
  skip_if_not_installed("nycflights13")
  expect_s3_class(visualize_airport_delays(), "ggplot")
})
