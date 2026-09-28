data("iris")

test_that("linreg rejects erroneous input", {
  expect_error(linreg(formula = Petal.Length ~ Sepdsal.Width + Sepal.Length, data = irfsfdis))
  expect_error(linreg(formula = Petal.Length ~ Sepdsal.Width + Sepal.Length, data = iris))
  expect_error(linreg(formula = "Petal.Length ~ Sepal.Width", data = iris))
})

test_that("class is correct", {
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_s3_class(linreg_mod, "linreg")
})

test_that("print() method works", {
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_output(print(linreg_mod), "linreg\\(formula = Petal.Length ~ Sepal.Width \\+ Sepal.Length, data = iris\\)")
  expect_output(print(linreg_mod), "( )*\\(Intercept\\)( )*Sepal.Width( )*Sepal.Length")
})

test_that("pred() method works", {
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_equal(round(unname(pred(linreg_mod)[c(1, 5, 7)]), 2), c(1.85, 1.53, 1.09))
})

test_that("resid() method works", {
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_equal(round(unname(resid(linreg_mod)[c(7, 13, 27)]), 2), c(0.31, -0.58, -0.20))
})

test_that("coef() method works", {
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_true(all(round(unname(coef(linreg_mod)), 2) %in% c(-2.52, -1.34, 1.78)))
  expect_named(coef(linreg_mod), c("(Intercept)", "Sepal.Width", "Sepal.Length"))
})

test_that("summary() method works", {
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_output(summary(linreg_mod), "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*")
  expect_output(summary(linreg_mod), "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*")
  expect_output(summary(linreg_mod), "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*")
  expect_output(summary(linreg_mod), "Residual standard error: 0.6[0-9]* on 147 degrees of freedom")
})

test_that("QR results match lm()", {
  f <- Petal.Length ~ Species + Sepal.Width
  ref <- lm(f, data = iris)
  mod <- linreg(f, data = iris)
  expect_equal(coef(mod), coef(ref))
  expect_equal(unname(pred(mod)), unname(fitted(ref)))
  expect_equal(unname(mod$std_error),
               unname(summary(ref)$coefficients[, "Std. Error"]))
  expect_equal(unname(mod$p_values),
               unname(summary(ref)$coefficients[, "Pr(>|t|)"]))
})

test_that("plot() returns two ggplot objects", {
  linreg_mod <- linreg(Petal.Length ~ Species, data = iris)
  pdf(NULL)
  p <- plot(linreg_mod)
  dev.off()
  expect_length(p, 2)
  expect_s3_class(p[[1]], "ggplot")
  expect_s3_class(p[[2]], "ggplot")
})
