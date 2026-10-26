# linregpkg

<!-- badges: start -->
[![R CMD check](https://github.com/Tvths/linregpkg/actions/workflows/r.yml/badge.svg)](https://github.com/Tvths/linregpkg/actions/workflows/r.yml)
<!-- badges: end -->

`linregpkg` is an R package for multiple linear regression and ridge regression.
`linreg()` estimates linear models with a QR decomposition. `ridgereg()` fits
ridge models with standardized predictors and an unpenalized intercept; its
default implementation uses an augmented QR decomposition. The package also
provides S3 methods, a `caret` tuning interface, and an airport-delay
visualization.

The package was developed for Computer Lab 4 and the Bonus Computer Lab in
**732A94 Advanced R Programming** at Linköping University.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("Tvths/linregpkg", build_vignettes = TRUE)
```

## Linear regression

Fit a model with `linreg()` using a formula and a data frame:

```r
library(linregpkg)

mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
summary(mod)
coef(mod)
head(resid(mod))
head(pred(mod))
plot(mod)
```

`linreg` objects support `print()`, `summary()`, `coef()`, `resid()`, `pred()`
and `plot()`.

## Ridge regression

`ridgereg()` takes a formula, data frame, and penalty `lambda`. Predictors are
standardized before fitting and the intercept is not penalized. The default
`method = "qr"` uses an augmented QR decomposition; `method = "ls"` is also
available.

```r
ridge_mod <- ridgereg(
  Petal.Length ~ Sepal.Width + Sepal.Length,
  data = iris,
  lambda = 1
)

print(ridge_mod)
coef(ridge_mod)
predict(ridge_mod)
predict(ridge_mod, newdata = iris[1:5, ])
```

`ridgereg` objects support `print()`, `coef()` and `predict()`. `predict()`
returns fitted values by default and can predict on a new data frame.

To tune `lambda` with 10-fold cross-validation using `caret`:

```r
cv_fit <- caret::train(
  Petal.Length ~ Sepal.Width + Sepal.Length,
  data = iris,
  method = ridgereg_caret(),
  tuneGrid = data.frame(lambda = c(0, 1, 10)),
  trControl = caret::trainControl(method = "cv", number = 10)
)
cv_fit$bestTune
```

## Airport-delay visualization

`visualize_airport_delays()` uses `nycflights13` data to plot destination
airports by longitude and latitude. Point colour shows mean arrival delay and
point size shows the number of flights.

```r
delay_plot <- visualize_airport_delays()
print(delay_plot)
```

## Vignettes and tests

The package includes an introduction to `linreg()` and a predictive-modelling
vignette for `ridgereg()`. The latter uses the BostonHousing data to compare
linear regression, forward selection, and ridge regression, tuning `lambda`
with 10-fold cross-validation and evaluating the models on a held-out test set.

Open the installed vignettes with:

```r
browseVignettes("linregpkg")
```

The test suite compares ridge coefficients with `MASS::lm.ridge()` and covers
prediction, input validation, model methods, and the airport-delay plot.

## Authors

- Viet Tien Trinh — LiU ID: `vietr933` — GitHub: [@Tvths](https://github.com/Tvths)
- Zhengyu Wang — LiU ID: `zhewa470` — GitHub: [@wwwzyccc777](https://github.com/wwwzyccc777)

## License

MIT
