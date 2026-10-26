#' Ridge regression
#'
#' Fits a ridge regression model and returns an object of S3 class
#' \code{ridgereg}. All covariates are normalised before estimation,
#' \eqn{x_{norm} = (x - \bar{x}) / \sqrt{V(x)}}, and the response is centred,
#' so the intercept is not penalised (same parametrisation as
#' \code{MASS::lm.ridge}).
#'
#' The ridge coefficients are
#' \eqn{\hat{\beta}^{ridge} = (X^T X + \lambda I)^{-1} X^T y}.
#' With \code{method = "qr"} (default) they are computed with the QR
#' decomposition of the augmented matrix
#' \eqn{\tilde{X} = [X; \sqrt{\lambda} I]} and response
#' \eqn{\tilde{y} = [y; 0]}, since
#' \eqn{\tilde{X}^T \tilde{X} = X^T X + \lambda I} and
#' \eqn{\tilde{X}^T \tilde{y} = X^T y}.
#' With \code{method = "ls"} the normal equations are solved directly.
#'
#' @param formula An object of class \code{formula}.
#' @param data A \code{data.frame} with the variables in \code{formula}.
#' @param lambda A non-negative number, the penalty \eqn{\lambda}.
#' @param method Either \code{"qr"} or \code{"ls"}.
#' @return An object of class \code{ridgereg}: a list with elements
#'   \code{coefficients} (on the normalised scale, intercept first),
#'   \code{fitted_values}, \code{lambda}, \code{x_mean}, \code{x_sd},
#'   \code{formula}, \code{data_name} and \code{call}.
#' @examples
#' mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, iris, lambda = 1)
#' print(mod)
#' coef(mod)
#' head(predict(mod))
#' @importFrom stats model.matrix
#' @export
ridgereg <- function(formula, data, lambda = 0, method = c("qr", "ls")) {
  method <- match.arg(method)
  if (!inherits(formula, "formula")) {
    stop("'formula' must be an object of class 'formula'.")
  }
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda < 0) {
    stop("'lambda' must be a single non-negative number.")
  }
  vars <- all.vars(formula)
  missing_vars <- setdiff(vars[vars != "."], names(data))
  if (length(missing_vars) > 0) {
    stop("Variable(s) not found in data: ", paste(missing_vars, collapse = ", "))
  }
  data_name <- deparse(substitute(data))
  data <- as.data.frame(data)

  X <- model.matrix(formula, data)
  y <- data[rownames(X), vars[1], drop = TRUE]
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  n <- nrow(X)
  p <- ncol(X)

  # Normalise covariates (divisor n, as in MASS::lm.ridge), centre y
  x_mean <- colMeans(X)
  x_sd <- sqrt(colSums(sweep(X, 2, x_mean)^2) / n)
  if (any(x_sd == 0)) stop("A covariate is constant and cannot be normalised.")
  Xs <- scale(X, center = x_mean, scale = x_sd)
  y_mean <- mean(y)
  yc <- y - y_mean

  if (method == "ls") {
    beta <- solve(crossprod(Xs) + lambda * diag(p), crossprod(Xs, yc))
  } else {
    X_aug <- rbind(Xs, sqrt(lambda) * diag(p))
    y_aug <- c(yc, rep(0, p))
    qr_X <- qr(X_aug)
    if (qr_X$rank < p) stop("The design matrix is rank deficient; use lambda > 0.")
    beta <- numeric(p)
    beta[qr_X$pivot] <- backsolve(qr.R(qr_X), crossprod(qr.Q(qr_X), y_aug))
  }
  beta <- drop(beta)
  names(beta) <- colnames(X)

  structure(
    list(
      coefficients  = c("(Intercept)" = y_mean, beta),
      fitted_values = drop(y_mean + Xs %*% beta),
      lambda        = lambda,
      x_mean        = x_mean,
      x_sd          = x_sd,
      formula       = formula,
      data_name     = data_name,
      call          = match.call()
    ),
    class = "ridgereg"
  )
}

#' Print a ridgereg object
#'
#' Prints the call and the ridge coefficients, similar to \code{print.lm}.
#'
#' @param x An object of class \code{ridgereg}.
#' @param ... Not used.
#' @return \code{x}, invisibly.
#' @export
print.ridgereg <- function(x, ...) {
  cat("\nCall:\n",
      "ridgereg(formula = ", paste(deparse(x$formula), collapse = " "),
      ", data = ", x$data_name, ", lambda = ", x$lambda, ")\n\n", sep = "")
  cat("Coefficients:\n")
  print.default(format(x$coefficients, digits = max(3L, getOption("digits") - 3L)),
                print.gap = 2L, quote = FALSE)
  cat("\n")
  invisible(x)
}

#' Coefficients of a ridgereg object
#'
#' @param object An object of class \code{ridgereg}.
#' @param ... Not used.
#' @return Named numeric vector \eqn{\hat{\beta}^{ridge}} (normalised scale).
#' @export
coef.ridgereg <- function(object, ...) {
  object$coefficients
}

#' Predict with a ridgereg object
#'
#' Returns fitted values, or predictions for \code{newdata}. New data are
#' normalised with the means and standard deviations of the training data.
#'
#' @param object An object of class \code{ridgereg}.
#' @param newdata Optional \code{data.frame} with the same covariates.
#' @param ... Not used.
#' @return Numeric vector of predicted values.
#' @importFrom stats predict delete.response terms model.matrix
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted_values)
  newdata <- as.data.frame(newdata)
  tt <- delete.response(terms(object$formula, data = newdata))
  X_new <- model.matrix(tt, newdata)
  X_new <- X_new[, names(object$x_mean), drop = FALSE]
  Xs_new <- scale(X_new, center = object$x_mean, scale = object$x_sd)
  b <- object$coefficients
  drop(b[1] + Xs_new %*% b[-1])
}
