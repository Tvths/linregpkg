#' Multiple linear regression
#'
#' Fits a multiple linear regression model and returns an object of S3 class
#' \code{linreg}. The design matrix \eqn{X} is created with
#' \code{\link[stats]{model.matrix}} and the dependent variable \eqn{y} is
#' picked out with \code{\link{all.vars}}.
#'
#' Two estimation methods are available:
#' \describe{
#'   \item{\code{"qr"} (default)}{With \eqn{X = QR},
#'     \eqn{\hat{\beta} = R^{-1} Q^T y} and
#'     \eqn{\widehat{Var}(\hat{\beta}) = \hat{\sigma}^2 (R^T R)^{-1}},
#'     since \eqn{X^T X = R^T Q^T Q R = R^T R}.}
#'   \item{\code{"ols"}}{Ordinary linear algebra:
#'     \eqn{\hat{\beta} = (X^T X)^{-1} X^T y} and
#'     \eqn{\widehat{Var}(\hat{\beta}) = \hat{\sigma}^2 (X^T X)^{-1}}.}
#' }
#'
#' From the estimates the function computes fitted values
#' \eqn{\hat{y} = X\hat{\beta}}, residuals \eqn{\hat{e} = y - \hat{y}},
#' degrees of freedom \eqn{df = n - p}, residual variance
#' \eqn{\hat{\sigma}^2 = e^T e / df}, t-values
#' \eqn{t = \hat{\beta} / \sqrt{\widehat{Var}(\hat{\beta})}} and p-values
#' using \code{\link[stats]{pt}}.
#'
#' @param formula An object of class \code{formula}, e.g. \code{y ~ x1 + x2}.
#' @param data A \code{data.frame} containing the variables in \code{formula}.
#' @param method Estimation method, either \code{"qr"} (default) or \code{"ols"}.
#'
#' @return An object of class \code{linreg}: a list with elements
#'   \code{coefficients}, \code{fitted_values}, \code{residuals}, \code{df},
#'   \code{sigma2}, \code{var_beta}, \code{std_error}, \code{t_values},
#'   \code{p_values}, \code{hat_values}, \code{formula}, \code{data_name},
#'   \code{method} and \code{call}.
#'
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' print(mod)
#' summary(mod)
#' coef(mod)
#'
#' @seealso \code{\link{print.linreg}}, \code{\link{plot.linreg}},
#'   \code{\link{residuals.linreg}}, \code{\link{pred}},
#'   \code{\link{coef.linreg}}, \code{\link{summary.linreg}}
#'
#' @importFrom stats model.matrix pt
#' @export
linreg <- function(formula, data, method = c("qr", "ols")) {
  # ---- Input checks -------------------------------------------------------
  if (!inherits(formula, "formula")) {
    stop("'formula' must be an object of class 'formula'.")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  method <- match.arg(method)
  vars <- all.vars(formula)
  missing_vars <- setdiff(vars[vars != "."], names(data))
  if (length(missing_vars) > 0) {
    stop("Variable(s) not found in data: ", paste(missing_vars, collapse = ", "))
  }

  data_name <- deparse(substitute(data))

  # ---- Design matrix and response -----------------------------------------
  data <- as.data.frame(data)
  X <- model.matrix(formula, data)
  # model.matrix drops rows with NA; pick y for the same rows
  y <- data[rownames(X), vars[1], drop = TRUE]

  n <- nrow(X)
  p <- ncol(X)
  df <- n - p
  if (df <= 0) stop("Not enough observations: n must be larger than p.")

  # ---- Estimation ---------------------------------------------------------
  if (method == "qr") {
    qr_X <- qr(X)
    if (qr_X$rank < p) stop("The design matrix X is rank deficient.")
    Q <- qr.Q(qr_X)
    R <- qr.R(qr_X)
    piv <- qr_X$pivot

    beta <- numeric(p)
    beta[piv] <- backsolve(R, crossprod(Q, y))   # R beta = Q^T y
    R_inv <- backsolve(R, diag(p))               # R^{-1}
    XtX_inv <- matrix(0, p, p)
    XtX_inv[piv, piv] <- R_inv %*% t(R_inv)      # (R^T R)^{-1}
    hat_values <- rowSums(Q^2)                   # diag(Q Q^T)
  } else {
    XtX_inv <- solve(t(X) %*% X)
    beta <- as.vector(XtX_inv %*% t(X) %*% y)
    hat_values <- rowSums((X %*% XtX_inv) * X)   # diag(X (X^T X)^{-1} X^T)
  }
  names(beta) <- colnames(X)

  # ---- Statistics ---------------------------------------------------------
  fitted_values <- as.vector(X %*% beta)
  residuals <- as.vector(y - fitted_values)
  sigma2 <- as.numeric(crossprod(residuals) / df)
  var_beta <- sigma2 * XtX_inv
  dimnames(var_beta) <- list(colnames(X), colnames(X))
  std_error <- sqrt(diag(var_beta))
  t_values <- beta / std_error
  p_values <- 2 * pt(abs(t_values), df = df, lower.tail = FALSE)

  structure(
    list(
      coefficients  = beta,
      fitted_values = fitted_values,
      residuals     = residuals,
      df            = df,
      sigma2        = sigma2,
      var_beta      = var_beta,
      std_error     = std_error,
      t_values      = t_values,
      p_values      = p_values,
      hat_values    = hat_values,
      formula       = formula,
      data_name     = data_name,
      method        = method,
      call          = match.call()
    ),
    class = "linreg"
  )
}
