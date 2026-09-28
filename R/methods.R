# Helper: the call written as "linreg(formula = ..., data = ...)"
linreg_call_string <- function(x) {
  paste0("linreg(formula = ", paste(deparse(x$formula), collapse = " "),
         ", data = ", x$data_name, ")")
}

#' Print a linreg object
#'
#' Prints the call and the estimated coefficients with their names,
#' similar to \code{print.lm}.
#'
#' @param x An object of class \code{linreg}.
#' @param ... Further arguments (not used).
#' @return \code{x}, invisibly.
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' print(mod)
#' @export
print.linreg <- function(x, ...) {
  cat("\nCall:\n", linreg_call_string(x), "\n\n", sep = "")
  cat("Coefficients:\n")
  print.default(format(x$coefficients, digits = max(3L, getOption("digits") - 3L)),
                print.gap = 2L, quote = FALSE)
  cat("\n")
  invisible(x)
}

#' Plot diagnostics for a linreg object
#'
#' Draws two diagnostic plots with \pkg{ggplot2}: "Residuals vs Fitted" and
#' "Scale-Location". The red line connects the median of the y-values for
#' each fitted value, and the three most extreme observations are labelled.
#'
#' Standardized residuals are computed as
#' \eqn{e_i / (\hat{\sigma}\sqrt{1 - h_{ii}})}, where \eqn{h_{ii}} are the
#' diagonal elements of the hat matrix.
#'
#' @param x An object of class \code{linreg}.
#' @param ... Further arguments (not used).
#' @return A list with the two \code{ggplot} objects, invisibly.
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' plot(mod)
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_hline stat_summary
#'   labs theme_bw theme element_text .data
#' @export
plot.linreg <- function(x, ...) {
  caption <- linreg_call_string(x)
  sd_res <- x$residuals / (sqrt(x$sigma2) * sqrt(1 - x$hat_values))

  df_plot <- data.frame(
    obs      = seq_along(x$residuals),
    fitted   = x$fitted_values,
    resid    = x$residuals,
    sqrt_std = sqrt(abs(sd_res))
  )

  # label the 3 observations with the largest absolute residuals
  top3 <- order(abs(df_plot$resid), decreasing = TRUE)[1:min(3, nrow(df_plot))]
  df_plot$label <- ""
  df_plot$label[top3] <- df_plot$obs[top3]

  base_theme <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, size = 11))

  p1 <- ggplot(df_plot, aes(x = .data$fitted, y = .data$resid)) +
    geom_point(shape = 1, size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
    stat_summary(fun = stats::median, geom = "line", colour = "red") +
    geom_text(aes(label = .data$label), hjust = -0.4, size = 3.5) +
    labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals",
         caption = caption) +
    base_theme

  p2 <- ggplot(df_plot, aes(x = .data$fitted, y = .data$sqrt_std)) +
    geom_point(shape = 1, size = 2.5) +
    stat_summary(fun = stats::median, geom = "line", colour = "red") +
    geom_text(aes(label = .data$label), hjust = -0.4, size = 3.5) +
    labs(title = "Scale-Location", x = "Fitted values",
         y = expression(sqrt("|Standardized residuals|")),
         caption = caption) +
    base_theme

  print(p1)
  print(p2)
  invisible(list(residuals_vs_fitted = p1, scale_location = p2))
}

#' Residuals of a linreg object
#'
#' Returns the vector of residuals \eqn{\hat{e} = y - \hat{y}}.
#' Called by both \code{resid()} and \code{residuals()}.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Further arguments (not used).
#' @return A numeric vector of residuals.
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' head(resid(mod))
#' @export
residuals.linreg <- function(object, ...) {
  object$residuals
}

#' Predicted values
#'
#' Generic function returning the predicted (fitted) values of a model.
#'
#' @param object A model object.
#' @param ... Further arguments passed to methods.
#' @return A numeric vector of predicted values.
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' Predicted values of a linreg object
#'
#' Returns the fitted values \eqn{\hat{y} = X\hat{\beta}}.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Further arguments (not used).
#' @return A numeric vector of fitted values.
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' head(pred(mod))
#' @export
pred.linreg <- function(object, ...) {
  object$fitted_values
}

#' Coefficients of a linreg object
#'
#' @param object An object of class \code{linreg}.
#' @param ... Further arguments (not used).
#' @return A named numeric vector of regression coefficients.
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' coef(mod)
#' @export
coef.linreg <- function(object, ...) {
  object$coefficients
}

#' Summary of a linreg object
#'
#' Prints a summary similar to \code{summary.lm}: the coefficients with their
#' standard errors, t-values and p-values, the residual standard error
#' \eqn{\hat{\sigma}} and the degrees of freedom.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Further arguments (not used).
#' @return The coefficient table (a matrix), invisibly.
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' summary(mod)
#' @importFrom stats printCoefmat
#' @export
summary.linreg <- function(object, ...) {
  coef_table <- cbind(
    "Estimate"   = object$coefficients,
    "Std. Error" = object$std_error,
    "t value"    = object$t_values,
    "Pr(>|t|)"   = object$p_values
  )

  cat("\nCall:\n", linreg_call_string(object), "\n\n", sep = "")
  cat("Coefficients:\n")
  printCoefmat(coef_table, signif.stars = TRUE, signif.legend = TRUE)
  cat("\nResidual standard error: ",
      format(signif(sqrt(object$sigma2), 4)),
      " on ", object$df, " degrees of freedom\n", sep = "")
  invisible(coef_table)
}
