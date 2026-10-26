#' ridgereg as a custom model for caret
#'
#' Returns a list that can be given as \code{method} to
#' \code{caret::train()} so that \eqn{\lambda} of \code{\link{ridgereg}} can
#' be tuned with resampling (e.g. 10-fold cross-validation).
#'
#' @return A list describing the model in the format caret expects.
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   fit <- caret::train(Petal.Length ~ ., data = iris[, 1:4],
#'                       method = ridgereg_caret(),
#'                       tuneGrid = data.frame(lambda = c(0, 1, 10)),
#'                       trControl = caret::trainControl(method = "cv", number = 5))
#'   fit$bestTune
#' }
#' }
#' @export
ridgereg_caret <- function() {
  list(
    label = "Ridge regression (ridgereg)",
    library = NULL,
    type = "Regression",
    loop = NULL,
    parameters = data.frame(parameter = "lambda", class = "numeric",
                            label = "Lambda"),
    grid = function(x, y, len = NULL, search = "grid") {
      data.frame(lambda = seq(0, 50, length.out = len))
    },
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      dat <- as.data.frame(x)
      dat$.outcome <- y
      ridgereg(.outcome ~ ., data = dat, lambda = param$lambda)
    },
    predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
      predict(modelFit, newdata = as.data.frame(newdata))
    },
    prob = NULL,
    sort = function(x) x[order(-x$lambda), ]
  )
}
