#' A short description of your function
#' 
#' @aliases mice.impute.bart
#' @inheritParams mice.impute.pmm
#' @param ntree The number of trees to grow. The default is 200.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details 
#' @note 
#' @author
#' @references 
#' @family univariate imputation functions
#' @keywords 
#' @export
#' @examples
#' \dontrun{
#' imp <- mice(nhanes2, meth = "bart", ntree = 200)
#' plot(imp)
#' }

mice.impute.bart <- function(y, ry, x, wy = NULL, ntree = 200, use.matcher = FALSE, donors = 5L, ...) {
    install.on.demand("dbarts", ...)
    if (is.null(wy)) {
        wy <- !ry
    }

    xobs <- x[ry, , drop = FALSE]
    xmis <- x[wy, , drop = FALSE]
    yobs <- y[ry]

    # Parameter estimates
   fit <- dbarts::bart(x.train = x, y.train = y, keeptrees = TRUE, verbose = FALSE) 

   yhatobs <- dbarts::fitted.bart(fit, type = "ev", sample = "train")[ry]
   yhatmis <- dbarts::fitted.bart(fit, type = "ev", sample = "train")[wy]

   # Find donors
   if (use.matcher) {
    idx <- matcher(yhatobs, yhatmis, k = donors)
  } else {
    idx <- matchindex(yhatobs, yhatmis, donors)
  }

  return(yobs[idx])
}
